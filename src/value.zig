const std = @import("std");
const expect = @import("std").testing.expect;
const Obj = @import("object.zig").Obj;

pub const ValueTag = enum {
    bool,
    nil,
    number,
    obj,
};

pub const Value = union(ValueTag) {
    const Self = @This();
    bool: bool,
    nil,
    number: f64,
    obj: *Obj,

    pub fn debug(self: Self) void {
        switch (self) {
            .bool => |b| std.debug.print("{s}", .{b}),
            .nil => |_| std.debug.print("<nil>", .{}),
            .number => |n| std.debug.print("{d}", .{n}),
            .obj => |o| o.debug(),
        }
    }

    pub fn new(comptime T: type, a: T) Self {
        if (T == bool) {
            return Self{ .bool = a };
        } else if (T == f64) {
            return Self{ .number = a };
        } else {
            unreachable;
        }
    }

    pub fn isString(self: Self) bool {
        return switch (self) {
            .obj => |obj| obj.is(Obj.Type.string),
            else => false,
        };
    }

    pub fn isNumber(self: Self) bool {
        switch (self) {
            .number => |_| return true,
            else => return false,
        }
    }

    pub fn isFalsey(self: Self) bool {
        return switch (self) {
            .bool => |b| !b,
            .nil => |_| true,
            else => false,
        };
    }

    pub fn isEqual(self: Self, other: Self) bool {
        if (@as(ValueTag, self) != @as(ValueTag, other)) {
            return false;
        }

        switch (self) {
            .obj => {
                if (self.obj.ty != other.obj.ty) {
                    return false;
                } else {
                    switch (self.obj.ty) {
                        .string => {
                            return std.mem.eql(
                                u8,
                                self.obj.asString().bytes,
                                other.obj.asString().bytes,
                            );
                        },
                    }
                }
            },
            else => return std.meta.eql(self, other),
        }
    }
};

test "isFalsey" {
    try expect(Value.isFalsey(Value{ .nil = {} }));
    try expect(Value.isFalsey(Value{ .bool = false }));
    try expect(!Value.isFalsey(Value{ .bool = true }));
    try expect(!Value.isFalsey(Value{ .number = 0 }));
    try expect(!Value.isFalsey(Value{ .number = 3 }));

    const str = Obj.String.copy(std.testing.allocator, "1") catch unreachable;
    defer str.deinit(std.testing.allocator);
    const str_val = .{ .obj = &str.obj };
    try expect(!Value.isFalsey(str_val));
}

test "isEqual" {
    const str = Obj.String.copy(std.testing.allocator, "1") catch unreachable;
    defer str.deinit(std.testing.allocator);
    const str_val = .{ .obj = &str.obj };
    const vals = [_]Value{
        .{ .bool = true },
        .{ .bool = false },
        .{ .nil = {} },
        .{ .number = 1 },
        str_val,
    };
    for (vals) |v1, idx1| {
        for (vals) |v2, idx2| {
            if (idx1 != idx2) {
                try expect(!Value.isEqual(v1, v2));
            } else {
                try expect(Value.isEqual(v1, v2));
            }
        }
    }

    const str1 = Obj.String.copy(std.testing.allocator, "1") catch unreachable;
    defer str1.deinit(std.testing.allocator);
    const str_val1 = .{ .obj = &str1.obj };

    const str2 = Obj.String.copy(std.testing.allocator, "2") catch unreachable;
    defer str2.deinit(std.testing.allocator);
    const str_val2 = .{ .obj = &str2.obj };

    try expect(!Value.isEqual(str_val, str_val2));
}
