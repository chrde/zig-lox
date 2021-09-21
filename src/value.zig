const std = @import("std");
const expect = @import("std").testing.expect;

pub const Value = union(enum) {
    const Self = @This();
    bool: bool,
    nil,
    number: f64,

    pub fn debug(self: Self) void {
        switch (self) {
            .bool => |b| std.debug.print("{s}", .{b}),
            .nil => |_| std.debug.print("<nil>", .{}),
            .number => |n| std.debug.print("{d}", .{n}),
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
            .number => |_| false,
        };
    }

    pub fn isEqual(self: Self, other: Self) bool {
        return std.meta.eql(self, other);
    }
};

test "isFalsey" {
    try expect(Value.isFalsey(Value{ .nil = {} }));
    try expect(Value.isFalsey(Value{ .bool = false }));
    try expect(!Value.isFalsey(Value{ .bool = true }));
    try expect(!Value.isFalsey(Value{ .number = 0 }));
    try expect(!Value.isFalsey(Value{ .number = 3 }));
}

test "isEqual" {
    const vals = [_]Value{
        .{ .bool = true },
        .{ .bool = false },
        .{ .nil = {} },
        .{ .number = 1 },
        .{ .number = 2 },
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
}
