const std = @import("std");
const Value = @import("value.zig").Value;
const ArrayList = std.ArrayList;

pub const OpCode = enum(usize) {
    add,
    constant,
    divide,
    multiply,
    negate,
    @"return",
    substract,
};

pub const Chunk = struct {
    const Self = @This();
    code: ArrayList(usize),
    constants: ArrayList(Value),
    lines: ArrayList(usize),

    pub fn init(allocator: *std.mem.Allocator) Chunk {
        return Chunk{
            .code = ArrayList(usize).init(allocator),
            .constants = ArrayList(Value).init(allocator),
            .lines = ArrayList(usize).init(allocator),
        };
    }

    pub fn deinit(self: Self) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    pub fn write(self: *Self, item: usize, line: usize) void {
        self.code.append(item) catch unreachable;
        self.lines.append(line) catch unreachable;
    }

    pub fn addConstant(self: *Self, v: Value) !usize {
        try self.constants.append(v);
        return self.constants.items.len - 1;
    }
};
