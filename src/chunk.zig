const std = @import("std");
const Value = @import("value.zig").Value;
const ArrayList = std.ArrayList;

pub const OpCode = enum(usize) {
    Return,
    Constant,
};

pub const Chunk = struct {
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

    pub fn write(self: *@This(), item: usize, line: usize) !void {
        try self.code.append(item);
        try self.lines.append(line);
    }

    pub fn addConstant(self: *@This(), v: Value) !usize {
        try self.constants.append(v);
        return self.constants.items.len - 1;
    }
};
