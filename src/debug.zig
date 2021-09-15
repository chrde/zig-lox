const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const value = @import("value.zig");

pub fn disassembleChunk(c: Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});
    var index: usize = 0;
    while (index < c.code.items.len) {
        index = disassembleInstruction(c, index);
    }
}

fn disassembleInstruction(c: Chunk, offset: usize) usize {
    std.debug.print("{d:0>4} ", .{offset});

    if (offset > 0 and (c.lines.items[offset] == c.lines.items[offset - 1])) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d:4} ", .{c.lines.items[offset]});
    }

    var op = c.code.items[offset];
    switch (@intToEnum(OpCode, op)) {
        OpCode.Return => {
            return simpleInstruction("OP_RETURN", offset);
        },
        OpCode.Constant => {
            return constantInstruction("OP_CONSTANT", c, offset);
        },
    }
}

fn simpleInstruction(comptime name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(name: []const u8, c: Chunk, offset: usize) usize {
    const constant_idx = c.code.items[offset + 1];
    const constant = c.constants.items[constant_idx];
    std.debug.print("{s:<12} {d:>4} ", .{ name, constant_idx });
    value.printValue(constant);
    std.debug.print("\n", .{});
    return offset + 2;
}
