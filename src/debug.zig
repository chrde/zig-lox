const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;

pub fn disassembleChunk(c: Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});
    var index: usize = 0;
    while (index < c.code.items.len) {
        index = disassembleInstruction(c, index);
    }
}

pub fn disassembleInstruction(c: Chunk, offset: usize) usize {
    std.debug.print("{d:0>4} ", .{offset});

    if (offset > 0 and (c.lines.items[offset] == c.lines.items[offset - 1])) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d:4} ", .{c.lines.items[offset]});
    }

    var op = c.code.items[offset];
    const result = switch (@intToEnum(OpCode, op)) {
        OpCode.@"return" => simpleInstruction("OP_RETURN", offset),
        OpCode.constant => constantInstruction("OP_CONSTANT", c, offset),
        OpCode.negate => simpleInstruction("OP_NEGATE", offset),
        OpCode.add => simpleInstruction("OP_ADD", offset),
        OpCode.substract => simpleInstruction("OP_SUBSTRACT", offset),
        OpCode.multiply => simpleInstruction("OP_MULTIPLE", offset),
        OpCode.divide => simpleInstruction("OP_DIVIDE", offset),
        OpCode.@"true" => simpleInstruction("OP_TRUE", offset),
        OpCode.@"false" => simpleInstruction("OP_FALSE", offset),
        OpCode.nil => simpleInstruction("OP_NIL", offset),
        OpCode.not => simpleInstruction("OP_NOT", offset),
        OpCode.equal => simpleInstruction("OP_EQUAL", offset),
        OpCode.greater => simpleInstruction("OP_GREATER", offset),
        OpCode.less => simpleInstruction("OP_LESS", offset),
    };
    std.debug.print("\n", .{});
    return result;
}

fn simpleInstruction(comptime name: []const u8, offset: usize) usize {
    std.debug.print("{s}", .{name});
    return offset + 1;
}

fn constantInstruction(name: []const u8, c: Chunk, offset: usize) usize {
    const constant_idx = c.code.items[offset + 1];
    var constant = c.constants.items[constant_idx];
    std.debug.print("{s:<12} {d:>4} ", .{ name, constant_idx });
    constant.debug();
    return offset + 2;
}
