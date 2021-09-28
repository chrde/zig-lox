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
        OpCode.@"return" => simpleInstruction("RETURN", offset),
        OpCode.constant => constantInstruction("CONSTANT", c, offset),
        OpCode.negate => simpleInstruction("NEGATE", offset),
        OpCode.add => simpleInstruction("ADD", offset),
        OpCode.substract => simpleInstruction("SUBSTRACT", offset),
        OpCode.multiply => simpleInstruction("MULTIPLY", offset),
        OpCode.divide => simpleInstruction("DIVIDE", offset),
        OpCode.@"true" => simpleInstruction("TRUE", offset),
        OpCode.@"false" => simpleInstruction("FALSE", offset),
        OpCode.nil => simpleInstruction("NIL", offset),
        OpCode.not => simpleInstruction("NOT", offset),
        OpCode.equal => simpleInstruction("EQUAL", offset),
        OpCode.greater => simpleInstruction("GREATER", offset),
        OpCode.less => simpleInstruction("LESS", offset),
        OpCode.print => simpleInstruction("PRINT", offset),
        OpCode.pop => simpleInstruction("POP", offset),
        OpCode.define_global => constantInstruction("DEFINE_GLOBAL", c, offset),
        OpCode.get_global => constantInstruction("GET_GLOBAL", c, offset),
        OpCode.set_global => constantInstruction("SET_GLOBAL", c, offset),
        OpCode.get_local => byteInstruction("GET_LOCAL", c, offset),
        OpCode.set_local => byteInstruction("SET_LOCAL", c, offset),
        OpCode.jump_if_false => jumpInstruction("JUMP_IF_FALSE", 1, c, offset),
        OpCode.jump => jumpInstruction("JUMP", 1, c, offset),
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

fn byteInstruction(name: []const u8, c: Chunk, offset: usize) usize {
    const slot = c.code.items[offset + 1];
    std.debug.print("{s:<12} {d:>4} \n", .{ name, slot });
    return offset + 2;
}

fn jumpInstruction(name: []const u8, sign: i8, c: Chunk, offset: usize) usize {
    const jump_target = offset + 3 + std.mem.readIntSlice(u16, c.code.items[offset + 1 ..], .Little);
    std.debug.print("{s:<12} {d:>4} -> {d} \n", .{ name, offset, jump_target });
    return offset + 3;
}
