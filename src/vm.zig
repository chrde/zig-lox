const std = @import("std");
const ArrayList = std.ArrayList;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const value = @import("value.zig");
const debug = @import("debug.zig");

const StackMax = 256;

pub const InterpreterError = error{
    Compile,
    Runtime,
};

const BinaryOp = enum { add, sub, mul, div };

pub const Vm = struct {
    chunk: Chunk,
    ip: usize,
    stack: ArrayList(value.Value),

    pub fn init(allocator: *std.mem.Allocator, chunk: Chunk) !Vm {
        return Vm{
            .chunk = chunk,
            .ip = 0,
            .stack = try ArrayList(value.Value).initCapacity(allocator, StackMax),
        };
    }

    fn debug_stack(self: Vm) void {
        std.debug.print("          ", .{});
        for (self.stack.items) |x| {
            std.debug.print("[ ", .{});
            value.printValue(x);
            std.debug.print(" ]", .{});
        }
        std.debug.print("\n", .{});
    }

    fn read_byte(self: *Vm) usize {
        const b = self.chunk.code.items[self.ip];
        self.ip += 1;
        return b;
    }

    fn read_constant(self: *Vm) value.Value {
        const b = self.read_byte();
        return self.chunk.constants.items[b];
    }

    fn binary_op(self: *Vm, op: BinaryOp) void {
        const left = self.stack.pop();
        const right = self.stack.pop();
        const result = switch (op) {
            .add => left + right,
            .sub => left - right,
            .mul => left * right,
            .div => left / right,
        };
        self.stack.appendAssumeCapacity(result);
    }

    pub fn interpret(self: *Vm) InterpreterError!void {
        while (true) {
            _ = debug.disassembleInstruction(self.chunk, self.ip);
            self.debug_stack();
            switch (@intToEnum(OpCode, self.read_byte())) {
                OpCode.Return => {
                    std.debug.print("\nresult: ", .{});
                    value.printValue(self.stack.pop());
                    std.debug.print("\n", .{});
                    break;
                },
                OpCode.Constant => {
                    const constant = self.read_constant();
                    self.stack.appendAssumeCapacity(constant);
                    // std.debug.print("\n", .{});
                },
                OpCode.Negate => {
                    self.stack.appendAssumeCapacity(-self.stack.pop());
                },
                OpCode.Add => self.binary_op(.add),
                OpCode.Substract => self.binary_op(.sub),
                OpCode.Multiply => self.binary_op(.mul),
                OpCode.Divide => self.binary_op(.div),
            }
        }
        return;
    }
};
