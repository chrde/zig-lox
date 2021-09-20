const std = @import("std");
const ArrayList = std.ArrayList;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const value = @import("value.zig");
const debug = @import("debug.zig");
const Compiler = @import("compiler.zig").Compiler;

const stack_max = 256;

pub const InterpreterError = error{
    Compile,
    Runtime,
};

const BinaryOp = enum { add, sub, mul, div };

pub const Vm = struct {
    const Self = @This();
    chunk: Chunk = undefined,
    ip: usize = undefined,
    allocator: *std.mem.Allocator,
    stack: ArrayList(value.Value),

    pub fn init(allocator: *std.mem.Allocator) !Self {
        return Self{
            .allocator = allocator,
            .stack = try ArrayList(value.Value).initCapacity(allocator, stack_max),
        };
    }

    pub fn deinit(self: Self) void {
        self.stack.deinit();
    }

    fn debug_stack(self: Self) void {
        std.debug.print("          ", .{});
        for (self.stack.items) |x| {
            std.debug.print("[ ", .{});
            value.printValue(x);
            std.debug.print(" ]", .{});
        }
        std.debug.print("\n", .{});
    }

    fn read_byte(self: *Self) usize {
        const b = self.chunk.code.items[self.ip];
        self.ip += 1;
        return b;
    }

    fn read_constant(self: *Self) value.Value {
        const b = self.read_byte();
        return self.chunk.constants.items[b];
    }

    fn binary_op(self: *Self, op: BinaryOp) void {
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

    pub fn interpret(self: *Self, source: []const u8) InterpreterError!void {
        var chunk = Chunk.init(self.allocator);
        defer chunk.deinit();

        var compiler = Compiler.init(source, &chunk);
        try compiler.compile();

        self.chunk = chunk;
        self.ip = 0;
        return self.run();
    }

    pub fn run(self: *Self) error{Runtime}!void {
        while (true) {
            _ = debug.disassembleInstruction(self.chunk, self.ip);
            self.debug_stack();
            switch (@intToEnum(OpCode, self.read_byte())) {
                OpCode.@"return" => {
                    std.debug.print("\nresult: ", .{});
                    value.printValue(self.stack.pop());
                    std.debug.print("\n", .{});
                    break;
                },
                OpCode.constant => {
                    const constant = self.read_constant();
                    self.stack.appendAssumeCapacity(constant);
                    // std.debug.print("\n", .{});
                },
                OpCode.negate => {
                    self.stack.appendAssumeCapacity(-self.stack.pop());
                },
                OpCode.add => self.binary_op(.add),
                OpCode.substract => self.binary_op(.sub),
                OpCode.multiply => self.binary_op(.mul),
                OpCode.divide => self.binary_op(.div),
            }
        }
        return;
    }
};
