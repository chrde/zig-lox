const std = @import("std");
const ArrayList = std.ArrayList;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const debug = @import("debug.zig");
const Compiler = @import("compiler.zig").Compiler;

const stack_max = 256;

pub const InterpreterError = error{
    Compile,
    Runtime,
};

const BinaryOp = enum { add, sub, mul, div, greater, less };

pub const Vm = struct {
    const Self = @This();
    chunk: Chunk = undefined,
    ip: usize = undefined,
    allocator: *std.mem.Allocator,
    stack: ArrayList(Value),

    pub fn init(allocator: *std.mem.Allocator) !Self {
        return Self{
            .allocator = allocator,
            .stack = try ArrayList(Value).initCapacity(allocator, stack_max),
        };
    }

    pub fn peekStack(self: Self, depth: usize) Value {
        return self.stack.items[self.stack.items.len - 1 - depth];
    }

    pub fn deinit(self: Self) void {
        self.stack.deinit();
    }

    fn debugStack(self: Self) void {
        std.debug.print("          ", .{});
        for (self.stack.items) |x| {
            std.debug.print("[ ", .{});
            x.debug();
            std.debug.print(" ]", .{});
        }
        std.debug.print("\n", .{});
    }

    fn readByte(self: *Self) usize {
        const b = self.chunk.code.items[self.ip];
        self.ip += 1;
        return b;
    }

    fn readConstant(self: *Self) Value {
        const b = self.readByte();
        return self.chunk.constants.items[b];
    }

    fn binaryOp(self: *Self, op: BinaryOp) error{Runtime}!void {
        if (!(self.peekStack(0).isNumber() and self.peekStack(1).isNumber())) {
            return self.runtimeError("Operands must be numbers.");
        }

        const right = self.stack.pop().number;
        const left = self.stack.pop().number;
        const result = switch (op) {
            .add => Value.new(f64, left + right),
            .sub => Value.new(f64, left - right),
            .mul => Value.new(f64, left * right),
            .div => Value.new(f64, left / right),
            .greater => Value.new(bool, left > right),
            .less => Value.new(bool, left < right),
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
            self.debugStack();
            switch (@intToEnum(OpCode, self.readByte())) {
                OpCode.@"return" => {
                    std.debug.print("\nresult: ", .{});
                    self.stack.pop().debug();
                    std.debug.print("\n", .{});
                    break;
                },
                OpCode.constant => {
                    const constant = self.readConstant();
                    self.stack.appendAssumeCapacity(constant);
                },
                OpCode.negate => {
                    if (self.peekStack(0).isNumber()) {
                        const n = self.stack.pop().number;
                        self.stack.appendAssumeCapacity(Value{ .number = -n });
                    } else {
                        self.runtimeError("Operand must be a number.");
                        return error.Runtime;
                    }
                },
                OpCode.add => try self.binaryOp(.add),
                OpCode.substract => try self.binaryOp(.sub),
                OpCode.multiply => try self.binaryOp(.mul),
                OpCode.divide => try self.binaryOp(.div),
                OpCode.greater => try self.binaryOp(.greater),
                OpCode.less => try self.binaryOp(.less),
                OpCode.@"true" => self.stack.appendAssumeCapacity(Value{ .bool = true }),
                OpCode.@"false" => self.stack.appendAssumeCapacity(Value{ .bool = false }),
                OpCode.nil => self.stack.appendAssumeCapacity(Value{ .nil = {} }),
                OpCode.not => self.stack.appendAssumeCapacity(Value{ .bool = self.stack.pop().isFalsey() }),
                OpCode.equal => {
                    const right = self.stack.pop();
                    const left = self.stack.pop();
                    const val = Value{ .bool = left.isEqual(right) };
                    self.stack.appendAssumeCapacity(val);
                },
            }
        }
        return;
    }

    fn runtimeError(self: *Self, msg: []const u8) void {
        const b = self.chunk.code.items[self.ip];
        const line = self.chunk.lines.items[b];
        std.debug.print("{s}\n[line {d}] in script\n", .{ msg, line });
    }
};
