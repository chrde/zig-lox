const std = @import("std");
const ArrayList = std.ArrayList;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const Obj = @import("object.zig").Obj;
const debug = @import("debug.zig");
const Compiler = @import("compiler.zig").Compiler;

const stack_max = 255;

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
    objects: ?*Obj = null,
    globals: std.StringHashMap(Value),
    strings: std.StringHashMap(*Obj.String),
    stack: ArrayList(Value),

    pub fn init(allocator: *std.mem.Allocator) !Self {
        return Self{
            .allocator = allocator,
            .strings = std.StringHashMap(*Obj.String).init(allocator),
            .globals = std.StringHashMap(Value).init(allocator),
            .stack = try ArrayList(Value).initCapacity(allocator, stack_max),
        };
    }

    pub fn peekStack(self: Self, depth: usize) Value {
        return self.stack.items[self.stack.items.len - 1 - depth];
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit();
        self.strings.deinit();
        self.destroyObjects();
        self.globals.deinit();
    }

    fn destroyObjects(self: *Self) void {
        var cur = self.objects;
        while (cur) |o| {
            cur = o.next;
            o.destroy(self);
        }
    }

    fn debugStack(self: *Self) void {
        std.debug.print("          ", .{});
        for (self.stack.items) |*x| {
            std.debug.print("[ ", .{});
            x.debug();
            std.debug.print(" ]", .{});
        }
        std.debug.print("\n", .{});
    }

    fn readShort(self: *Self) u16 {
        const s = std.mem.readIntSlice(u16, self.chunk.code.items[self.ip..], .Little);
        self.ip += 2;
        return s;
    }

    fn readByte(self: *Self) u8 {
        const b = self.chunk.code.items[self.ip];
        self.ip += 1;
        return b;
    }

    fn readConstant(self: *Self) Value {
        const b = self.readByte();
        return self.chunk.constants.items[b];
    }

    fn readString(self: *Self) *Obj.String {
        const b = self.readByte();
        const v = self.chunk.constants.items[b];
        return v.obj.asString();
    }

    fn binaryOp(self: *Self, op: BinaryOp) error{Runtime}!void {
        if (!(self.peekStack(0).isNumber() and self.peekStack(1).isNumber())) {
            return self.runtimeError("Operands must be numbers.", .{});
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

        var compiler = Compiler.init(self, source, &chunk) catch unreachable;
        defer compiler.deinit();
        try compiler.compile();

        self.chunk = chunk;
        self.ip = 0;
        return self.run();
    }

    fn concatenate(self: *Self) !void {
        const right = self.stack.pop().obj.asString();
        const left = self.stack.pop().obj.asString();
        const new_bytes = try std.mem.concat(self.allocator, u8, &[_][]const u8{ left.bytes, right.bytes });
        const new_str = try Obj.String.takeString(self, new_bytes);
        self.stack.appendAssumeCapacity(Value{ .obj = &new_str.obj });
    }

    pub fn run(self: *Self) error{Runtime}!void {
        while (true) {
            self.debugStack();
            _ = debug.disassembleInstruction(self.chunk, self.ip);
            switch (@intToEnum(OpCode, self.readByte())) {
                OpCode.@"return" => {
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
                        return self.runtimeError("Operand must be a number.", .{});
                    }
                },
                OpCode.add => {
                    if (self.peekStack(0).isString() and self.peekStack(1).isString()) {
                        self.concatenate() catch unreachable;
                    } else {
                        try self.binaryOp(.add);
                    }
                },
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
                OpCode.print => {
                    self.stack.pop().debug();
                    std.debug.print("\n", .{});
                },
                OpCode.pop => _ = self.stack.pop(),
                OpCode.define_global => {
                    const str = self.readString();
                    self.globals.put(str.bytes, self.peekStack(0)) catch unreachable;
                    _ = self.stack.pop();
                },
                OpCode.get_global => {
                    const str = self.readString();
                    const val = self.globals.get(str.bytes) orelse {
                        return self.runtimeError("Undefined variable '{s}'.", .{str.bytes});
                    };
                    self.stack.appendAssumeCapacity(val);
                },
                OpCode.set_global => {
                    const str = self.readString();
                    if (self.globals.getEntry(str.bytes)) |entry| {
                        entry.value_ptr.* = self.peekStack(0);
                    } else {
                        return self.runtimeError("Undefined variable '{s}'.", .{str.bytes});
                    }
                },
                OpCode.get_local => {
                    const slot = self.readByte();
                    self.stack.appendAssumeCapacity(self.stack.items[slot]);
                },
                OpCode.set_local => {
                    const slot = self.readByte();
                    self.stack.items[slot] = self.peekStack(0);
                },
                OpCode.jump_if_false => {
                    const offset = self.readShort();
                    if (self.peekStack(0).isFalsey()) {
                        self.ip += offset;
                    }
                },
                OpCode.jump => {
                    const offset = self.readShort();
                    self.ip += offset;
                },
            }
        }
        return;
    }

    fn runtimeError(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        const b = self.chunk.code.items[self.ip];
        const line = self.chunk.lines.items[b];
        std.debug.print(fmt, args);
        std.debug.print("\n", .{});
        std.debug.print("[line {d}] in script\n", .{line});
        return error.Runtime;
    }
};
