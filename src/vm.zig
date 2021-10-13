const std = @import("std");
const ArrayList = std.ArrayList;
const Chunk = @import("chunk.zig").Chunk;
const Stack = @import("stack.zig").Stack;
const OpCode = @import("chunk.zig").OpCode;
const Parser = @import("parser.zig").Parser;
const Value = @import("value.zig").Value;
const Obj = @import("object.zig").Obj;
const debug = @import("debug.zig");
const Compiler = @import("compiler.zig").Compiler;
const range = @import("utils.zig").range;

const max_stack = 255;
const max_frames = 255;

pub const InterpreterError = error{
    Compile,
    Runtime,
};

const BinaryOp = enum { add, sub, mul, div, greater, less };

pub const Vm = struct {
    const Self = @This();
    allocator: *std.mem.Allocator,
    objects: ?*Obj = null,
    globals: std.StringHashMap(Value),
    strings: std.StringHashMap(*Obj.String),
    stack: Stack,

    pub fn init(allocator: *std.mem.Allocator) !Self {
        return Self{
            .allocator = allocator,
            .strings = std.StringHashMap(*Obj.String).init(allocator),
            .globals = std.StringHashMap(Value).init(allocator),
            .stack = try Stack.init(allocator),
        };
    }

    pub fn defineNative(self: *Self, name: []const u8, native: Obj.NativeFn.NativeType) void {
        const v_name = Obj.String.copy(self, name) catch unreachable;
        self.stack.push(Value{ .obj = &v_name.obj });
        const fun = Obj.NativeFn.create(self, v_name, native) catch unreachable;
        self.stack.push(Value{ .obj = &fun.obj });
        self.globals.put(self.stack.peek(1).obj.asString().bytes, self.stack.peek(0)) catch unreachable;
        _ = self.stack.pop();
        _ = self.stack.pop();
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

    fn chunk(self: *Self) *Chunk {
        return &self.stack.currentFrame().fun.chunk;
    }

    fn readShort(self: *Self) u16 {
        const f = self.stack.currentFrame();
        const s = std.mem.readIntSlice(u16, self.chunk().code.items[f.ip..], .Little);
        f.ip += 2;
        return s;
    }

    fn readByte(self: *Self) u8 {
        const f = self.stack.currentFrame();
        const b = self.chunk().code.items[f.ip];
        f.ip += 1;
        return b;
    }

    fn readConstant(self: *Self) Value {
        const b = self.readByte();
        return self.chunk().constants.items[b];
    }

    fn readString(self: *Self) *Obj.String {
        const b = self.readByte();
        const v = self.chunk().constants.items[b];
        return v.obj.asString();
    }

    fn binaryOp(self: *Self, op: BinaryOp) error{Runtime}!void {
        if (!(self.stack.peek(0).isNumber() and self.stack.peek(1).isNumber())) {
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
        self.stack.push(result);
    }

    fn call(self: *Self, fun: *Obj.Function, arg_count: u8) !void {
        if (arg_count != fun.arity) {
            return self.runtimeError("Expected {d} arguments but got {d}.", .{ fun.arity, arg_count });
        }
        const frame = Stack.Frame{
            .fun = fun,
            .fp = self.stack.slots.items.len - 1 - arg_count,
        };
        self.stack.pushFrame(frame);
    }

    fn callValue(self: *Self, callee: Value, arg_count: u8) !void {
        if (!callee.isObj()) {
            return self.runtimeError("Can only call functions and classes.", .{});
        }
        switch (callee.obj.ty) {
            .fun => try self.call(callee.obj.asFunction(), arg_count),
            .native => {
                const native = callee.obj.asNative();
                const args = self.stack.slots.items.len - 1 - arg_count;
                const native_args = self.stack.slots.items[args..];
                const result = (native.native)(native_args);
                for (range(arg_count)) |_| {
                    _ = self.stack.pop();
                }
                self.stack.push(result);
            },
            else => return self.runtimeError("Can only call functions and classes.", .{}),
        }
    }

    pub fn interpret(self: *Self, source: []const u8) InterpreterError!void {
        var parser = Parser.init(source);
        var compiler = Compiler.init(self, &parser, .script) catch unreachable;
        defer compiler.deinit();
        const result = try compiler.compile();

        if (result) |fun| {
            self.stack.push(Value{ .obj = &fun.obj });
            const first_frame = Stack.Frame{
                .fun = fun,
                .fp = self.stack.slots.items.len - 1,
            };
            self.stack.pushFrame(first_frame);
            self.defineNative("clock", Obj.NativeFn.clock);
            return self.run();
        } else {
            // TODO(chrde): return some error
        }
    }

    fn concatenate(self: *Self) !void {
        const right = self.stack.pop().obj.asString();
        const left = self.stack.pop().obj.asString();
        const new_bytes = try std.mem.concat(self.allocator, u8, &[_][]const u8{ left.bytes, right.bytes });
        const new_str = try Obj.String.takeString(self, new_bytes);
        self.stack.push(Value{ .obj = &new_str.obj });
    }

    pub fn run(self: *Self) error{Runtime}!void {
        std.debug.print("||= running =||\n", .{});
        while (true) {
            self.stack.debug();
            _ = debug.disassembleInstruction(self.chunk().*, self.stack.ip());
            switch (@intToEnum(OpCode, self.readByte())) {
                OpCode.@"return" => {
                    const result = self.stack.pop();
                    _ = self.stack.popFrame();
                    if (self.stack.frames.items.len == 0) {
                        return;
                    } else {
                        self.stack.push(result);
                    }
                },
                OpCode.constant => {
                    const constant = self.readConstant();
                    self.stack.push(constant);
                },
                OpCode.negate => {
                    if (self.stack.peek(0).isNumber()) {
                        const n = self.stack.pop().number;
                        self.stack.push(Value{ .number = -n });
                    } else {
                        return self.runtimeError("Operand must be a number.", .{});
                    }
                },
                OpCode.add => {
                    if (self.stack.peek(0).isString() and self.stack.peek(1).isString()) {
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
                OpCode.@"true" => self.stack.push(Value{ .bool = true }),
                OpCode.@"false" => self.stack.push(Value{ .bool = false }),
                OpCode.nil => self.stack.push(Value{ .nil = {} }),
                OpCode.not => self.stack.push(Value{ .bool = self.stack.pop().isFalsey() }),
                OpCode.equal => {
                    const right = self.stack.pop();
                    const left = self.stack.pop();
                    const val = Value{ .bool = left.isEqual(right) };
                    self.stack.push(val);
                },
                OpCode.print => {
                    self.stack.pop().debug();
                    std.debug.print("\n", .{});
                },
                OpCode.pop => _ = self.stack.pop(),
                OpCode.define_global => {
                    const str = self.readString();
                    self.globals.put(str.bytes, self.stack.peek(0)) catch unreachable;
                    _ = self.stack.pop();
                },
                OpCode.get_global => {
                    const str = self.readString();
                    const val = self.globals.get(str.bytes) orelse {
                        return self.runtimeError("Undefined variable '{s}'.", .{str.bytes});
                    };
                    self.stack.push(val);
                },
                OpCode.set_global => {
                    const str = self.readString();
                    if (self.globals.getEntry(str.bytes)) |entry| {
                        entry.value_ptr.* = self.stack.peek(0);
                    } else {
                        return self.runtimeError("Undefined variable '{s}'.", .{str.bytes});
                    }
                },
                OpCode.get_local => {
                    const slot = self.readByte();
                    self.stack.push(self.stack.get(slot));
                },
                OpCode.set_local => {
                    const slot = self.readByte();
                    self.stack.set(slot, self.stack.peek(0));
                },
                OpCode.jump_if_false => {
                    const offset = self.readShort();
                    const cond = self.stack.peek(0);
                    if (cond.isFalsey()) {
                        self.stack.advance_ip(offset);
                    }
                },
                OpCode.jump_if_true => {
                    const offset = self.readShort();
                    if (!self.stack.peek(0).isFalsey()) {
                        self.stack.advance_ip(offset);
                    }
                },
                OpCode.jump => {
                    const offset = self.readShort();
                    self.stack.advance_ip(offset);
                },
                OpCode.loop => {
                    const offset = self.readShort();
                    self.stack.advance_ip(offset);
                },
                OpCode.call => {
                    const arg_count = self.readByte();
                    try self.callValue(self.stack.peek(arg_count), arg_count);
                },
            }
        }
        return;
    }

    fn runtimeError(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        std.debug.print(fmt, args);
        std.debug.print("\n", .{});
        self.stack.printStackTrace();
        return error.Runtime;
    }
};
