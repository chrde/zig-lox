const std = @import("std");
const ArrayList = std.ArrayList;
const Value = @import("value.zig").Value;
const Obj = @import("object.zig").Obj;
const reverse_iter = @import("utils.zig").reverse_iter;

const max_stack = 255;
const max_frames = 255;

pub const Stack = struct {
    pub const Frame = struct {
        fun: *Obj.Function,
        ip: usize = 0,
        fp: usize,
    };

    const Self = @This();
    slots: ArrayList(Value),
    frames: ArrayList(Frame),

    pub fn init(allocator: *std.mem.Allocator) !Self {
        return Self{
            .slots = try ArrayList(Value).initCapacity(allocator, max_stack),
            .frames = try ArrayList(Frame).initCapacity(allocator, max_frames),
        };
    }

    pub fn pushFrame(self: *Self, frame: Frame) void {
        self.frames.appendAssumeCapacity(frame);
    }

    pub fn ip(self: *Self) usize {
        return self.currentFrame().ip;
    }

    pub fn fp(self: *Self) usize {
        return self.currentFrame().fp;
    }

    pub fn advance_ip(self: *Self, offset: usize) void {
        self.currentFrame().ip += offset;
    }

    pub fn currentFrame(self: *Self) *Frame {
        const x = self.frames.items.len - 1;
        return &self.frames.items[x];
    }

    pub fn peek(self: *Self, depth: usize) Value {
        return self.slots.items[self.slots.items.len - 1 - depth];
    }

    // idx is relative to the current fp
    pub fn get(self: *Self, idx: usize) Value {
        return self.slots.items[idx + self.fp()];
    }

    // depth is relative to the current fp
    pub fn set(self: *Self, idx: usize, val: Value) void {
        self.slots.items[idx + self.fp()] = val;
    }

    pub fn deinit(self: *Self) void {
        self.slots.deinit();
        self.frames.deinit();
    }

    pub fn debug(self: *Self) void {
        std.debug.print("f:{}       ", .{self.frames.items.len - 1});
        for (self.slots.items[0..]) |*x| {
            std.debug.print("[ ", .{});
            x.debug();
            std.debug.print(" ]", .{});
        }
        std.debug.print("\n", .{});
    }

    pub fn printStackTrace(self: *Self) void {
        var frames = reverse_iter(self.frames.items);
        while (frames.next()) |frame| {
            const fun = frame.fun;
            const instruction = frame.ip - frame.fp - 1;
            const line = fun.chunk.lines.items[instruction];
            std.debug.print("[line {d}] in {s}()\n", .{ line, fun.name.bytes });
        }
    }

    pub fn popFrame(self: *Self) Frame {
        const last = self.frames.pop();
        self.slots.shrinkRetainingCapacity(last.fp);
        return last;
    }

    pub fn pop(self: *Self) Value {
        return self.slots.pop();
    }

    pub fn push(self: *Self, val: Value) void {
        self.slots.appendAssumeCapacity(val);
    }
};
