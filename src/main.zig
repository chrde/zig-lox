const std = @import("std");
const chunk = @import("chunk.zig");
const debug = @import("debug.zig");
const value = @import("value.zig");
const Vm = @import("vm.zig").Vm;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var c = chunk.Chunk.init(&arena.allocator);

    const line = 1;

    try c.write(@enumToInt(chunk.OpCode.Constant), line);
    try c.write(try c.addConstant(1.2), line);
    try c.write(@enumToInt(chunk.OpCode.Negate), line);

    try c.write(@enumToInt(chunk.OpCode.Constant), line);
    try c.write(try c.addConstant(3.4), line);
    try c.write(@enumToInt(chunk.OpCode.Add), line);

    try c.write(@enumToInt(chunk.OpCode.Constant), line);
    try c.write(try c.addConstant(5.6), line);
    try c.write(@enumToInt(chunk.OpCode.Divide), line);

    try c.write(@enumToInt(chunk.OpCode.Return), line);
    // debug.disassembleChunk(c, "test chunk");

    var vm = try Vm.init(&arena.allocator, c);
    try vm.interpret();
    // const args = try std.process.argsAlloc(std.testing.allocator);
    // defer std.process.argsFree(std.testing.allocator, args);
    // std.log.info("All your codebase are belong to us.", .{});
}

const MyEnum = enum {
    var count: u32 = 0;
    One,
    Two,

    pub fn name1(comptime self: MyEnum) []const u8 {
        return "foo" ++ @tagName(self);
    }

    pub fn foo() @This() {
        if (MyEnum.count == 0) {
            return MyEnum.One;
        } else {
            return MyEnum.Two;
        }
    }

    pub fn name(self: MyEnum) []const u8 {
        return inline for (std.meta.fields(MyEnum)) |field| {
            if (field.value == @enumToInt(self)) {
                return "foo" ++ field.name;
            }
        } else unreachable;
    }
};

test "magic" {
    MyEnum.count += 2;
    std.debug.print("{s}", .{MyEnum.name(MyEnum.foo())});
}
