const std = @import("std");
const chunk = @import("chunk.zig");
const debug = @import("debug.zig");
const value = @import("value.zig");
const Vm = @import("vm.zig").Vm;

const source_file_max_bytes = 1024 * 1024 * 100;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var it = try std.process.argsWithAllocator(&arena.allocator);
    defer it.deinit();

    var vm = try Vm.init(&arena.allocator);
    defer vm.deinit();
    // NOTE(chrde): exe name
    std.debug.assert(it.skip());

    var name_opt = it.next(&arena.allocator);
    if (name_opt) |result| {
        const name = try result;
        defer arena.allocator.free(name);
        std.debug.print("file {s}", .{name});
        try runFile(&vm, name, &arena.allocator);
    } else {
        std.debug.print("repl", .{});
        try repl(&vm);
    }

    // var c = chunk.Chunk.init(&arena.allocator);

    // const line = 1;

    // try c.write(@enumToInt(chunk.OpCode.Constant), line);
    // try c.write(try c.addConstant(1.2), line);
    // try c.write(@enumToInt(chunk.OpCode.Negate), line);

    // try c.write(@enumToInt(chunk.OpCode.Constant), line);
    // try c.write(try c.addConstant(3.4), line);
    // try c.write(@enumToInt(chunk.OpCode.Add), line);

    // try c.write(@enumToInt(chunk.OpCode.Constant), line);
    // try c.write(try c.addConstant(5.6), line);
    // try c.write(@enumToInt(chunk.OpCode.Divide), line);

    // try c.write(@enumToInt(chunk.OpCode.Return), line);
    // debug.disassembleChunk(c, "test chunk");

    // try vm.interpret1();
    // const args = try std.process.argsAlloc(std.testing.allocator);
    // defer std.process.argsFree(std.testing.allocator, args);
    // std.log.info("All your codebase are belong to us.", .{});
}

fn repl(vm: *Vm) !void {
    const stdin = std.io.getStdIn();
    const stdout = std.io.getStdOut();
    var line_buf: [1024]u8 = undefined;

    while (true) {
        try stdout.writeAll("> ");
        var line = (try stdin.reader().readUntilDelimiterOrEof(&line_buf, '\n')) orelse {
            try stdout.writeAll("\nBye bye.\n");
            break;
        };
        try vm.interpret(line);
        try stdout.writer().print("you wrote {s}\n", .{line});
    }
}

test "interpret" {
    var vm = try Vm.init(std.testing.allocator);
    defer vm.deinit();
    try vm.interpret("print 4;");
}

fn runFile(vm: *Vm, path: []const u8, allocator: *std.mem.Allocator) !void {
    const source = try readFile(path, allocator);
    defer allocator.free(source);

    try vm.interpret(source);
}

fn readFile(path: []const u8, allocator: *std.mem.Allocator) ![]const u8 {
    const file = std.fs.cwd().openFile(path, .{ .read = true }) catch |err| {
        std.log.err("Can not open file `{s}`: {any}\n", .{ path, err });
        return err;
    };
    defer file.close();

    return file.readToEndAlloc(allocator, source_file_max_bytes) catch |err| {
        std.log.err("Can not read file `{s}`: {any}\n", .{ path, err });
        return err;
    };
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
