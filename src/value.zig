pub const Value = f64;
const std = @import("std");

pub fn printValue(v: Value) void {
    std.debug.print("'{d}'", .{v});
}
