const std = @import("std");

// https://zigforum.org/t/ideomatic-reverse-iteration/799
fn ReverseIterator(comptime Container: type) type {
    return struct {
        items: Container,
        current: usize = 0,

        pub fn next(self: *@This()) ?std.meta.Child(Container) {
            if (self.current >= self.items.len) return null;
            self.current += 1;
            return self.items[self.items.len - self.current];
        }
    };
}
pub fn reverse_iter(items: anytype) ReverseIterator(@TypeOf(items)) {
    return .{ .items = items };
}

// https://github.com/nektro/zig-range/blob/890ca308fe09b3d5c866d5cfb3b3d7a95dbf939f/src/lib.zig#L9
pub fn range(len: usize) []const u0 {
    return @as([*]u0, undefined)[0..len];
}
