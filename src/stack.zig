const std = @import("std");
const ArrayList = std.ArrayList;

pub const Stack = struct {
    slots: ArrayList(Value),
}
