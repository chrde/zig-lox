const std = @import("std");
const Allocator = std.mem.Allocator;
const expect = @import("std").testing.expect;
const Vm = @import("vm.zig").Vm;

pub const Obj = struct {
    ty: Type,
    hack: bool = true,
    next: ?*Obj,

    pub const Type = enum { string };

    pub fn asString(self: *Obj) *String {
        std.debug.assert(self.is(Type.string));
        return @fieldParentPtr(String, "obj", self);
    }

    pub fn destroy(self: *Obj, vm: *Vm) void {
        switch (self.ty) {
            .string => {
                self.asString().destroy(vm);
            },
        }
    }

    pub fn is(self: Obj, ty: Type) bool {
        return self.ty == ty;
    }

    pub fn debug(self: *Obj) void {
        switch (self.ty) {
            .string => {
                const bytes = self.asString().bytes;
                std.debug.print("{s}", .{bytes});
            },
        }
    }

    pub const String = struct {
        obj: Obj,
        bytes: []const u8,

        fn create(vm: *Vm, bytes: []const u8) !*String {
            const new = try vm.allocator.create(String);
            new.obj.ty = .string;
            new.obj.next = vm.objects;
            new.bytes = bytes;
            vm.objects = &new.obj;
            try vm.strings.put(bytes, new);
            return new;
        }

        pub fn destroy(self: *String, vm: *Vm) void {
            vm.allocator.free(self.bytes);
            vm.allocator.destroy(self);
        }

        pub fn takeString(vm: *Vm, bytes: []const u8) !*String {
            if (vm.strings.get(bytes)) |interned| {
                vm.allocator.free(bytes);
                return interned;
            } else {
                return String.create(vm, bytes);
            }
        }

        pub fn copy(vm: *Vm, bytes: []const u8) !*String {
            if (vm.strings.get(bytes)) |interned| {
                return interned;
            } else {
                const new_bytes = try vm.allocator.dupe(u8, bytes);
                return String.create(vm, new_bytes);
            }
        }
    };
};
