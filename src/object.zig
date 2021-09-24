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

    pub fn create(vm: *Vm, ty: Type) !*Obj {
        switch (ty) {
            .string => {
                const new = try vm.allocator.create(String);
                new.obj.ty = ty;
                return &new.obj;
            },
        }
    }

    pub fn destroy(self: *Obj, vm: *Vm) void {
        switch (self.ty) {
            .string => {
                self.asString().destroy(vm);
                vm.allocator.destroy(self);
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

        pub fn destroy(self: *String, vm: *Vm) void {
            vm.allocator.free(self.bytes);
            vm.allocator.destroy(self);
        }

        pub fn takeString(vm: *Vm, bytes: []const u8) !*String {
            const new_obj = try Obj.create(vm, .string);
            const str = new_obj.asString();
            str.bytes = bytes;
            return str;
        }

        pub fn copy(vm: *Vm, bytes: []const u8) !*String {
            const new_bytes = vm.allocator.alloc(u8, bytes.len) catch unreachable;
            std.mem.copy(u8, new_bytes, bytes);
            const new_obj = try Obj.create(vm, .string);
            new_obj.asString().bytes = new_bytes;
            return new_obj.asString();
        }
    };
};
