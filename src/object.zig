const std = @import("std");
const Allocator = std.mem.Allocator;
const Chunk = @import("chunk.zig").Chunk;
const expect = @import("std").testing.expect;
const Vm = @import("vm.zig").Vm;

pub const Obj = struct {
    ty: Type,
    hack: bool = true,
    next: ?*Obj,

    pub const Type = enum { string, fun };

    pub fn asString(self: *Obj) *String {
        std.debug.assert(self.is(Type.string));
        return @fieldParentPtr(String, "obj", self);
    }

    pub fn asFunction(self: *Obj) *Function {
        std.debug.assert(self.is(Type.fun));
        return @fieldParentPtr(Function, "obj", self);
    }

    pub fn destroy(self: *Obj, vm: *Vm) void {
        switch (self.ty) {
            .string => {
                self.asString().destroy(vm);
            },
            .fun => {
                self.asFunction().destroy(vm);
            },
        }
    }

    pub fn is(self: Obj, ty: Type) bool {
        return self.ty == ty;
    }

    fn create(comptime T: type, t: Type, vm: *Vm) !*T {
        const new = try vm.allocator.create(T);
        new.obj.ty = t;
        new.obj.next = vm.objects;
        vm.objects = &new.obj;

        return new;
    }

    pub fn debug(self: *Obj) void {
        switch (self.ty) {
            .string => {
                const bytes = self.asString().bytes;
                std.debug.print("{s}", .{bytes});
            },
            .fun => {
                const name = self.asFunction().name.bytes;
                std.debug.print("<fn {s}>", .{name});
            },
        }
    }

    pub const String = struct {
        obj: Obj,
        bytes: []const u8,

        fn create(vm: *Vm, bytes: []const u8) !*String {
            const str = try Obj.create(String, .string, vm);

            str.bytes = bytes;
            try vm.strings.put(bytes, str);
            return str;
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

    pub const Function = struct {
        obj: Obj,
        arity: u8,
        chunk: Chunk,
        name: *String,

        pub fn create(vm: *Vm, name: *String) !*Function {
            const fun = try Obj.create(Function, .fun, vm); 
            fun.chunk = Chunk.init(vm.allocator);
            fun.name = name;
            return fun;
        }

        pub fn destroy(self: *Function, vm: *Vm) void {
            self.chunk.deinit();
            vm.allocator.destroy(self);
        }
    };
};
