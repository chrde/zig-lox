const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});

    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("zig-lox", "src/main.zig");
    exe.setTarget(target);
    // exe.addPackage(.{ .name = "zig-lox-lib", .path = "lib/src/lib.zig" });
    exe.setBuildMode(mode);
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const tests = b.addTest("src/tests.zig");
    tests.setBuildMode(mode);
    tests.setNamePrefix("debug test ");
    const test_step = b.step("test", "Run all tests in debug mode.");
    test_step.dependOn(&tests.step);

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
    run_step.dependOn(&run_cmd.step);
}
