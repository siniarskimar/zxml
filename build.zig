const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    _ = b.addModule("zxml", .{
        .root_source_file = b.path("src/xml.zig"),
        .target = target,
        .optimize = optimize,
    });

    const test_zxml = b.addTest(.{
        .root_source_file = b.path("src/test.zig"),
        .target = target,
    });

    const run_test_zxml = b.addRunArtifact(test_zxml);

    const step_test = b.step("test", "Run unit tests");
    step_test.dependOn(&run_test_zxml.step);
}
