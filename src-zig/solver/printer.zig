const std = @import("std");
const builtin = @import("builtin");
const core = @import("core.zig");

pub var visualize: bool = false;
pub var visualize_erase: bool = true;

fn cellToChar(val: u8) []const u8 {
    return switch (val) {
        0 => "0",
        1 => "1",
        2 => "2",
        3 => "3",
        4 => "4",
        5 => "5",
        6 => "6",
        7 => "7",
        8 => "8",
        core.FLAG => "F",
        core.OPEN => "-",
        core.UNKNOWN => ".",
        core.SAFE => "S",
        core.DANGER => "D",
        else => unreachable,
    };
}

pub fn eraseField(problem: *const core.Problem) void {
    if (builtin.cpu.arch == .wasm32 or builtin.cpu.arch == .wasm64) return;
    if (!visualize_erase) return;
    var buf: [16]u8 = undefined;
    const esc = std.fmt.bufPrint(&buf, "\x1b[{d}A", .{problem.h + 1}) catch unreachable;
    var bw = std.fs.File.stdout().writer(&.{});
    bw.interface.writeAll(esc) catch {};
}

pub fn fieldToStr(problem: *const core.Problem, field: []const u8, out: []u8) []const u8 {
    var len: usize = 0;
    for (0..problem.h) |y| {
        if (y > 0) {
            out[len] = '\n';
            len += 1;
        }
        for (0..problem.w) |x| {
            const ch = cellToChar(field[y * problem.w + x]);
            @memcpy(out[len..][0..ch.len], ch);
            len += ch.len;
        }
    }
    return out[0..len];
}

pub fn printField(problem: *const core.Problem, field: []const u8) void {
    if (builtin.cpu.arch == .wasm32 or builtin.cpu.arch == .wasm64) return;
    var buf: [512]u8 = undefined;
    const str = fieldToStr(problem, field, &buf);
    var bw = std.fs.File.stdout().writer(&.{});
    bw.interface.writeAll(str) catch {};
    bw.interface.writeAll("\n\n") catch {};
}

pub fn maybePrint(problem: *const core.Problem, field: []const u8) void {
    if (builtin.cpu.arch == .wasm32 or builtin.cpu.arch == .wasm64) return;
    if (visualize) {
        eraseField(problem);
        printField(problem, field);
        std.Thread.sleep(50_000_000); // 50ms per frame
    }
}
