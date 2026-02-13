const std = @import("std");
const parser = @import("solver/parser.zig");
const printer = @import("solver/printer.zig");
const algorithm = @import("solver/algorithm.zig");
const solver = @import("solver.zig");

// Benchmark solving puzzles. Invoke as:
//
//     solver bench [--iters N] --puzzle "..."
//
fn cmdBenchOne(input: []const u8, iters: usize, stdout: anytype) !void {
    const name = if (std.mem.indexOfAny(u8, input, " \n")) |ws| input[0..ws] else input;
    try stdout.print("{s}", .{name});
    var buf: [256]u8 = undefined;
    // Warmup
    for (0..iters) |_| {
        _ = solver.solvePlayerProblem(input, &buf);
    }
    var timer = try std.time.Timer.start();
    for (0..iters) |_| {
        _ = solver.solvePlayerProblem(input, &buf);
    }
    const bench_ns = timer.read();
    try stdout.print("\t{d:.3} ms/solve\t{d} iters\n", .{
        @as(f64, @floatFromInt(bench_ns)) / @as(f64, @floatFromInt(iters)) / 1_000_000.0,
        iters,
    });
}

// Benchmark solving puzzles. Invoke as:
//
//     solver bench [--iters N]
//
fn cmdBenchAll(iters: usize, stdout: anytype) !void {
    const content = std.fs.cwd().readFileAlloc(std.heap.page_allocator, "dev/tests.txt", 1024 * 1024) catch |err| {
        try stdout.print("Failed to read dev/tests.txt: {}\n", .{err});
        return;
    };
    defer std.heap.page_allocator.free(content);

    var pos: usize = 0;
    while (std.mem.indexOfPos(u8, content, pos, "Given:")) |given_idx| {
        pos = given_idx + "Given:".len;
        const expect_idx = std.mem.indexOfPos(u8, content, pos, "Expect") orelse break;
        const input = std.mem.trim(u8, content[pos..expect_idx], " \n\r");
        pos = expect_idx;
        try cmdBenchOne(input, iters, stdout);
    }
}

// Visualize solving a single puzzle. Invoke as:
//
//     solver visualize --puzzle "..." [--erase false]
//
fn cmdVisualize(input: []const u8, erase: bool) void {
    const problem = parser.parsePlayerProblem(input) orelse {
        var bw = std.fs.File.stdout().writer(&.{});
        bw.interface.writeAll("Failed to parse puzzle\n") catch {};
        return;
    };
    printer.visualize = true;
    printer.visualize_erase = erase;
    printer.printField(&problem, &problem.field);

    if (algorithm.solve(&problem)) |field| {
        printer.eraseField(&problem);
        printer.printField(&problem, &field);
    } else {
        printer.eraseField(&problem);
        var bw = std.fs.File.stdout().writer(&.{});
        bw.interface.writeAll("No solution found\n") catch {};
    }
}

// Solve a puzzle and print solution. Invoke as:
//
//     solver solve --puzzle "..."
//
fn cmdSolveOne(input: []const u8, stdout: anytype) !void {
    var buf: [512]u8 = undefined;
    if (solver.solvePlayerProblem(input, &buf)) |result| {
        try stdout.print("{s}\n", .{result});
    } else {
        try stdout.print("No solution found\n", .{});
    }
}

fn cmdHint(input: []const u8, stdout: anytype) !void {
    var buf: [512]u8 = undefined;
    if (solver.hintPlayerProblem(input, &buf)) |result| {
        try stdout.print("{s}\n", .{result});
    } else {
        try stdout.print("No hints found\n", .{});
    }
}

// Solve all puzzles from public/puzzles. Invoke as:
//
//     solver solve-all
//
fn cmdSolveAll(stdout: anytype) !void {
    const allocator = std.heap.page_allocator;
    var dir = std.fs.cwd().openDir("public/puzzles", .{ .iterate = true }) catch |err| {
        try stdout.print("Failed to open public/puzzles: {}\n", .{err});
        return;
    };
    defer dir.close();

    // Collect .txt filenames
    var files = try std.ArrayList([]const u8).initCapacity(allocator, 16);
    defer {
        for (files.items) |name| allocator.free(name);
        files.deinit(allocator);
    }

    var it = dir.iterate();
    while (try it.next()) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".txt")) continue;
        const name = try allocator.dupe(u8, entry.name);
        try files.append(allocator, name);
    }

    std.mem.sort([]const u8, files.items, {}, struct {
        fn f(_: void, a: []const u8, b: []const u8) bool {
            return std.mem.order(u8, a, b) == .lt;
        }
    }.f);

    for (files.items) |filename| {
        const content = dir.readFileAlloc(allocator, filename, 10 * 1024 * 1024) catch |err| {
            try stdout.print("{s}: failed to read: {}\n", .{ filename, err });
            continue;
        };
        defer allocator.free(content);

        var timer = try std.time.Timer.start();
        var solved: usize = 0;
        var failed: usize = 0;

        var line_iter = std.mem.splitScalar(u8, content, '\n');
        while (line_iter.next()) |line| {
            if (line.len == 0) continue;

            const problem = parser.parseRawProblem(line) orelse {
                failed += 1;
                const id_end = std.mem.indexOfAny(u8, line, " \t") orelse line.len;
                try stdout.print("  PARSE ERROR: {s}\n", .{line[0..id_end]});
                continue;
            };

            var puzzle_timer = try std.time.Timer.start();
            const result = algorithm.solve(&problem);
            const puzzle_ns = puzzle_timer.read();
            const puzzle_ms = @as(f64, @floatFromInt(puzzle_ns)) / 1_000_000.0;
            if (result != null) {
                solved += 1;
                if (puzzle_ms > 5.0) {
                    const id_end = std.mem.indexOfAny(u8, line, " \t") orelse line.len;
                    try stdout.print("  SLOW: {s}\t{d:.1} ms\n", .{ line[0..id_end], puzzle_ms });
                }
            } else {
                failed += 1;
                const id_end = std.mem.indexOfAny(u8, line, " \t") orelse line.len;
                try stdout.print("  NO SOLUTION: {s}\n", .{line[0..id_end]});
                var fbuf: [512]u8 = undefined;
                try stdout.print("  field:\n{s}\n", .{printer.fieldToStr(&problem, &problem.field, &fbuf)});
            }
        }

        const elapsed_ns = timer.read();
        const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;
        if (failed > 0) {
            try stdout.print("{s}\t{d} solved\t{d} FAILED\t{d:.1} ms\n", .{ filename, solved, failed, elapsed_ms });
        } else {
            try stdout.print("{s}\t{d} solved\t{d:.1} ms\n", .{ filename, solved, elapsed_ms });
        }
    }
}

pub fn main() !void {
    var w = std.fs.File.stdout().writer(&.{});
    const stdout = &w.interface;

    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);

    if (args.len < 2) {
        try stdout.print("Usage: solver bench [--puzzle \"...\"] [--iters N]\n       solver visualize --puzzle \"...\"\n       solver solve --puzzle \"...\"\n       solver hint --puzzle \"...\"\n       solver solve-all\n", .{});
        return;
    }

    const command = args[1];
    var iters: usize = 1000;
    var puzzle: ?[]const u8 = null;
    var erase: bool = true;

    var i: usize = 2;
    while (i < args.len) : (i += 1) {
        if (std.mem.eql(u8, args[i], "--iters")) {
            i += 1;
            if (i >= args.len) {
                try stdout.print("--iters requires a number\n", .{});
                return;
            }
            iters = std.fmt.parseInt(usize, args[i], 10) catch {
                try stdout.print("--iters: invalid number '{s}'\n", .{args[i]});
                return;
            };
        } else if (std.mem.eql(u8, args[i], "--puzzle")) {
            i += 1;
            if (i >= args.len) {
                try stdout.print("--puzzle requires a puzzle string\n", .{});
                return;
            }
            puzzle = args[i];
        } else if (std.mem.eql(u8, args[i], "--erase")) {
            i += 1;
            if (i >= args.len) {
                try stdout.print("--erase requires true or false\n", .{});
                return;
            }
            erase = std.mem.eql(u8, args[i], "true");
        } else {
            try stdout.print("Unknown argument: {s}\n", .{args[i]});
            return;
        }
    }

    if (std.mem.eql(u8, command, "bench")) {
        if (puzzle) |p| {
            try cmdBenchOne(p, iters, stdout);
        } else {
            try cmdBenchAll(iters, stdout);
        }
    } else if (std.mem.eql(u8, command, "visualize")) {
        if (puzzle) |p| {
            cmdVisualize(p, erase);
        } else {
            try stdout.print("visualize requires --puzzle\n", .{});
        }
    } else if (std.mem.eql(u8, command, "solve")) {
        if (puzzle) |p| {
            try cmdSolveOne(p, stdout);
        } else {
            try stdout.print("solve requires --puzzle\n", .{});
        }
    } else if (std.mem.eql(u8, command, "hint")) {
        if (puzzle) |p| {
            try cmdHint(p, stdout);
        } else {
            try stdout.print("hint requires --puzzle\n", .{});
        }
    } else if (std.mem.eql(u8, command, "solve-all")) {
        try cmdSolveAll(stdout);
    } else {
        try stdout.print("Unknown command: {s}\n", .{command});
    }
}

test "solve and hint puzzles from tests.txt" {
    const content = std.fs.cwd().readFileAlloc(std.testing.allocator, "dev/tests.txt", 1024 * 1024) catch |err| {
        std.debug.print("\nFailed to read dev/tests.txt: {}\n", .{err});
        return err;
    };
    defer std.testing.allocator.free(content);
    var buf: [512]u8 = undefined;
    var pos: usize = 0;
    var test_count: usize = 0;

    while (std.mem.indexOfPos(u8, content, pos, "Given:")) |given_idx| {
        pos = given_idx + "Given:".len;
        const expect_solution_idx = std.mem.indexOfPos(u8, content, pos, "Expect solution:") orelse break;
        const input = std.mem.trim(u8, content[pos..expect_solution_idx], " \n\r");

        pos = expect_solution_idx + "Expect solution:".len;
        const expect_hint_idx = std.mem.indexOfPos(u8, content, pos, "Expect hint:") orelse break;
        const expected_solution = std.mem.trim(u8, content[pos..expect_hint_idx], " \n\r");

        pos = expect_hint_idx + "Expect hint:".len;
        const next_given = std.mem.indexOfPos(u8, content, pos, "Given:");
        const hint_end = next_given orelse content.len;
        const expected_hint = std.mem.trim(u8, content[pos..hint_end], " \n\r");

        pos = hint_end;

        // Test solve
        const solve_result = solver.solvePlayerProblem(input, &buf) orelse {
            std.debug.print("\nFAIL solve: returned null for: {s}\n", .{input[0..@min(input.len, 40)]});
            return error.TestUnexpectedResult;
        };

        std.testing.expectEqualStrings(expected_solution, solve_result) catch {
            std.debug.print("\nFAIL solve: {s}\n", .{input[0..@min(input.len, 40)]});
            return error.TestUnexpectedResult;
        };

        // Test hint
        const hint_result = solver.hintPlayerProblem(input, &buf) orelse {
            std.debug.print("\nFAIL hint: returned null for: {s}\n", .{input[0..@min(input.len, 40)]});
            return error.TestUnexpectedResult;
        };

        std.testing.expectEqualStrings(expected_hint, hint_result) catch {
            std.debug.print("\nFAIL hint: {s}\n", .{input[0..@min(input.len, 40)]});
            return error.TestUnexpectedResult;
        };

        test_count += 1;
    }

    try std.testing.expect(test_count > 0);
}
