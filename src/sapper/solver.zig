//! Minesweeper constraint solver ported from ClojureScript
//!
//! Implements three constraint checks:
//! - total-check: Ensures flag count stays within bounds
//! - vanilla-check: Ensures numbered cells have valid neighbor counts
//! - anti-triplet-check: Ensures no three flags in a row (horizontal, vertical, diagonal)
//!
//! Build and run benchmark:
//!   zig build-exe solver.zig -O ReleaseFast && ./solver
//!
//! Run tests:
//!   zig test solver.zig
//!
//! Input format: "[T]8x8-26-1234D\n..1?F..\n..." where:
//!   [T], [V], [T*] etc = rule indicators (multiple allowed)
//!   8x8 = width x height
//!   26 = total flags
//!   1234D = puzzle id (ignored)
//!   . = unknown cell
//!   F = flagged cell (mine)
//!   ? = confirmed safe (opened) cell
//!   0-8 = numbered cell showing adjacent mine count

const std = @import("std");

// Field cell values
const FLAG: i8 = 9;
const OPEN: i8 = 10;
const UNKNOWN: i8 = 11;

const Rules = packed struct {
    total: bool = false,
    vanilla: bool = false,
    anti_triplet: bool = false,
};

const Problem = struct {
    field: []i8,
    w: usize,
    h: usize,
    total_flags: usize,
    rules: Rules,
    known_indices: []const u8,
    constrained_indices: []const u8,
    unknown_count: usize,
    flag_indices: std.ArrayList(u8),
    open_indices: std.ArrayList(u8),

    fn setFlag(self: *Problem, i: usize) void {
        self.field[i] = FLAG;
        self.flag_indices.appendAssumeCapacity(@intCast(i));
        self.unknown_count -= 1;
    }

    fn setOpen(self: *Problem, i: usize) void {
        self.field[i] = OPEN;
        self.open_indices.appendAssumeCapacity(@intCast(i));
        self.unknown_count -= 1;
    }

    fn setUnknown(self: *Problem, i: usize) void {
        if (self.field[i] == FLAG) {
            _ = self.flag_indices.pop();
        } else if (self.field[i] == OPEN) {
            _ = self.open_indices.pop();
        }
        self.field[i] = UNKNOWN;
        self.unknown_count += 1;
    }
};

fn countNeighbors(problem: *const Problem, idx: usize, val: i8) i8 {
    const x = idx % problem.w;
    const y = idx / problem.w;
    var res: i8 = 0;

    const x_start: usize = if (x > 0) x - 1 else 0;
    const x_end: usize = if (x + 1 < problem.w) x + 2 else problem.w;
    const y_start: usize = if (y > 0) y - 1 else 0;
    const y_end: usize = if (y + 1 < problem.h) y + 2 else problem.h;

    for (y_start..y_end) |ny| {
        for (x_start..x_end) |nx| {
            if (nx == x and ny == y) continue;
            if (problem.field[ny * problem.w + nx] == val) res += 1;
        }
    }
    return res;
}

// [*] Count amount of mines
fn totalCheck(problem: *const Problem) bool {
    const flagged = problem.flag_indices.items.len;
    return flagged <= problem.total_flags and
        flagged + problem.unknown_count >= problem.total_flags;
}

// [V] Nothing special - check each numbered cell
fn vanillaCheck(problem: *const Problem) bool {
    for (problem.known_indices) |i| {
        const value = problem.field[i];
        const fs = countNeighbors(problem, i, FLAG);
        const unk = countNeighbors(problem, i, UNKNOWN);
        if (fs > value) return false;
        if (fs + unk < value) return false;
    }
    return true;
}

// [T] Flags may not form row of three orthogonally or diagonally
fn antiTripletCheck(problem: *const Problem) bool {
    const flag_indices = problem.flag_indices.items;
    for (flag_indices) |idx| {
        const y = idx / problem.w;
        const x = idx % problem.w;

        // FFF horizontal
        if (x + 2 < problem.w) {
            if (problem.field[idx + 1] == FLAG and problem.field[idx + 2] == FLAG) {
                return false;
            }
        }

        // FFF vertical
        if (y + 2 < problem.h) {
            if (problem.field[idx + problem.w] == FLAG and problem.field[idx + 2 * problem.w] == FLAG) {
                return false;
            }
        }

        // Diagonal down-right
        if (x + 2 < problem.w and y + 2 < problem.h) {
            if (problem.field[idx + problem.w + 1] == FLAG and problem.field[idx + 2 * problem.w + 2] == FLAG) {
                return false;
            }
        }

        // Diagonal down-left
        if (x >= 2 and y + 2 < problem.h) {
            if (problem.field[idx + problem.w - 1] == FLAG and problem.field[idx + 2 * problem.w - 2] == FLAG) {
                return false;
            }
        }
    }
    return true;
}

// fn autoFinish(problem: *Problem) bool {
//     const ctx = problem.ctx;

//     // Can open the rest
//     if (problem.flagged() == ctx.total_flags) {
//         for (0..ctx.size) |i| {
//             if (problem.field[i] == UNKNOWN) {
//                 problem.withVal(i, OPEN);
//             }
//         }
//         return true;
//     }

//     // Can flag the rest
//     if (ctx.total_flags - problem.flagged() == problem.unknown_count) {
//         for (0..ctx.size) |i| {
//             if (problem.field[i] == UNKNOWN) {
//                 problem.withVal(i, FLAG);
//             }
//         }
//         return true;
//     }

//     return false;
// }

fn bestCandidate(problem: *const Problem) ?u8 {
    var min_rating: i8 = 127;
    var min_index: ?u8 = null;

    for (problem.known_indices) |i| {
        const value = problem.field[i];
        const flags = countNeighbors(problem, i, FLAG);
        const rating = value - flags;

        if (rating < min_rating) {
            // Find first unknown neighbor
            const x = i % problem.w;
            const y = i / problem.w;
            const x_start: usize = if (x > 0) x - 1 else 0;
            const x_end: usize = if (x + 1 < problem.w) x + 2 else problem.w;
            const y_start: usize = if (y > 0) y - 1 else 0;
            const y_end: usize = if (y + 1 < problem.h) y + 2 else problem.h;

            outer: for (y_start..y_end) |ny| {
                for (x_start..x_end) |nx| {
                    const nbi = ny * problem.w + nx;
                    if (problem.field[nbi] == UNKNOWN) {
                        min_rating = rating;
                        min_index = @intCast(nbi);
                        break :outer;
                    }
                }
            }
        }
    }

    if (min_index) |idx| {
        return idx;
    }

    // Fallback: first unknown in constrained_indices
    for (problem.constrained_indices) |c| {
        if (problem.field[c] == UNKNOWN) {
            return c;
        }
    }

    return null;
}

fn autoOpen(problem: *Problem) bool {
    const flag_indices_len_before = problem.flag_indices.items.len;
    const open_indices_len_before = problem.open_indices.items.len;

    for (problem.known_indices) |i| {
        const value = problem.field[i];
        const fs = countNeighbors(problem, i, FLAG);
        const unk = countNeighbors(problem, i, UNKNOWN);
        if (unk > 0 and (value == fs or value - fs == unk)) {
            const x: usize = i % problem.w;
            const y: usize = i / problem.w;
            const x_start: usize = if (x > 0) x - 1 else 0;
            const x_end: usize = if (x + 1 < problem.w) x + 2 else problem.w;
            const y_start: usize = if (y > 0) y - 1 else 0;
            const y_end: usize = if (y + 1 < problem.h) y + 2 else problem.h;

            for (y_start..y_end) |ny| {
                for (x_start..x_end) |nx| {
                    if (nx == x and ny == y) continue;
                    const nbi = ny * problem.w + nx;
                    if (problem.field[nbi] != UNKNOWN) continue;
                    if (value == fs) problem.setOpen(nbi)
                    else if (value - fs == unk) problem.setFlag(nbi);
                }
            }
        }
    }

    if (autoFinish(problem))
        return true;

    // Undo flags/opens
    while (problem.flag_indices.items.len > flag_indices_len_before) {
        const idx = problem.flag_indices.items[problem.flag_indices.items.len - 1];
        problem.setUnknown(idx);
    }
    while (problem.open_indices.items.len > open_indices_len_before) {
        const idx = problem.open_indices.items[problem.open_indices.items.len - 1];
        problem.setUnknown(idx);
    }

    return false;
}

fn autoFinish(problem: *Problem) bool {
    const flag_indices_len_before = problem.flag_indices.items.len;
    const open_indices_len_before = problem.open_indices.items.len;
    const flagged = flag_indices_len_before;

    if (problem.unknown_count > 0) {
        if (problem.unknown_count == problem.total_flags - flagged) {
            // Can flag the rest
            for (0..problem.w * problem.h) |i| {
                if (problem.field[i] == UNKNOWN) problem.setFlag(i);
            }
        } else if (flagged == problem.total_flags) {
            // Can open the rest
            for (0..problem.w * problem.h) |i| {
                if (problem.field[i] == UNKNOWN) problem.setOpen(i);
            }
        }
    }

    if (checkConstraints(problem))
        return true;

    // Undo
    while (problem.flag_indices.items.len > flag_indices_len_before) {
        const idx = problem.flag_indices.items[problem.flag_indices.items.len - 1];
        problem.setUnknown(idx);
    }
    while (problem.open_indices.items.len > open_indices_len_before) {
        const idx = problem.open_indices.items[problem.open_indices.items.len - 1];
        problem.setUnknown(idx);
    }

    return false;
}

fn checkConstraints(problem: *Problem) bool {
    if (problem.rules.total and !totalCheck(problem)) return false;
    if (problem.rules.vanilla and !vanillaCheck(problem)) return false;
    if (problem.rules.anti_triplet and !antiTripletCheck(problem)) return false;

    if (problem.unknown_count == 0) return true;
    return diveDeeper(problem);
}

fn diveDeeper(problem: *Problem) bool {
    const candidate = bestCandidate(problem) orelse return false;

    // Try FLAG first
    problem.setFlag(candidate);
    if (autoOpen(problem)) return true;
    problem.setUnknown(candidate);

    // Try OPEN
    problem.setOpen(candidate);
    if (autoOpen(problem)) return true;
    problem.setUnknown(candidate);

    return false;
}

fn parseCell(ch: u8) i8 {
    return switch (ch) {
        'F' => FLAG,
        '?' => OPEN,
        '.' => UNKNOWN,
        '0'...'8' => @intCast(ch - '0'),
        else => UNKNOWN,
    };
}

fn cellToChar(val: i8) u8 {
    if (val >= 0 and val <= 8) return '0' + @as(u8, @intCast(val));
    return switch (val) {
        FLAG => 'F',
        OPEN => '?',
        else => '.',
    };
}

fn isDigit(ch: u8) bool {
    return ch >= '0' and ch <= '9';
}

fn isWhitespace(ch: u8) bool {
    return ch == ' ' or ch == '\t' or ch == '\n' or ch == '\r';
}

fn parseNumber(input: []const u8, pos: *usize) ?usize {
    const start = pos.*;
    while (pos.* < input.len and isDigit(input[pos.*])) {
        pos.* += 1;
    }
    if (pos.* == start) return null;
    return std.fmt.parseInt(usize, input[start..pos.*], 10) catch null;
}

fn skipWhitespace(input: []const u8, pos: *usize) void {
    while (pos.* < input.len and isWhitespace(input[pos.*])) {
        pos.* += 1;
    }
}

/// Parse and solve puzzle
/// Input format: "[T]8x8-26-1234D\n..1?F.."
/// Returns solved field as string "..F?.\n..1F." or null if unsolvable
/// Result is written into caller-provided `out` buffer
pub fn solve(input: []const u8, out: []u8) ?[]const u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var pos: usize = 0;

    // Parse rule indicators [T], [V], [T*], etc.
    // All puzzles have total and vanilla checks
    var rules = Rules{ .total = true, .vanilla = true };
    while (pos < input.len and input[pos] == '[') {
        pos += 1; // skip '['
        const rule_start = pos;
        while (pos < input.len and input[pos] != ']') {
            pos += 1;
        }
        const rule = input[rule_start..pos];
        if (std.mem.eql(u8, rule, "T")) {
            rules.anti_triplet = true;
        }
        if (pos < input.len) pos += 1; // skip ']'
    }

    // Parse WxH
    const w = parseNumber(input, &pos) orelse return null;
    if (pos >= input.len or input[pos] != 'x') return null;
    pos += 1; // skip 'x'
    const h = parseNumber(input, &pos) orelse return null;

    // Skip '-'
    if (pos >= input.len or input[pos] != '-') return null;
    pos += 1;

    // Parse total flags
    const total_flags = parseNumber(input, &pos) orelse return null;

    // Skip '-'
    if (pos >= input.len or input[pos] != '-') return null;
    pos += 1;

    // Skip puzzle id (alphanumeric)
    while (pos < input.len and !isWhitespace(input[pos])) {
        pos += 1;
    }

    // Skip whitespace before field
    skipWhitespace(input, &pos);

    // Allocate field
    const size = w * h;
    const field = allocator.alloc(i8, size) catch return null;
    var unknown_count: usize = 0;
    var flag_indices = std.ArrayList(u8).initCapacity(allocator, size) catch return null;
    var open_indices = std.ArrayList(u8).initCapacity(allocator, size) catch return null;

    var known_indices = std.ArrayList(u8).initCapacity(allocator, size) catch return null;
    var constrained_indices = std.ArrayList(u8).initCapacity(allocator, size) catch return null;

    var idx: usize = 0;
    while (pos < input.len and idx < size) {
        const ch = input[pos];
        if (isWhitespace(ch)) {
            pos += 1;
            continue;
        }

        const val = parseCell(ch);
        field[idx] = val;

        if (val == FLAG) {
            flag_indices.appendAssumeCapacity(@intCast(idx));
        } else if (val == OPEN) {
            open_indices.appendAssumeCapacity(@intCast(idx));
        } else if (val == UNKNOWN) {
            unknown_count += 1;
            constrained_indices.appendAssumeCapacity(@intCast(idx));
        }

        if (val <= 8) {
            known_indices.appendAssumeCapacity(@intCast(idx));
        }

        idx += 1;
        pos += 1;
    }

    if (idx != size) return null; // Not enough cells

    var problem = Problem{
        .field = field,
        .w = w,
        .h = h,
        .total_flags = total_flags,
        .rules = rules,
        .known_indices = known_indices.items,
        .constrained_indices = constrained_indices.items,
        .unknown_count = unknown_count,
        .flag_indices = flag_indices,
        .open_indices = open_indices,
    };

    if (!autoOpen(&problem)) return null;

    // Build result string into caller's buffer
    var ri: usize = 0;
    for (0..h) |y| {
        if (y > 0) {
            out[ri] = '\n';
            ri += 1;
        }
        for (0..w) |x| {
            out[ri] = cellToChar(field[y * w + x]);
            ri += 1;
        }
    }
    return out[0..ri];
}

// Benchmark
pub fn main() !void {
    var w = std.fs.File.stdout().writer(&.{});
    const stdout = &w.interface;

    const test_cases = [_]struct {
        name: []const u8,
        input: []const u8,
    }{
        .{
            .name = "[V]8x8-26-1388D",
            .input =
            \\[V]8x8-26-1388D
            \\........
            \\.....6..
            \\2.....4.
            \\2.2....1
            \\........
            \\........
            \\2?.42...
            \\0111?.3.
            ,
        },
        .{
            .name = "[T]8x8-26-10817",
            .input =
            \\[T]8x8-26-10817
            \\..3.....
            \\.......3
            \\........
            \\.4..3...
            \\.....5..
            \\........
            \\..3.....
            \\..2.....
            ,
        },
    };

    for (test_cases) |tc| {
        try stdout.print("Benching {s}...\n", .{tc.name});

        var buf: [256]u8 = undefined;

        // Warmup
        const warmup_iters: usize = 1000;
        var timer = try std.time.Timer.start();
        for (0..warmup_iters) |_| {
            _ = solve(tc.input, &buf);
        }
        const warmup_ns = timer.read();
        try stdout.print("  Warmup: {d:.3} ms/solve, {d} iters\n", .{
            @as(f64, @floatFromInt(warmup_ns)) / @as(f64, @floatFromInt(warmup_iters)) / 1_000_000.0,
            warmup_iters,
        });

        // Benchmark
        const bench_iters: usize = 10000;
        timer.reset();
        for (0..bench_iters) |_| {
            _ = solve(tc.input, &buf);
        }
        const bench_ns = timer.read();
        try stdout.print("  Bench:  {d:.3} ms/solve, {d} iters\n", .{
            @as(f64, @floatFromInt(bench_ns)) / @as(f64, @floatFromInt(bench_iters)) / 1_000_000.0,
            bench_iters,
        });
    }
}

test "solve puzzles from tests.txt" {
    const content = std.fs.cwd().readFileAlloc(std.testing.allocator, "dev/tests.txt", 1024 * 1024) catch |err| {
        std.debug.print("\nFailed to read dev/tests.txt: {}\n", .{err});
        return err;
    };
    defer std.testing.allocator.free(content);
    var buf: [256]u8 = undefined;
    var pos: usize = 0;
    var test_count: usize = 0;

    while (std.mem.indexOfPos(u8, content, pos, "Given:")) |given_idx| {
        pos = given_idx + "Given:".len;
        const expect_idx = std.mem.indexOfPos(u8, content, pos, "Expect:") orelse break;
        const input = std.mem.trim(u8, content[pos..expect_idx], " \n\r");

        pos = expect_idx + "Expect:".len;
        const next_given = std.mem.indexOfPos(u8, content, pos, "Given:");
        const expect_end = next_given orelse content.len;
        const expected = std.mem.trim(u8, content[pos..expect_end], " \n\r");

        pos = expect_end;

        const result = solve(input, &buf) orelse {
            std.debug.print("\nFAIL: solve returned null for: {s}\n", .{input[0..@min(input.len, 40)]});
            return error.TestUnexpectedResult;
        };

        std.testing.expectEqualStrings(expected, result) catch {
            std.debug.print("\nFAIL: {s}\n", .{input[0..@min(input.len, 40)]});
            return error.TestUnexpectedResult;
        };

        test_count += 1;
    }

    try std.testing.expect(test_count > 0);
}
