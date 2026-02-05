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
const FLAG: u8 = 9;
const OPEN: u8 = 10;
const UNKNOWN: u8 = 11;

const Rules = packed struct {
    total: bool = false,
    vanilla: bool = false,
    anti_triplet: bool = false,
};

const Problem = struct {
    field: []u8,
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

fn countNeighbors(problem: *const Problem, idx: usize, val: u8) u8 {
    const x = idx % problem.w;
    const y = idx / problem.w;
    var res: u8 = 0;

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

fn checkConstraints(problem: *const Problem) bool {
    if (problem.rules.total and !totalCheck(problem)) return false;
    if (problem.rules.vanilla and !vanillaCheck(problem)) return false;
    if (problem.rules.anti_triplet and !antiTripletCheck(problem)) return false;
    return true;
}

// fn autoOpen(problem: *Problem, known: []const u8) bool {
//     const ctx = problem.ctx;
//     var changed = false;
//     var known_idx: usize = 0;
//     while (known_idx < known.len) {
//         const i = known[known_idx];
//         const value = problem.field[i];
//         const unk = countNeighbors(problem.field, ctx, i, UNKNOWN);

//         if (unk == 0) {
//             known_idx += 1;
//             continue;
//         }

//         const fs = countNeighbors(problem.field, ctx, i, FLAG);

//         // All flagged, can open the rest
//         if (fs == value) {
//             const x = i % ctx.w;
//             const y = i / ctx.w;
//             const x_start: usize = if (x > 0) x - 1 else 0;
//             const x_end: usize = if (x + 1 < ctx.w) x + 2 else ctx.w;
//             const y_start: usize = if (y > 0) y - 1 else 0;
//             const y_end: usize = if (y + 1 < ctx.h) y + 2 else ctx.h;

//             for (y_start..y_end) |ny| {
//                 for (x_start..x_end) |nx| {
//                     const nbi = ny * ctx.w + nx;
//                     if (problem.field[nbi] == UNKNOWN) {
//                         problem.withVal(nbi, OPEN);
//                     }
//                 }
//             }
//             known_idx = 0;
//             changed = true;
//             continue;
//         }

//         // Can flag the rest
//         if (value - fs == unk) {
//             const x = i % ctx.w;
//             const y = i / ctx.w;
//             const x_start: usize = if (x > 0) x - 1 else 0;
//             const x_end: usize = if (x + 1 < ctx.w) x + 2 else ctx.w;
//             const y_start: usize = if (y > 0) y - 1 else 0;
//             const y_end: usize = if (y + 1 < ctx.h) y + 2 else ctx.h;

//             for (y_start..y_end) |ny| {
//                 for (x_start..x_end) |nx| {
//                     const nbi = ny * ctx.w + nx;
//                     if (problem.field[nbi] == UNKNOWN) {
//                         problem.withVal(nbi, FLAG);
//                     }
//                 }
//             }
//             known_idx = 0;
//             changed = true;
//             continue;
//         }

//         known_idx += 1;
//     }
//     return changed;
// }

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
    var min_rating: u8 = 255;
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

fn solveImpl(problem: *Problem) bool {
    // Check constraints
    if (!checkConstraints(problem)) {
        return false;
    }

    // Leaf - all explored
    if (problem.unknown_count == 0) {
        return true;
    }

    // Find best candidate to try
    const candidate = bestCandidate(problem) orelse return false;

    // Try FLAG first
    problem.setFlag(candidate);
    if (solveImpl(problem)) {
        return true;
    }
    problem.setUnknown(candidate);

    // Try OPEN
    problem.setOpen(candidate);
    if (solveImpl(problem)) {
        return true;
    }
    problem.setUnknown(candidate);

    return false;
}

fn parseCell(ch: u8) u8 {
    return switch (ch) {
        'F' => FLAG,
        '?' => OPEN,
        '.' => UNKNOWN,
        '0'...'8' => ch - '0',
        else => UNKNOWN,
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
pub fn solve(input: []const u8) bool {
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
    const w = parseNumber(input, &pos) orelse return false;
    if (pos >= input.len or input[pos] != 'x') return false;
    pos += 1; // skip 'x'
    const h = parseNumber(input, &pos) orelse return false;

    // Skip '-'
    if (pos >= input.len or input[pos] != '-') return false;
    pos += 1;

    // Parse total flags
    const total_flags = parseNumber(input, &pos) orelse return false;

    // Skip '-'
    if (pos >= input.len or input[pos] != '-') return false;
    pos += 1;

    // Skip puzzle id (alphanumeric)
    while (pos < input.len and !isWhitespace(input[pos])) {
        pos += 1;
    }

    // Skip whitespace before field
    skipWhitespace(input, &pos);

    // Allocate field
    const size = w * h;
    const field = allocator.alloc(u8, size) catch return false;
    var unknown_count: usize = 0;
    var flag_indices = std.ArrayList(u8).initCapacity(allocator, size) catch return false;
    var open_indices = std.ArrayList(u8).initCapacity(allocator, size) catch return false;

    var known_indices = std.ArrayList(u8).initCapacity(allocator, size) catch return false;
    var constrained_indices = std.ArrayList(u8).initCapacity(allocator, size) catch return false;

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

    if (idx != size) return false; // Not enough cells

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

    return solveImpl(&problem);
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

        // Warmup
        const warmup_iters: usize = 1000;
        var timer = try std.time.Timer.start();
        for (0..warmup_iters) |_| {
            _ = solve(tc.input);
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
            _ = solve(tc.input);
        }
        const bench_ns = timer.read();
        try stdout.print("  Bench:  {d:.3} ms/solve, {d} iters\n", .{
            @as(f64, @floatFromInt(bench_ns)) / @as(f64, @floatFromInt(bench_iters)) / 1_000_000.0,
            bench_iters,
        });

        // Verify it actually solves
        const result = solve(tc.input);
        try stdout.print("  Solved: {}\n\n", .{result});
    }
}

test "solve vanilla puzzle" {
    const input =
        \\[V]8x8-26-1388D
        \\........
        \\.....6..
        \\2.....4.
        \\2.2....1
        \\........
        \\........
        \\2?.42...
        \\0111?.3.
    ;
    try std.testing.expect(solve(input));
}

test "solve anti-triplet puzzle" {
    const input =
        \\[T]8x8-26-10817
        \\..3.....
        \\.......3
        \\........
        \\.4..3...
        \\.....5..
        \\........
        \\..3.....
        \\..2.....
    ;
    try std.testing.expect(solve(input));
}
