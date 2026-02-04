//! Minesweeper constraint solver ported from ClojureScript
//!
//! Implements three constraint checks:
//! - total-check: Ensures flag count stays within bounds
//! - vanilla-check: Ensures numbered cells have valid neighbor counts
//! - anti-triplet-check: Ensures no three flags in a row (horizontal, vertical, diagonal)
//!
//! Hardcoded for 8x8 grid with 26 total flags.
//!
//! Build and run benchmark:
//!   zig build-exe solver.zig -O ReleaseFast && ./solver
//!
//! Run tests:
//!   zig test solver.zig
//!
//! Input format: "..1?F..\n..." where:
//!   . = unknown cell
//!   F = flagged cell (mine)
//!   ? = confirmed safe (opened) cell
//!   0-8 = numbered cell showing adjacent mine count

const std = @import("std");

// Field cell values
const FLAG: u8 = 9;
const OPEN: u8 = 10;
const UNKNOWN: u8 = 11;

// Hardcoded dimensions
const W: usize = 8;
const H: usize = 8;
const TOTAL_FLAGS: usize = 26;
const SIZE: usize = W * H;

// Pre-computed neighbors for each cell
const neighbours: [SIZE][8]i8 = blk: {
    var result: [SIZE][8]i8 = undefined;
    for (0..H) |y| {
        for (0..W) |x| {
            const i = y * W + x;
            var nbs: [8]i8 = .{ -1, -1, -1, -1, -1, -1, -1, -1 };
            var nb_count: usize = 0;
            for ([_]i8{ -1, 0, 1 }) |dy| {
                for ([_]i8{ -1, 0, 1 }) |dx| {
                    if (dx == 0 and dy == 0) continue;
                    const nx: i8 = @as(i8, @intCast(x)) + dx;
                    const ny: i8 = @as(i8, @intCast(y)) + dy;
                    if (nx >= 0 and ny >= 0 and nx < W and ny < H) {
                        nbs[nb_count] = @intCast(@as(i8, @intCast(ny)) * W + nx);
                        nb_count += 1;
                    }
                }
            }
            result[i] = nbs;
        }
    }
    break :blk result;
};

const Problem = struct {
    field: [SIZE]u8,
    flagged: usize,
    unknown: usize,
    flag_indices: std.BoundedArray(u8, TOTAL_FLAGS + 10),

    fn clone(self: *const Problem) Problem {
        return Problem{
            .field = self.field,
            .flagged = self.flagged,
            .unknown = self.unknown,
            .flag_indices = self.flag_indices,
        };
    }

    fn withVal(self: *Problem, i: usize, val: u8) void {
        self.field[i] = val;
        if (val == FLAG) {
            self.flagged += 1;
            self.flag_indices.appendAssumeCapacity(@intCast(i));
        }
        self.unknown -= 1;
    }
};

fn countVal(field: *const [SIZE]u8, nbs: *const [8]i8, val: u8) u8 {
    var res: u8 = 0;
    for (nbs) |nb| {
        if (nb < 0) break;
        if (field[@intCast(nb)] == val) res += 1;
    }
    return res;
}

// [*] Count amount of mines
fn totalCheck(problem: *const Problem) bool {
    return problem.flagged <= TOTAL_FLAGS and
        problem.flagged + problem.unknown >= TOTAL_FLAGS;
}

// [V] Nothing special - check each numbered cell
fn vanillaCheck(problem: *const Problem, known: []const u8) bool {
    for (known) |i| {
        const value = problem.field[i];
        const nbs = &neighbours[i];
        const fs = countVal(&problem.field, nbs, FLAG);
        const unk = countVal(&problem.field, nbs, UNKNOWN);
        if (fs > value) return false;
        if (fs + unk < value) return false;
    }
    return true;
}

// [T] Flags may not form row of three orthogonally or diagonally
fn antiTripletCheck(problem: *const Problem) bool {
    const flag_indices = problem.flag_indices.constSlice();
    for (flag_indices) |idx| {
        const y = idx / W;
        const x = idx % W;

        // FFF
        // ...
        // ...
        if (x + 2 < W) {
            if (problem.field[idx + 1] == FLAG and problem.field[idx + 2] == FLAG) {
                return false;
            }
        }

        // F..
        // F..
        // F..
        if (y + 2 < H) {
            if (problem.field[idx + W] == FLAG and problem.field[idx + 2 * W] == FLAG) {
                return false;
            }
        }

        // F..
        // .F.
        // ..F
        if (x + 2 < W and y + 2 < H) {
            if (problem.field[idx + W + 1] == FLAG and problem.field[idx + 2 * W + 2] == FLAG) {
                return false;
            }
        }

        // ..F
        // .F.
        // F..
        if (x >= 2 and y + 2 < H) {
            if (problem.field[idx + W - 1] == FLAG and problem.field[idx + 2 * W - 2] == FLAG) {
                return false;
            }
        }
    }
    return true;
}

pub const RuleSet = struct {
    use_anti_triplet: bool = false,
};

fn checkConstraints(problem: *const Problem, known: []const u8, rules: RuleSet) bool {
    if (!totalCheck(problem)) return false;
    if (!vanillaCheck(problem, known)) return false;
    if (rules.use_anti_triplet and !antiTripletCheck(problem)) return false;
    return true;
}

fn autoOpen(problem: *Problem, known: []const u8) bool {
    var changed = false;
    var known_idx: usize = 0;
    while (known_idx < known.len) {
        const i = known[known_idx];
        const value = problem.field[i];
        const nbs = &neighbours[i];
        const unk = countVal(&problem.field, nbs, UNKNOWN);

        if (unk == 0) {
            known_idx += 1;
            continue;
        }

        const fs = countVal(&problem.field, nbs, FLAG);

        // All flagged, can open the rest
        if (fs == value) {
            for (nbs) |nb| {
                if (nb < 0) break;
                const nbi: usize = @intCast(nb);
                if (problem.field[nbi] == UNKNOWN) {
                    problem.withVal(nbi, OPEN);
                }
            }
            known_idx = 0;
            changed = true;
            continue;
        }

        // Can flag the rest
        if (value - fs == unk) {
            for (nbs) |nb| {
                if (nb < 0) break;
                const nbi: usize = @intCast(nb);
                if (problem.field[nbi] == UNKNOWN) {
                    problem.withVal(nbi, FLAG);
                }
            }
            known_idx = 0;
            changed = true;
            continue;
        }

        known_idx += 1;
    }
    return changed;
}

fn autoFinish(problem: *Problem) bool {
    // Can open the rest
    if (problem.flagged == TOTAL_FLAGS) {
        for (0..SIZE) |i| {
            if (problem.field[i] == UNKNOWN) {
                problem.withVal(i, OPEN);
            }
        }
        return true;
    }

    // Can flag the rest
    if (TOTAL_FLAGS - problem.flagged == problem.unknown) {
        for (0..SIZE) |i| {
            if (problem.field[i] == UNKNOWN) {
                problem.withVal(i, FLAG);
            }
        }
        return true;
    }

    return false;
}

fn bestCandidate(problem: *const Problem, known: []const u8, candidates: []const u8) ?u8 {
    var min_rating: u8 = 255;
    var min_index: ?u8 = null;

    for (known) |i| {
        const nbs = &neighbours[i];
        const value = problem.field[i];
        const flags = countVal(&problem.field, nbs, FLAG);
        const rating = value - flags;

        if (rating < min_rating) {
            // Find first unknown neighbor
            for (nbs) |nb| {
                if (nb < 0) break;
                const nbi: usize = @intCast(nb);
                if (problem.field[nbi] == UNKNOWN) {
                    min_rating = rating;
                    min_index = @intCast(nbi);
                    break;
                }
            }
        }
    }

    if (min_index) |idx| {
        return idx;
    }

    // Fallback: first unknown in candidates
    for (candidates) |c| {
        if (problem.field[c] == UNKNOWN) {
            return c;
        }
    }

    return null;
}

fn solveImpl(problem: *Problem, known: []const u8, candidates: []const u8, rules: RuleSet) bool {
    // Check constraints
    if (!checkConstraints(problem, known, rules)) {
        return false;
    }

    // Leaf - all explored
    if (problem.unknown == 0) {
        return true;
    }

    // Try auto-open
    if (autoOpen(problem, known)) {
        return solveImpl(problem, known, candidates, rules);
    }

    // Try auto-finish
    if (autoFinish(problem)) {
        return solveImpl(problem, known, candidates, rules);
    }

    // Find best candidate to try
    const candidate = bestCandidate(problem, known, candidates) orelse return false;

    // Try FLAG first
    var p1 = problem.clone();
    p1.withVal(candidate, FLAG);
    if (solveImpl(&p1, known, candidates, rules)) {
        problem.* = p1;
        return true;
    }

    // Try OPEN
    var p2 = problem.clone();
    p2.withVal(candidate, OPEN);
    if (solveImpl(&p2, known, candidates, rules)) {
        problem.* = p2;
        return true;
    }

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

/// Parse problem string and solve it
/// Input format: "..1?F..\n..." where . = unknown, F = flag, ? = open, 0-8 = numbered
/// Returns true if solution found, false otherwise
pub fn solve(input: []const u8) bool {
    return solveWithRules(input, .{});
}

/// Solve with anti-triplet rule enabled
pub fn solveAntiTriplet(input: []const u8) bool {
    return solveWithRules(input, .{ .use_anti_triplet = true });
}

pub fn solveWithRules(input: []const u8, rules: RuleSet) bool {
    var field: [SIZE]u8 = undefined;
    var flagged: usize = 0;
    var unknown: usize = 0;
    var flag_indices = std.BoundedArray(u8, TOTAL_FLAGS + 10){};
    var known_arr: [SIZE]u8 = undefined;
    var known_count: usize = 0;
    var candidates_arr: [SIZE]u8 = undefined;
    var candidates_count: usize = 0;

    var idx: usize = 0;
    for (input) |ch| {
        if (ch == '\n' or ch == '\r' or ch == ' ' or ch == '\t') continue;
        if (idx >= SIZE) break;

        const val = parseCell(ch);
        field[idx] = val;

        if (val == FLAG) {
            flagged += 1;
            flag_indices.appendAssumeCapacity(@intCast(idx));
        } else if (val == UNKNOWN) {
            unknown += 1;
            candidates_arr[candidates_count] = @intCast(idx);
            candidates_count += 1;
        }

        if (val <= 8) {
            known_arr[known_count] = @intCast(idx);
            known_count += 1;
        }

        idx += 1;
    }

    var problem = Problem{
        .field = field,
        .flagged = flagged,
        .unknown = unknown,
        .flag_indices = flag_indices,
    };

    return solveImpl(&problem, known_arr[0..known_count], candidates_arr[0..candidates_count], rules);
}

// Benchmark
pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    const test_cases = [_]struct {
        name: []const u8,
        input: []const u8,
        use_anti_triplet: bool,
    }{
        .{
            .name = "[V]8x8-26-1388D",
            .use_anti_triplet = false,
            .input =
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
            .use_anti_triplet = true,
            .input =
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

        const rules = RuleSet{ .use_anti_triplet = tc.use_anti_triplet };

        // Warmup
        const warmup_iters: usize = 100;
        var timer = try std.time.Timer.start();
        for (0..warmup_iters) |_| {
            _ = solveWithRules(tc.input, rules);
        }
        const warmup_ns = timer.read();
        try stdout.print("  Warmup: {d:.3} ms/solve, {d} iters\n", .{
            @as(f64, @floatFromInt(warmup_ns)) / @as(f64, @floatFromInt(warmup_iters)) / 1_000_000.0,
            warmup_iters,
        });

        // Benchmark
        const bench_iters: usize = 1000;
        timer.reset();
        for (0..bench_iters) |_| {
            _ = solveWithRules(tc.input, rules);
        }
        const bench_ns = timer.read();
        try stdout.print("  Bench:  {d:.3} ms/solve, {d} iters\n", .{
            @as(f64, @floatFromInt(bench_ns)) / @as(f64, @floatFromInt(bench_iters)) / 1_000_000.0,
            bench_iters,
        });

        // Verify it actually solves
        const result = solveWithRules(tc.input, rules);
        try stdout.print("  Solved: {}\n\n", .{result});
    }
}

test "solve vanilla puzzle" {
    const input =
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
        \\..3.....
        \\.......3
        \\........
        \\.4..3...
        \\.....5..
        \\........
        \\..3.....
        \\..2.....
    ;
    try std.testing.expect(solveAntiTriplet(input));
}
