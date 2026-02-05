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

// Maximum supported grid size
const MAX_SIZE: usize = 64;

const Context = struct {
    w: usize,
    h: usize,
    total_flags: usize,
    size: usize,
    use_anti_triplet: bool,
    allocator: std.mem.Allocator,
};

const Problem = struct {
    field: []u8,
    unknown: usize,
    flag_indices: std.ArrayList(u8),
    ctx: *const Context,

    fn flagged(self: *const Problem) usize {
        return self.flag_indices.items.len;
    }

    fn clone(self: *const Problem) !Problem {
        return Problem{
            .field = try self.ctx.allocator.dupe(u8, self.field),
            .unknown = self.unknown,
            .flag_indices = try self.flag_indices.clone(self.ctx.allocator),
            .ctx = self.ctx,
        };
    }

    fn deinit(self: *Problem) void {
        self.ctx.allocator.free(self.field);
        self.flag_indices.deinit(self.ctx.allocator);
    }

    fn withVal(self: *Problem, i: usize, val: u8) void {
        self.field[i] = val;
        if (val == FLAG) {
            self.flag_indices.appendAssumeCapacity(@intCast(i));
        }
        self.unknown -= 1;
    }
};

fn countNeighbors(field: []const u8, ctx: *const Context, idx: usize, val: u8) u8 {
    const x = idx % ctx.w;
    const y = idx / ctx.w;
    var res: u8 = 0;

    const x_start: usize = if (x > 0) x - 1 else 0;
    const x_end: usize = if (x + 1 < ctx.w) x + 2 else ctx.w;
    const y_start: usize = if (y > 0) y - 1 else 0;
    const y_end: usize = if (y + 1 < ctx.h) y + 2 else ctx.h;

    for (y_start..y_end) |ny| {
        for (x_start..x_end) |nx| {
            if (nx == x and ny == y) continue;
            if (field[ny * ctx.w + nx] == val) res += 1;
        }
    }
    return res;
}

// [*] Count amount of mines
fn totalCheck(problem: *const Problem) bool {
    return problem.flagged() <= problem.ctx.total_flags and
        problem.flagged() + problem.unknown >= problem.ctx.total_flags;
}

// [V] Nothing special - check each numbered cell
fn vanillaCheck(problem: *const Problem, known: []const u8) bool {
    for (known) |i| {
        const value = problem.field[i];
        const fs = countNeighbors(problem.field, problem.ctx, i, FLAG);
        const unk = countNeighbors(problem.field, problem.ctx, i, UNKNOWN);
        if (fs > value) return false;
        if (fs + unk < value) return false;
    }
    return true;
}

// [T] Flags may not form row of three orthogonally or diagonally
fn antiTripletCheck(problem: *const Problem) bool {
    const ctx = problem.ctx;
    const flag_indices = problem.flag_indices.items;
    for (flag_indices) |idx| {
        const y = idx / ctx.w;
        const x = idx % ctx.w;

        // FFF horizontal
        if (x + 2 < ctx.w) {
            if (problem.field[idx + 1] == FLAG and problem.field[idx + 2] == FLAG) {
                return false;
            }
        }

        // FFF vertical
        if (y + 2 < ctx.h) {
            if (problem.field[idx + ctx.w] == FLAG and problem.field[idx + 2 * ctx.w] == FLAG) {
                return false;
            }
        }

        // Diagonal down-right
        if (x + 2 < ctx.w and y + 2 < ctx.h) {
            if (problem.field[idx + ctx.w + 1] == FLAG and problem.field[idx + 2 * ctx.w + 2] == FLAG) {
                return false;
            }
        }

        // Diagonal down-left
        if (x >= 2 and y + 2 < ctx.h) {
            if (problem.field[idx + ctx.w - 1] == FLAG and problem.field[idx + 2 * ctx.w - 2] == FLAG) {
                return false;
            }
        }
    }
    return true;
}

fn checkConstraints(problem: *const Problem, known: []const u8) bool {
    if (!totalCheck(problem)) return false;
    if (!vanillaCheck(problem, known)) return false;
    if (problem.ctx.use_anti_triplet and !antiTripletCheck(problem)) return false;
    return true;
}

fn autoOpen(problem: *Problem, known: []const u8) bool {
    const ctx = problem.ctx;
    var changed = false;
    var known_idx: usize = 0;
    while (known_idx < known.len) {
        const i = known[known_idx];
        const value = problem.field[i];
        const unk = countNeighbors(problem.field, ctx, i, UNKNOWN);

        if (unk == 0) {
            known_idx += 1;
            continue;
        }

        const fs = countNeighbors(problem.field, ctx, i, FLAG);

        // All flagged, can open the rest
        if (fs == value) {
            const x = i % ctx.w;
            const y = i / ctx.w;
            const x_start: usize = if (x > 0) x - 1 else 0;
            const x_end: usize = if (x + 1 < ctx.w) x + 2 else ctx.w;
            const y_start: usize = if (y > 0) y - 1 else 0;
            const y_end: usize = if (y + 1 < ctx.h) y + 2 else ctx.h;

            for (y_start..y_end) |ny| {
                for (x_start..x_end) |nx| {
                    const nbi = ny * ctx.w + nx;
                    if (problem.field[nbi] == UNKNOWN) {
                        problem.withVal(nbi, OPEN);
                    }
                }
            }
            known_idx = 0;
            changed = true;
            continue;
        }

        // Can flag the rest
        if (value - fs == unk) {
            const x = i % ctx.w;
            const y = i / ctx.w;
            const x_start: usize = if (x > 0) x - 1 else 0;
            const x_end: usize = if (x + 1 < ctx.w) x + 2 else ctx.w;
            const y_start: usize = if (y > 0) y - 1 else 0;
            const y_end: usize = if (y + 1 < ctx.h) y + 2 else ctx.h;

            for (y_start..y_end) |ny| {
                for (x_start..x_end) |nx| {
                    const nbi = ny * ctx.w + nx;
                    if (problem.field[nbi] == UNKNOWN) {
                        problem.withVal(nbi, FLAG);
                    }
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
    const ctx = problem.ctx;

    // Can open the rest
    if (problem.flagged() == ctx.total_flags) {
        for (0..ctx.size) |i| {
            if (problem.field[i] == UNKNOWN) {
                problem.withVal(i, OPEN);
            }
        }
        return true;
    }

    // Can flag the rest
    if (ctx.total_flags - problem.flagged() == problem.unknown) {
        for (0..ctx.size) |i| {
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
    const ctx = problem.ctx;

    for (known) |i| {
        const value = problem.field[i];
        const flags = countNeighbors(problem.field, ctx, i, FLAG);
        const rating = value - flags;

        if (rating < min_rating) {
            // Find first unknown neighbor
            const x = i % ctx.w;
            const y = i / ctx.w;
            const x_start: usize = if (x > 0) x - 1 else 0;
            const x_end: usize = if (x + 1 < ctx.w) x + 2 else ctx.w;
            const y_start: usize = if (y > 0) y - 1 else 0;
            const y_end: usize = if (y + 1 < ctx.h) y + 2 else ctx.h;

            outer: for (y_start..y_end) |ny| {
                for (x_start..x_end) |nx| {
                    const nbi = ny * ctx.w + nx;
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

    // Fallback: first unknown in candidates
    for (candidates) |c| {
        if (problem.field[c] == UNKNOWN) {
            return c;
        }
    }

    return null;
}

fn solveImpl(problem: *Problem, known: []const u8, candidates: []const u8) bool {
    // Check constraints
    if (!checkConstraints(problem, known)) {
        return false;
    }

    // Leaf - all explored
    if (problem.unknown == 0) {
        return true;
    }

    // Try auto-open
    if (autoOpen(problem, known)) {
        return solveImpl(problem, known, candidates);
    }

    // Try auto-finish
    if (autoFinish(problem)) {
        return solveImpl(problem, known, candidates);
    }

    // Find best candidate to try
    const candidate = bestCandidate(problem, known, candidates) orelse return false;

    // Try FLAG first
    var p1 = problem.clone() catch return false;
    defer p1.deinit();
    p1.withVal(candidate, FLAG);
    if (solveImpl(&p1, known, candidates)) {
        return true;
    }

    // Try OPEN
    var p2 = problem.clone() catch return false;
    defer p2.deinit();
    p2.withVal(candidate, OPEN);
    if (solveImpl(&p2, known, candidates)) {
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
    var buffer: [64 * 1024]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator = fba.allocator();

    var pos: usize = 0;

    // Parse rule indicators [T], [V], [T*], etc.
    var use_anti_triplet = false;
    while (pos < input.len and input[pos] == '[') {
        pos += 1; // skip '['
        const rule_start = pos;
        while (pos < input.len and input[pos] != ']') {
            pos += 1;
        }
        const rule = input[rule_start..pos];
        if (std.mem.eql(u8, rule, "T")) {
            use_anti_triplet = true;
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

    const size = w * h;
    if (size > MAX_SIZE) return false;

    var ctx = Context{
        .w = w,
        .h = h,
        .total_flags = total_flags,
        .size = size,
        .use_anti_triplet = use_anti_triplet,
        .allocator = allocator,
    };

    // Allocate field
    const field = allocator.alloc(u8, size) catch return false;
    var unknown: usize = 0;
    var flag_indices = std.ArrayList(u8).initCapacity(allocator, total_flags + 10) catch return false;

    var known_arr: [MAX_SIZE]u8 = undefined;
    var known_count: usize = 0;
    var candidates_arr: [MAX_SIZE]u8 = undefined;
    var candidates_count: usize = 0;

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
        pos += 1;
    }

    if (idx != size) return false; // Not enough cells

    var problem = Problem{
        .field = field,
        .unknown = unknown,
        .flag_indices = flag_indices,
        .ctx = &ctx,
    };

    return solveImpl(&problem, known_arr[0..known_count], candidates_arr[0..candidates_count]);
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
