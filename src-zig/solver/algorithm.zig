const std = @import("std");
const core = @import("core.zig");
const printer = @import("printer.zig");
const vanilla = @import("rules/vanilla.zig");
const quad = @import("rules/quad.zig");
const no_triplet = @import("rules/no_triplet.zig");
const triplet = @import("rules/triplet.zig");
const connected = @import("rules/connected.zig");
const dual = @import("rules/dual.zig");

const FLAG = core.FLAG;
const OPEN = core.OPEN;
const UNKNOWN = core.UNKNOWN;
const MAX_SIZE = core.MAX_SIZE;

pub const Support = struct {
    field: [MAX_SIZE]u8,
    neighbor_indices: [MAX_SIZE * 9]u8,
    neighbor_masks: [MAX_SIZE]u64,
    known_indices: [MAX_SIZE]u8,
    known_indices_len: usize,
    unknown_count: usize,
    flag_indices: [MAX_SIZE]u8,
    flag_indices_len: usize,
    last_checked_flag_idx: usize,
    open_indices: [MAX_SIZE]u8,
    open_indices_len: usize,
    last_checked_open_idx: usize,
    flags_around: [MAX_SIZE]i8,
    unknowns_around: [MAX_SIZE]i8,

    pub fn neighbors(self: *const Support, i: usize) []const u8 {
        const start = i * 9 + 1;
        const len = self.neighbor_indices[i * 9];
        return self.neighbor_indices[start .. start + len];
    }

    pub fn knownSlice(self: *const Support) []const u8 {
        return self.known_indices[0..self.known_indices_len];
    }

    pub fn flagSlice(self: *const Support) []const u8 {
        return self.flag_indices[0..self.flag_indices_len];
    }

    pub fn openSlice(self: *const Support) []const u8 {
        return self.open_indices[0..self.open_indices_len];
    }

    fn setFlag(self: *Support, i: usize) void {
        std.debug.assert(self.field[i] == UNKNOWN);
        self.field[i] = FLAG;
        self.flag_indices[self.flag_indices_len] = @intCast(i);
        self.flag_indices_len += 1;
        self.unknown_count -= 1;
        for (self.neighbors(i)) |ni| {
            self.flags_around[ni] += 1;
            self.unknowns_around[ni] -= 1;
        }
    }

    fn setOpen(self: *Support, i: usize) void {
        std.debug.assert(self.field[i] == UNKNOWN);
        self.field[i] = OPEN;
        self.open_indices[self.open_indices_len] = @intCast(i);
        self.open_indices_len += 1;
        self.unknown_count -= 1;
        for (self.neighbors(i)) |ni| {
            self.unknowns_around[ni] -= 1;
        }
    }

    fn setUnknown(self: *Support, i: usize) void {
        if (self.field[i] == FLAG) {
            self.flag_indices_len -= 1;
            const popped_flag = self.flag_indices[self.flag_indices_len];
            std.debug.assert(popped_flag == @as(u8, @intCast(i)));
            for (self.neighbors(i)) |ni| {
                self.flags_around[ni] -= 1;
                self.unknowns_around[ni] += 1;
            }
        } else if (self.field[i] == OPEN) {
            self.open_indices_len -= 1;
            const popped_open = self.open_indices[self.open_indices_len];
            std.debug.assert(popped_open == @as(u8, @intCast(i)));
            for (self.neighbors(i)) |ni| {
                self.unknowns_around[ni] += 1;
            }
        }
        self.field[i] = UNKNOWN;
        self.unknown_count += 1;
    }
};


// [*] Total amount of flags across the entire field
fn totalCheck(problem: *const core.Problem, support: *const Support) bool {
    const flagged = support.flag_indices_len;
    return flagged <= problem.total_flags and
        flagged + support.unknown_count >= problem.total_flags;
}

pub fn beginSolving(problem: *const core.Problem, support: *Support) bool {
    printer.maybePrint(problem, &support.field);
    return autoOpen(problem, support);
}

fn autoOpen(problem: *const core.Problem, support: *Support) bool {
    const flag_indices_len_before = support.flag_indices_len;
    const open_indices_len_before = support.open_indices_len;

    for (support.knownSlice()) |i| {
        const unk = support.unknowns_around[i];
        if (unk == 0) continue;

        const remaining = @as(i64, support.field[i]) - @as(i64, support.flags_around[i]);
        if (remaining == 0) {
            for (support.neighbors(i)) |ni| {
                if (support.field[ni] == UNKNOWN) {
                    support.setOpen(ni);
                }
            }
            continue;
        }

        if (remaining == unk) {
            for (support.neighbors(i)) |ni| {
                if (support.field[ni] == UNKNOWN) {
                    support.setFlag(ni);
                }
            }
        }
    }

    if (autoFinish(problem, support))
        return true;

    // Undo flags/opens
    while (support.flag_indices_len > flag_indices_len_before) {
        const idx = support.flag_indices[support.flag_indices_len - 1];
        support.setUnknown(idx);
    }
    while (support.open_indices_len > open_indices_len_before) {
        const idx = support.open_indices[support.open_indices_len - 1];
        support.setUnknown(idx);
    }

    return false;
}

fn autoFinish(problem: *const core.Problem, support: *Support) bool {
    const flag_indices_len_before = support.flag_indices_len;
    const open_indices_len_before = support.open_indices_len;
    const flagged = flag_indices_len_before;

    if (support.unknown_count > 0) {
        if (support.unknown_count + flagged <= problem.total_flags) {
            // Can flag the rest
            for (0..problem.w * problem.h) |i| {
                if (support.field[i] == UNKNOWN) support.setFlag(i);
            }
        } else if (flagged >= problem.total_flags) {
            // Can open the rest
            for (0..problem.w * problem.h) |i| {
                if (support.field[i] == UNKNOWN) support.setOpen(i);
            }
        }
    }

    if (checkConstraints(problem, support))
        return true;

    // Undo
    while (support.flag_indices_len > flag_indices_len_before) {
        const idx = support.flag_indices[support.flag_indices_len - 1];
        support.setUnknown(idx);
    }
    while (support.open_indices_len > open_indices_len_before) {
        const idx = support.open_indices[support.open_indices_len - 1];
        support.setUnknown(idx);
    }

    return false;
}

fn checkConstraints(problem: *const core.Problem, support: *Support) bool {
    if (problem.rules.total and !totalCheck(problem, support)) return false;
    if (problem.rules.no_triplet and !no_triplet.check(problem, support)) return false;
    if (problem.rules.triplet and !triplet.check(problem, support)) return false;
    if (problem.rules.quad and !quad.check(problem, support)) return false;
    if (problem.rules.connected and !connected.check(problem, support)) return false;
    if (problem.rules.dual and !dual.check(problem, support)) return false;
    if (problem.rules.vanilla and !vanilla.check(support)) return false;
    if (support.unknown_count == 0) return true;
    return diveDeeper(problem, support);
}

fn bestCandidate(problem: *const core.Problem, support: *const Support) ?usize {
    var min_rating: f64 = 1000;
    var min_index: ?usize = null;

    if (problem.rules.quad) {
        if (quad.bestCandidate(problem, support, &min_rating)) |idx| {
            min_index = idx;
            if (min_rating <= 1) return idx;
        }
    }

    if (problem.rules.dual) {
        if (dual.bestCandidate(problem, support, &min_rating)) |idx| {
            min_index = idx;
            if (min_rating <= 1) return idx;
        }
    }

    if (problem.rules.vanilla) {
        if (vanilla.bestCandidate(support, &min_rating)) |idx| {
            min_index = idx;
        }
    }

    if (min_index) |idx| {
        return idx;
    }

    // Fallback: first unknown
    const size = problem.w * problem.h;
    for (support.field[0..size], 0..) |value, i| {
        if (value == UNKNOWN) {
            return i;
        }
    }

    return null;
}

fn diveDeeper(problem: *const core.Problem, support: *Support) bool {
    const candidate = bestCandidate(problem, support) orelse return false;

    const last_checked_flag_idx = support.flag_indices_len;
    const last_checked_open_idx = support.open_indices_len;

    // Try FLAG first
    support.last_checked_flag_idx = last_checked_flag_idx;
    support.last_checked_open_idx = last_checked_open_idx;
    support.setFlag(candidate);
    if (beginSolving(problem, support)) return true;
    support.setUnknown(candidate);

    // Try OPEN
    support.last_checked_flag_idx = last_checked_flag_idx;
    support.last_checked_open_idx = last_checked_open_idx;
    support.setOpen(candidate);
    if (beginSolving(problem, support)) return true;
    support.setUnknown(candidate);

    return false;
}

pub fn buildSupport(problem: *const core.Problem) ?Support {
    const w = problem.w;
    const h = problem.h;
    const size = w * h;

    var support: Support = undefined;
    support.known_indices_len = 0;
    support.flag_indices_len = 0;
    support.open_indices_len = 0;
    support.unknown_count = 0;
    support.last_checked_flag_idx = 0;
    support.last_checked_open_idx = 0;

    for (0..size) |idx| {
        const val = problem.field[idx];
        if (val <= 8) {
            support.field[idx] = val;
            support.known_indices[support.known_indices_len] = @intCast(idx);
            support.known_indices_len += 1;
        } else {
            support.field[idx] = UNKNOWN;
            support.unknown_count += 1;
        }
    }

    // Precompute neighbor bitmasks for connected check
    for (0..size) |i| {
        const x = i % w;
        const y = i / w;
        var len: u8 = 0;
        var mask: u64 = 0;
        const x_start: usize = if (x > 0) x - 1 else 0;
        const x_end: usize = if (x + 1 < w) x + 2 else w;
        const y_start: usize = if (y > 0) y - 1 else 0;
        const y_end: usize = if (y + 1 < h) y + 2 else h;

        for (y_start..y_end) |ny| {
            for (x_start..x_end) |nx| {
                if (!(nx == x and ny == y)) {
                    const ni = ny * w + nx;
                    support.neighbor_indices[i * 9 + 1 + len] = @intCast(ni);
                    len += 1;
                    mask |= @as(u64, 1) << @intCast(ni);
                }
            }
        }
        support.neighbor_indices[i * 9] = len;
        support.neighbor_masks[i] = mask;
    }

    @memset(support.flags_around[0..size], 0);
    @memset(support.unknowns_around[0..size], 0);

    for (0..size) |i| {
        support.unknowns_around[i] = core.countNeighbors(&support.field, w, h, i, UNKNOWN);
    }

    // Seed open_indices with known cells (they are open by definition)
    for (support.known_indices[0..support.known_indices_len]) |ki| {
        support.open_indices[support.open_indices_len] = ki;
        support.open_indices_len += 1;
    }

    // Apply initial flags and opens through setFlag/setOpen
    for (0..size) |idx| {
        const val = problem.field[idx];
        if (val == FLAG) support.setFlag(idx)
        else if (val == OPEN) support.setOpen(idx);
    }

    return support;
}

pub fn solve(problem: *const core.Problem) ?[MAX_SIZE]u8 {
    var support = buildSupport(problem) orelse return null;
    if (!beginSolving(problem, &support)) return null;
    return support.field;
}

pub fn hint(problem: *core.Problem) [MAX_SIZE]u8 {
    const size = problem.w * problem.h;

    var result: [MAX_SIZE]u8 = undefined;
    @memcpy(result[0..size], problem.field[0..size]);

    for (0..size) |i| {
        if (problem.field[i] != UNKNOWN) continue;

        // Test: what if this cell is a flag?
        problem.field[i] = FLAG;
        const is_flag_possible = solve(problem) != null;
        problem.field[i] = UNKNOWN;

        if (!is_flag_possible) {
            result[i] = core.SAFE;
            continue;
        }

        // Test: what if this cell is open?
        problem.field[i] = OPEN;
        const is_open_possible = solve(problem) != null;
        problem.field[i] = UNKNOWN;

        if (!is_open_possible) {
            result[i] = core.DANGER;
        }
    }

    return result;
}
