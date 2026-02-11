const std = @import("std");
const core = @import("core.zig");
const printer = @import("printer.zig");
const vanilla = @import("rules/vanilla.zig");
const quad = @import("rules/quad.zig");
const no_triplet = @import("rules/no_triplet.zig");
const connected = @import("rules/connected.zig");

const FLAG = core.FLAG;
const OPEN = core.OPEN;
const UNKNOWN = core.UNKNOWN;

pub const Support = struct {
    field: []u8,
    neighbor_indices: [64*9]u8,
    neighbor_masks: [64]u64,
    known_indices: []const u8,
    unknown_count: usize,
    flag_indices: std.ArrayList(u8),
    last_checked_flag_idx: usize,
    open_indices: std.ArrayList(u8),
    last_checked_open_idx: usize,
    flags_around: [64]i8,
    unknowns_around: [64]i8,

    pub fn neighbors(self: *const Support, i: usize) []const u8 {
        const start = i * 9 + 1;
        const len = self.neighbor_indices[i * 9];
        return self.neighbor_indices[start..start + len];
    }

    fn setFlag(self: *Support, i: usize) void {
        std.debug.assert(self.field[i] == UNKNOWN);
        self.field[i] = FLAG;
        self.flag_indices.appendAssumeCapacity(@intCast(i));
        self.unknown_count -= 1;
        for (self.neighbors(i)) |ni| {
            self.flags_around[ni] += 1;
            self.unknowns_around[ni] -= 1;
        }
    }

    fn setOpen(self: *Support, i: usize) void {
        std.debug.assert(self.field[i] == UNKNOWN);
        self.field[i] = OPEN;
        self.open_indices.appendAssumeCapacity(@intCast(i));
        self.unknown_count -= 1;
        for (self.neighbors(i)) |ni| {
            self.unknowns_around[ni] -= 1;
        }
    }

    fn setUnknown(self: *Support, i: usize) void {
        if (self.field[i] == FLAG) {
            const popped_flag = self.flag_indices.pop();
            std.debug.assert(popped_flag == @as(u8, @intCast(i)));
            for (self.neighbors(i)) |ni| {
                self.flags_around[ni] -= 1;
                self.unknowns_around[ni] += 1;
            }
        } else if (self.field[i] == OPEN) {
            const popped_open = self.open_indices.pop();
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
    const flagged = support.flag_indices.items.len;
    return flagged <= problem.total_flags and
        flagged + support.unknown_count >= problem.total_flags;
}

pub fn beginSolving(problem: *const core.Problem, support: *Support) bool {
    printer.maybePrint(problem, support.field);
    return autoOpen(problem, support);
}

fn autoOpen(problem: *const core.Problem, support: *Support) bool {
    const flag_indices_len_before = support.flag_indices.items.len;
    const open_indices_len_before = support.open_indices.items.len;

    for (support.known_indices) |i| {
        const unk = support.unknowns_around[i];
        if (unk == 0) continue;

        const rel = @as(i64, support.field[i]) - @as(i64, support.flags_around[i]);
        if (rel == 0) {
            for (support.neighbors(i)) |ni| {
                if (support.field[ni] == UNKNOWN) {
                    support.setOpen(ni);
                }
            }
            continue;
        }

        if (rel == unk) {
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
    while (support.flag_indices.items.len > flag_indices_len_before) {
        const idx = support.flag_indices.items[support.flag_indices.items.len - 1];
        support.setUnknown(idx);
    }
    while (support.open_indices.items.len > open_indices_len_before) {
        const idx = support.open_indices.items[support.open_indices.items.len - 1];
        support.setUnknown(idx);
    }

    return false;
}

fn autoFinish(problem: *const core.Problem, support: *Support) bool {
    const flag_indices_len_before = support.flag_indices.items.len;
    const open_indices_len_before = support.open_indices.items.len;
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
    while (support.flag_indices.items.len > flag_indices_len_before) {
        const idx = support.flag_indices.items[support.flag_indices.items.len - 1];
        support.setUnknown(idx);
    }
    while (support.open_indices.items.len > open_indices_len_before) {
        const idx = support.open_indices.items[support.open_indices.items.len - 1];
        support.setUnknown(idx);
    }

    return false;
}

fn checkConstraints(problem: *const core.Problem, support: *Support) bool {
    if (problem.rules.total and !totalCheck(problem, support)) return false;
    if (problem.rules.no_triplet and !no_triplet.check(problem, support)) return false;
    if (problem.rules.quad and !quad.check(problem, support)) return false;
    if (problem.rules.connected and !connected.check(problem, support)) return false;
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

    if (problem.rules.vanilla) {
        if (vanilla.bestCandidate(support, &min_rating)) |idx| {
            min_index = idx;
        }
    }

    if (min_index) |idx| {
        return idx;
    }

    // Fallback: first unknown
    for (support.field[0..], 0..) |value, i| {
        if (value == UNKNOWN) {
            return i;
        }
    }

    return null;
}

fn diveDeeper(problem: *const core.Problem, support: *Support) bool {
    const candidate = bestCandidate(problem, support) orelse return false;

    const last_checked_flag_idx = support.flag_indices.items.len;
    const last_checked_open_idx = support.open_indices.items.len;

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

fn buildSupport(problem: *const core.Problem, allocator: std.mem.Allocator) ?Support {
    const w = problem.w;
    const h = problem.h;
    const size = w * h;
    var known_indices = std.ArrayList(u8).initCapacity(allocator, size) catch return null;

    const field = allocator.alloc(u8, size) catch return null;
    var unknown_count: usize = 0;

    for (0..size) |idx| {
        const val = problem.field[idx];
        if (val <= 8) {
            field[idx] = val;
            known_indices.appendAssumeCapacity(@intCast(idx));
        } else {
            field[idx] = UNKNOWN;
            unknown_count += 1;
        }
    }

    // Precompute neighbor bitmasks for connected check
    var neighbor_indices: [64 * 9]u8 = undefined;
    var neighbor_masks: [64]u64 = undefined;
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
                    neighbor_indices[i * 9 + 1 + len] = @intCast(ni);
                    len += 1;
                    mask |= @as(u64, 1) << @intCast(ni);
                }
            }
        }
        neighbor_indices[i * 9] = len;
        neighbor_masks[i] = mask;
    }

    var support = Support{
        .field = field,
        .neighbor_indices = neighbor_indices,
        .neighbor_masks = neighbor_masks,
        .known_indices = known_indices.items,
        .unknown_count = unknown_count,
        .flag_indices = std.ArrayList(u8).initCapacity(allocator, size) catch return null,
        .last_checked_flag_idx = 0,
        .open_indices = std.ArrayList(u8).initCapacity(allocator, size) catch return null,
        .last_checked_open_idx = 0,
        .flags_around = .{0} ** 64,
        .unknowns_around = .{0} ** 64,
    };

    for (0..size) |i| {
        support.unknowns_around[i] = core.countNeighbors(field, w, h, i, UNKNOWN);
    }

    // Seed open_indices with known cells (they are open by definition)
    for (known_indices.items) |ki| {
        support.open_indices.appendAssumeCapacity(ki);
    }

    // Apply initial flags and opens through setFlag/setOpen
    for (0..size) |idx| {
        const val = problem.field[idx];
        if (val == FLAG) support.setFlag(idx)
        else if (val == OPEN) support.setOpen(idx);
    }

    return support;
}

pub fn solve(problem: *const core.Problem, allocator: std.mem.Allocator) ?[]const u8 {
    var support = buildSupport(problem, allocator) orelse return null;
    if (!beginSolving(problem, &support)) return null;
    return support.field;
}
