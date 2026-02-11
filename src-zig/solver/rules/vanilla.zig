const std = @import("std");
const core = @import("../core.zig");
const algorithm = @import("../algorithm.zig");

const Support = algorithm.Support;
const UNKNOWN = core.UNKNOWN;

// [V] Nothing special - check each numbered cell
pub fn check(support: *const Support) bool {
    for (support.knownSlice()) |i| {
        const rel = @as(i64, support.field[i]) - @as(i64, support.flags_around[i]);
        const unk = support.unknowns_around[i];
        if (rel < 0) return false;
        if (unk < rel) return false;
    }
    return true;
}

// [V] pick a cell that has least to open
pub fn bestCandidate(support: *const Support, min_rating: *f64) ?usize {
    var best: ?usize = null;
    for (support.knownSlice()) |i| {
        const rel = @as(i64, support.field[i]) - support.flags_around[i];
        const unknowns = support.unknowns_around[i];
        const rating = @as(f64, @floatFromInt(unknowns - rel));
        if (unknowns > 0 and rating < min_rating.*) {
            for (support.neighbors(i)) |ni| {
                if (support.field[ni] == UNKNOWN) {
                    min_rating.* = rating;
                    best = @intCast(ni);
                    break;
                }
            }
            if (min_rating.* <= 1) return best;
        }
    }
    return best;
}
