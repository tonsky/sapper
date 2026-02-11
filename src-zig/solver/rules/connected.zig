const core = @import("../core.zig");
const algorithm = @import("../algorithm.zig");

const Support = algorithm.Support;
const FLAG = core.FLAG;
const UNKNOWN = core.UNKNOWN;

// [C] All mines are orthogonally or diagonally connected
pub fn check(problem: *const core.Problem, support: *const Support) bool {
    // Flagging a cell doesn't change connected property
    if (support.last_checked_open_idx == support.open_indices_len) return true;

    const field = &support.field;
    const w = problem.w;
    const size = w * problem.h;
    const passable_mask: u8 = FLAG | UNKNOWN;

    // Build bitmask of FLAG cells, UNKNOWN cells, and passable (FLAG|UNKNOWN) cells
    var flags: u64 = 0;
    var passable: u64 = 0;
    for (0..size) |i| {
        const bit = @as(u64, 1) << @intCast(i);
        if (field[i] & passable_mask != 0) {
            passable |= bit;
        }
        if (field[i] == FLAG) {
            flags |= bit;
        }
    }

    if (flags == 0) return true;

    // Flood fill from lowest FLAG bit, moving through passable cells
    var visited: u64 = flags & (~flags +% 1); // isolate lowest flag bit
    while (true) {
        // Expand all visited cells' neighbors at once
        var expanded: u64 = 0;
        var remaining = visited;
        while (remaining != 0) {
            const bit: u6 = @intCast(@ctz(remaining));
            expanded |= support.neighbor_masks[bit];
            remaining &= remaining - 1; // clear lowest bit
        }
        expanded &= passable & ~visited;
        if (expanded == 0) break;
        visited |= expanded;
    }

    // Can't reach all flags from flags
    if (!(flags & visited == flags)) return false;

    // For each unsolved known cell, check it has enough visited neighbors
    // to potentially satisfy its value
    for (support.knownSlice()) |ki| {
        if (support.flags_around[ki] >= support.field[ki]) continue;
        const nbrs = support.neighbor_masks[ki] & visited;
        const visited_count = @popCount(nbrs);
        if (visited_count < field[ki]) return false;
    }

    return true;
}
