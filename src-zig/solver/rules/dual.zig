const core = @import("../core.zig");
const algorithm = @import("../algorithm.zig");

const Support = algorithm.Support;
const FLAG = core.FLAG;
const UNKNOWN = core.UNKNOWN;
const RESERVED = core.RESERVED;

// [D] Dual: all mines must form 1x2 or 2x1 blocks
// Blocks do not touch each other orthogonally; diagonally is allowed

pub fn check(problem: *const core.Problem, support: *const Support) bool {
    const w: usize = @intCast(problem.w);
    const h: usize = @intCast(problem.h);
    const field = &support.field;

    for (support.flagSlice()) |idx| {
        const x: usize = @intCast(idx % w);
        const y: usize = @intCast(idx / w);

        var flags: usize = 0;
        var unknowns: usize = 0;

        const l = if (x > 0)     field[x - 1 + y * w] else RESERVED;
        const r = if (x < w - 1) field[x + 1 + y * w] else RESERVED;
        const t = if (y > 0)     field[x + (y - 1) * w] else RESERVED;
        const b = if (y < h - 1) field[x + (y + 1) * w] else RESERVED;

        if (l == FLAG) flags += 1 else if (l == UNKNOWN) unknowns += 1;
        if (r == FLAG) flags += 1 else if (r == UNKNOWN) unknowns += 1;
        if (t == FLAG) flags += 1 else if (t == UNKNOWN) unknowns += 1;
        if (b == FLAG) flags += 1 else if (b == UNKNOWN) unknowns += 1;

        if (flags > 1 or flags + unknowns == 0) return false;
    }
    return true;
}

pub fn bestCandidate(problem: *const core.Problem, support: *const Support, min_rating: *f64) ?usize {
    const w: usize = @intCast(problem.w);
    const h: usize = @intCast(problem.h);
    const field = &support.field;

    for (support.flagSlice()) |idx| {
        const x: usize = @intCast(idx % w);
        const y: usize = @intCast(idx / w);

        var flags: usize = 0;
        var unknown: ?usize = null;

        const l = if (x > 0)     field[x - 1 + y * w] else RESERVED;
        const r = if (x < w - 1) field[x + 1 + y * w] else RESERVED;
        const t = if (y > 0)     field[x + (y - 1) * w] else RESERVED;
        const b = if (y < h - 1) field[x + (y + 1) * w] else RESERVED;

        if (l == FLAG) flags += 1 else if (l == UNKNOWN) unknown = x - 1 + y * w;
        if (r == FLAG) flags += 1 else if (r == UNKNOWN) unknown = x + 1 + y * w;
        if (t == FLAG) flags += 1 else if (t == UNKNOWN) unknown = x + (y - 1) * w;
        if (b == FLAG) flags += 1 else if (b == UNKNOWN) unknown = x + (y + 1) * w;

        if (flags >= 1) continue;

        if (unknown) |i| {
            min_rating.* = 1;
            return i;
        }
    }

    return null;
}
