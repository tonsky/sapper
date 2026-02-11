const core = @import("../core.zig");
const algorithm = @import("../algorithm.zig");

const Support = algorithm.Support;
const FLAG = core.FLAG;
const UNKNOWN = core.UNKNOWN;

// [Q] Every 2x2 area must contain at least one flag
pub fn check(problem: *const core.Problem, support: *const Support) bool {
    const mask: u8 = FLAG | UNKNOWN;
    const w = problem.w;
    for (support.openSlice()[support.last_checked_open_idx..]) |open_idx| {
        const open_y = open_idx / w;
        const open_x = open_idx % w;

        const x_start: usize = if (open_x > 0) open_x - 1 else 0;
        const x_end: usize = if (open_x < w - 1) open_x else open_x - 1;
        const y_start: usize = if (open_y > 0) open_y - 1 else 0;
        const y_end: usize = if (open_y < problem.h - 1) open_y else open_y - 1;

        for (y_start..y_end + 1) |y| {
            for (x_start..x_end + 1) |x| {
                if (support.field[y * w + x] & mask != 0) continue;
                if (support.field[y * w + x + 1] & mask != 0) continue;
                if (support.field[(y + 1) * w + x] & mask != 0) continue;
                if (support.field[(y + 1) * w + x + 1] & mask != 0) continue;
                return false;
            }
        }
    }
    return true;
}

// [Q] Pick unknown in the most constrained 2x2 quad (fewest unknowns, no flags)
pub fn bestCandidate(problem: *const core.Problem, support: *const Support, min_rating: *f64) ?usize {
    var best: ?usize = null;
    const w = problem.w;
    const field = &support.field;
    for (0..problem.h - 1) |y| {
        for (0..w - 1) |x| {
            const qi = y * w + x;
            const tl = field[qi];
            const tr = field[qi + 1];
            const bl = field[qi + w];
            const br = field[qi + w + 1];
            if (tl == FLAG or tr == FLAG or bl == FLAG or br == FLAG) continue;

            var unknowns: f64 = 0;
            var unknown_idx: ?usize = null;
            if (tl == UNKNOWN) { unknowns += 1; unknown_idx = qi; }
            if (tr == UNKNOWN) { unknowns += 1; unknown_idx = qi + 1; }
            if (bl == UNKNOWN) { unknowns += 1; unknown_idx = qi + w; }
            if (br == UNKNOWN) { unknowns += 1; unknown_idx = qi + w + 1; }

            if (unknowns == 1) {
                min_rating.* = 0;
                return unknown_idx;
            }

            const rating = (5 - unknowns) * 10;
            if (unknown_idx != null and rating < min_rating.*) {
                min_rating.* = rating;
                best = unknown_idx;
            }
        }
    }
    return best;
}
