const core = @import("../core.zig");
const algorithm = @import("../algorithm.zig");

const Support = algorithm.Support;
const FLAG = core.FLAG;

// [T] Flags may not form row of three orthogonally or diagonally
pub fn check(problem: *const core.Problem, support: *const Support) bool {
    // Openining a cell can't violate anti-triplet proprerty
    if (support.last_checked_flag_idx == support.flag_indices.items.len) return true;

    const w = problem.w;
    const flag_indices = support.flag_indices.items;
    for (flag_indices) |idx| {
        const y = idx / w;
        const x = idx % w;

        // FFF
        // ...
        // ...
        if (x + 2 < w) {
            if (support.field[idx + 1] == FLAG and support.field[idx + 2] == FLAG) {
                return false;
            }
        }

        // F..
        // F..
        // F..
        if (y + 2 < problem.h) {
            if (support.field[idx + w] == FLAG and support.field[idx + 2 * w] == FLAG) {
                return false;
            }
        }

        // F..
        // .F.
        // ..F
        if (x + 2 < w and y + 2 < problem.h) {
            if (support.field[idx + w + 1] == FLAG and support.field[idx + 2 * w + 2] == FLAG) {
                return false;
            }
        }

        // ..F
        // .F.
        // F..
        if (x >= 2 and y + 2 < problem.h) {
            if (support.field[idx + w - 1] == FLAG and support.field[idx + 2 * w - 2] == FLAG) {
                return false;
            }
        }
    }
    return true;
}
