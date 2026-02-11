const core = @import("../core.zig");
const algorithm = @import("../algorithm.zig");

const Support = algorithm.Support;
const FLAG = core.FLAG;

// [T] Flags may not form row of three orthogonally or diagonally
pub fn check(problem: *const core.Problem, support: *const Support) bool {
    const w: isize = @intCast(problem.w);
    const h: isize = @intCast(problem.h);
    const field = &support.field;

    // Directions: horizontal, vertical, diagonal \, diagonal /
    const dirs = [_][2]isize{ .{ 1, 0 }, .{ 0, 1 }, .{ 1, 1 }, .{ -1, 1 } };
    // For each direction, check flag at start (+1,+2), middle (-1,+1), and end (-2,-1)
    const offsets = [_][2]isize{ .{ 1, 2 }, .{ -1, 1 }, .{ -2, -1 } };

    for (support.flagSlice()[support.last_checked_flag_idx..]) |idx| {
        const x: isize = @intCast(idx % problem.w);
        const y: isize = @intCast(idx / problem.w);

        for (dirs) |dir| {
            const dx = dir[0];
            const dy = dir[1];
            for (offsets) |off| {
                const x1 = x + dx * off[0];
                const y1 = y + dy * off[0];
                const x2 = x + dx * off[1];
                const y2 = y + dy * off[1];
                if (x1 < 0 or x1 >= w or y1 < 0 or y1 >= h) continue;
                if (x2 < 0 or x2 >= w or y2 < 0 or y2 >= h) continue;
                const ni = @as(usize, @intCast(y1 * w + x1));
                const nj = @as(usize, @intCast(y2 * w + x2));
                if (field[ni] == FLAG and field[nj] == FLAG) return false;
            }
        }
    }
    return true;
}
