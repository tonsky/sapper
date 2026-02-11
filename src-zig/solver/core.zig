const std = @import("std");

pub const FLAG: u8       = 0b00010000;
pub const OPEN: u8       = 0b00100000;
pub const UNKNOWN: u8    = 0b01000000;
pub const KNOWN_MASK: u8 = 0b00001111;
pub const RESERVED: u8   = 0b10000000;
pub const DANGER: u8     = 0b10010000;
pub const SAFE: u8       = 0b10100000;

pub const Rules = packed struct {
    total: bool = false,
    vanilla: bool = false,
    no_triplet: bool = false,
    quad: bool = false,
    connected: bool = false,
};

pub const Problem = struct {
    field: []u8,
    w: usize,
    h: usize,
    total_flags: usize,
    rules: Rules,
};

pub fn countNeighbors(field: []u8, w: usize, h: usize, idx: usize, mask: u8) i8 {
    const x = idx % w;
    const y = idx / w;
    var res: i8 = 0;

    const x_start: usize = if (x > 0) x - 1 else 0;
    const x_end: usize = if (x + 1 < w) x + 2 else w;
    const y_start: usize = if (y > 0) y - 1 else 0;
    const y_end: usize = if (y + 1 < h) y + 2 else h;

    for (y_start..y_end) |ny| {
        for (x_start..x_end) |nx| {
            if (nx == x and ny == y) continue;
            if (field[ny * w + nx] & mask != 0) res += 1;
        }
    }
    return res;
}
