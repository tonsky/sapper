const std = @import("std");

const FLAG: u8 = 0b00010000;
const OPEN: u8 = 0b00100000;
const UNKNOWN: u8 = 0b01000000;
const KNOWN_MASK: u8 = 0b00001111;

var visualize: bool = false;
var visualize_erase: bool = true;

const Rules = packed struct {
    total: bool = false,
    vanilla: bool = false,
    anti_triplet: bool = false,
    quad: bool = false,
    connected: bool = false,
};

const Problem = struct {
    field: []u8,
    w: usize,
    h: usize,
    total_flags: usize,
    rules: Rules,
    neighbor_indices: [64*9]u8,
    neighbor_masks: [64]u64,
    known_indices: []const u8,
    unknown_indices: []const u8,
    unknown_count: usize,
    flag_indices: std.ArrayList(u8),
    last_checked_flag_idx: usize,
    open_indices: std.ArrayList(u8),
    last_checked_open_idx: usize,
    flags_around: [64]i8,
    unknowns_around: [64]i8,

    fn oneof(self: *Problem, x: usize, y: usize, mask: u8) bool {
        return self.field[(y * self.w) + x] & mask != 0;
    }

    fn neighbors(self: *const Problem, i: usize) []const u8 {
        const start = i * 9 + 1;
        const len = self.neighbor_indices[i * 9];
        return self.neighbor_indices[start..start + len];
    }

    fn setFlag(self: *Problem, i: usize) void {
        std.debug.assert(self.field[i] == UNKNOWN);
        self.field[i] = FLAG;
        self.flag_indices.appendAssumeCapacity(@intCast(i));
        self.unknown_count -= 1;
        for (self.neighbors(i)) |ni| {
            self.flags_around[ni] += 1;
            self.unknowns_around[ni] -= 1;
        }
    }

    fn setOpen(self: *Problem, i: usize) void {
        std.debug.assert(self.field[i] == UNKNOWN);
        self.field[i] = OPEN;
        self.open_indices.appendAssumeCapacity(@intCast(i));
        self.unknown_count -= 1;
        for (self.neighbors(i)) |ni| {
            self.unknowns_around[ni] -= 1;
        }
    }

    fn setUnknown(self: *Problem, i: usize) void {
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

fn countNeighbors(field: []u8, w: usize, h: usize, idx: usize, mask: u8) i8 {
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

// [*] Total amount of flags across the entire field
fn totalCheck(problem: *const Problem) bool {
    const flagged = problem.flag_indices.items.len;
    return flagged <= problem.total_flags and
        flagged + problem.unknown_count >= problem.total_flags;
}

// [V] Nothing special - check each numbered cell
fn vanillaCheck(problem: *const Problem) bool {
    for (problem.known_indices) |i| {
        const rel = @as(i64, problem.field[i]) - @as(i64, problem.flags_around[i]);
        const unk = problem.unknowns_around[i];
        if (rel < 0) return false;
        if (unk < rel) return false;
    }
    return true;
}

// [Q] Every 2x2 area must contain at least one flag
fn quadCheck(problem: *Problem) bool {
    // Flagging a cell can't violate quad property
    if (problem.last_checked_open_idx == problem.open_indices.items.len) return true;

    const mask: u8 = FLAG | UNKNOWN;

    for (problem.open_indices.items[problem.last_checked_open_idx..]) |open_idx| {
        const open_y = open_idx / problem.w;
        const open_x = open_idx % problem.w;

        const x_start: usize = if (open_x > 0) open_x - 1 else 0;
        const x_end: usize = if (open_x < problem.w - 1) open_x else open_x - 1;
        const y_start: usize = if (open_y > 0) open_y - 1 else 0;
        const y_end: usize = if (open_y < problem.h - 1) open_y else open_y - 1;

        for (y_start..y_end + 1) |y| {
            for (x_start..x_end + 1) |x| {
                if (problem.oneof(x, y, mask)) continue;
                if (problem.oneof(x + 1, y, mask)) continue;
                if (problem.oneof(x, y + 1, mask)) continue;
                if (problem.oneof(x + 1, y + 1, mask)) continue;
                return false;
            }
        }
    }
    return true;
}

// [C] All mines are orthogonally or diagonally connected
fn connectedCheck(problem: *const Problem) bool {
    // Flagging a cell doesn't change connected proprerty
    if (problem.last_checked_open_idx == problem.open_indices.items.len) return true;

    const field = problem.field;
    const size = problem.w * problem.h;
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
            expanded |= problem.neighbor_masks[bit];
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
    for (problem.known_indices) |ki| {
        if (problem.flags_around[ki] >= problem.field[ki]) continue;
        const neighbors = problem.neighbor_masks[ki] & visited;
        const visited_count = @popCount(neighbors);
        if (visited_count < field[ki]) return false;
    }

    return true;
}

// [T] Flags may not form row of three orthogonally or diagonally
fn antiTripletCheck(problem: *const Problem) bool {
    // Openining a cell can't violate anti-triplet proprerty
    if (problem.last_checked_flag_idx == problem.flag_indices.items.len) return true;

    const flag_indices = problem.flag_indices.items;
    for (flag_indices) |idx| {
        const y = idx / problem.w;
        const x = idx % problem.w;

        // FFF
        // ...
        // ...
        if (x + 2 < problem.w) {
            if (problem.field[idx + 1] == FLAG and problem.field[idx + 2] == FLAG) {
                return false;
            }
        }

        // F..
        // F..
        // F..
        if (y + 2 < problem.h) {
            if (problem.field[idx + problem.w] == FLAG and problem.field[idx + 2 * problem.w] == FLAG) {
                return false;
            }
        }

        // F..
        // .F.
        // ..F
        if (x + 2 < problem.w and y + 2 < problem.h) {
            if (problem.field[idx + problem.w + 1] == FLAG and problem.field[idx + 2 * problem.w + 2] == FLAG) {
                return false;
            }
        }

        // ..F
        // .F.
        // F..
        if (x >= 2 and y + 2 < problem.h) {
            if (problem.field[idx + problem.w - 1] == FLAG and problem.field[idx + 2 * problem.w - 2] == FLAG) {
                return false;
            }
        }
    }
    return true;
}

fn eraseField(problem: *const Problem) void {
    if (!visualize_erase) return;
    var buf: [16]u8 = undefined;
    const esc = std.fmt.bufPrint(&buf, "\x1b[{d}A", .{problem.h + 1}) catch unreachable;
    var bw = std.fs.File.stdout().writer(&.{});
    bw.interface.writeAll(esc) catch {};
}

fn fieldToStr(problem: *const Problem, out: []u8) []const u8 {
    var len: usize = 0;
    for (0..problem.h) |y| {
        if (y > 0) {
            out[len] = '\n';
            len += 1;
        }
        for (0..problem.w) |x| {
            const ch = cellToChar(problem.field[y * problem.w + x]);
            @memcpy(out[len..][0..ch.len], ch);
            len += ch.len;
        }
    }
    return out[0..len];
}

fn printField(problem: *const Problem) void {
    var buf: [512]u8 = undefined;
    const str = fieldToStr(problem, &buf);
    var bw = std.fs.File.stdout().writer(&.{});
    bw.interface.writeAll(str) catch {};
    bw.interface.writeAll("\n\n") catch {};
}

fn beginSolving(problem: *Problem) bool {
    if (visualize) {
        eraseField(problem);
        printField(problem);
        std.Thread.sleep(50_000_000); // 50ms per frame
    }
    return autoOpen(problem);
}

fn autoOpen(problem: *Problem) bool {
    const flag_indices_len_before = problem.flag_indices.items.len;
    const open_indices_len_before = problem.open_indices.items.len;

    for (problem.known_indices) |i| {
        const unk = problem.unknowns_around[i];
        if (unk == 0) continue;

        const rel = @as(i64, problem.field[i]) - @as(i64, problem.flags_around[i]);
        if (rel == 0) {
            for (problem.neighbors(i)) |ni| {
                if (problem.field[ni] == UNKNOWN) {
                    problem.setOpen(ni);
                }
            }
            continue;
        }

        if (rel == unk) {
            for (problem.neighbors(i)) |ni| {
                if (problem.field[ni] == UNKNOWN) {
                    problem.setFlag(ni);
                }
            }
        }
    }

    if (autoFinish(problem))
        return true;

    // Undo flags/opens
    while (problem.flag_indices.items.len > flag_indices_len_before) {
        const idx = problem.flag_indices.items[problem.flag_indices.items.len - 1];
        problem.setUnknown(idx);
    }
    while (problem.open_indices.items.len > open_indices_len_before) {
        const idx = problem.open_indices.items[problem.open_indices.items.len - 1];
        problem.setUnknown(idx);
    }

    return false;
}

fn autoFinish(problem: *Problem) bool {
    const flag_indices_len_before = problem.flag_indices.items.len;
    const open_indices_len_before = problem.open_indices.items.len;
    const flagged = flag_indices_len_before;

    if (problem.unknown_count > 0) {
        if (problem.unknown_count + flagged <= problem.total_flags) {
            // Can flag the rest
            for (0..problem.w * problem.h) |i| {
                if (problem.field[i] == UNKNOWN) problem.setFlag(i);
            }
        } else if (flagged >= problem.total_flags) {
            // Can open the rest
            for (0..problem.w * problem.h) |i| {
                if (problem.field[i] == UNKNOWN) problem.setOpen(i);
            }
        }
    }

    if (checkConstraints(problem))
        return true;

    // Undo
    while (problem.flag_indices.items.len > flag_indices_len_before) {
        const idx = problem.flag_indices.items[problem.flag_indices.items.len - 1];
        problem.setUnknown(idx);
    }
    while (problem.open_indices.items.len > open_indices_len_before) {
        const idx = problem.open_indices.items[problem.open_indices.items.len - 1];
        problem.setUnknown(idx);
    }

    return false;
}

fn checkConstraints(problem: *Problem) bool {
    if (problem.rules.total and !totalCheck(problem)) return false;
    if (problem.rules.anti_triplet and !antiTripletCheck(problem)) return false;
    if (problem.rules.quad and !quadCheck(problem)) return false;
    if (problem.rules.connected and !connectedCheck(problem)) return false;
    if (problem.rules.vanilla and !vanillaCheck(problem)) return false;
    if (problem.unknown_count == 0) return true;
    return diveDeeper(problem);
}

fn bestCandidate(problem: *const Problem) ?usize {
    var min_rating: f64 = 1000;
    var min_index: ?usize = null;

    rating: {
        // [Q] Pick unknown in the most constrained 2x2 quad (fewest unknowns, no flags)
        if (problem.rules.quad) {
            const w = problem.w;
            const field = problem.field;
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
                        min_index = unknown_idx;
                        break :rating;
                    }

                    const rating = (5 - unknowns) * 10;
                    if (unknown_idx != null and rating < min_rating) {
                        min_rating = rating;
                        min_index = unknown_idx;
                    }
                }
            }
        }

        // [V] pick a cell that has least to open
        if (problem.rules.vanilla) {
            for (problem.known_indices) |i| {
                const rel = @as(i64, problem.field[i]) - problem.flags_around[i];
                const unknowns = problem.unknowns_around[i];
                const rating = @as(f64, @floatFromInt(unknowns - rel));
                if (unknowns > 0 and rating < min_rating) {
                    for (problem.neighbors(i)) |ni| {
                        if (problem.field[ni] == UNKNOWN) {
                            min_rating = rating;
                            min_index = @intCast(ni);
                            break;
                        }
                    }
                    if (min_rating <= 1) break :rating;
                }
            }
        }
    }

    if (min_index) |idx| {
        return idx;
    }

    // Fallback: first unknown in unknown_indices
    for (problem.field[0..], 0..) |value, i| {
        if (value == UNKNOWN) {
            return i;
        }
    }

    return null;
}

fn diveDeeper(problem: *Problem) bool {
    const candidate = bestCandidate(problem) orelse return false;

    const last_checked_flag_idx = problem.flag_indices.items.len;
    const last_checked_open_idx = problem.open_indices.items.len;

    // Try FLAG first
    problem.last_checked_flag_idx = last_checked_flag_idx;
    problem.last_checked_open_idx = last_checked_open_idx;
    problem.setFlag(candidate);
    if (beginSolving(problem)) return true;
    problem.setUnknown(candidate);

    // Try OPEN
    problem.last_checked_flag_idx = last_checked_flag_idx;
    problem.last_checked_open_idx = last_checked_open_idx;
    problem.setOpen(candidate);
    if (beginSolving(problem)) return true;
    problem.setUnknown(candidate);

    return false;
}

fn parseCell(ch: u8) u8 {
    return switch (ch) {
        'F' => FLAG,
        '?', '-' => OPEN,
        '.' => UNKNOWN,
        '0'...'8' => @intCast(ch - '0'),
        else => unreachable,
    };
}

fn cellToChar(val: u8) []const u8 {
    return switch (val) {
        0 => "0",
        1 => "1",
        2 => "2",
        3 => "3",
        4 => "4",
        5 => "5",
        6 => "6",
        7 => "7",
        8 => "8",
        FLAG => "F",
        OPEN => "-",
        UNKNOWN => ".",
        else => unreachable,
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

const Meta = struct {
    rules: Rules,
    w: usize,
    h: usize,
    total_flags: usize,
    pos: usize,
};

fn parseId(input: []const u8) ?Meta {
    var pos: usize = 0;

    // Parse rule indicators [T], [V], [T*], etc.
    // All puzzles have total and vanilla checks
    var rules = Rules{ .total = true, .vanilla = true };
    while (pos < input.len and input[pos] == '[') {
        pos += 1; // skip '['
        const rule_start = pos;
        while (pos < input.len and input[pos] != ']') {
            pos += 1;
        }
        const rule = input[rule_start..pos];
        if (std.mem.eql(u8, rule, "T")) {
            rules.anti_triplet = true;
        } else if (std.mem.eql(u8, rule, "Q")) {
            rules.quad = true;
        } else if (std.mem.eql(u8, rule, "C")) {
            rules.connected = true;
        }
        if (pos < input.len) pos += 1; // skip ']'
    }

    // Parse WxH
    const w = parseNumber(input, &pos) orelse return null;
    if (pos >= input.len or input[pos] != 'x') return null;
    pos += 1; // skip 'x'
    const h = parseNumber(input, &pos) orelse return null;

    // Skip '-'
    if (pos >= input.len or input[pos] != '-') return null;
    pos += 1;

    // Parse total flags
    const total_flags = parseNumber(input, &pos) orelse return null;

    // Skip '-'
    if (pos >= input.len or input[pos] != '-') return null;
    pos += 1;

    // Skip puzzle id (alphanumeric)
    while (pos < input.len and !isWhitespace(input[pos])) {
        pos += 1;
    }

    // Skip whitespace before field
    skipWhitespace(input, &pos);

    return Meta{
        .rules = rules,
        .w = w,
        .h = h,
        .total_flags = total_flags,
        .pos = pos,
    };
}

fn buildProblem(input: []const u8, w: usize, h: usize, total_flags: usize, rules: Rules, allocator: std.mem.Allocator) ?Problem {
    const size = w * h;
    var known_indices = std.ArrayList(u8).initCapacity(allocator, size) catch return null;
    var unknown_indices = std.ArrayList(u8).initCapacity(allocator, size) catch return null;

    const field = allocator.alloc(u8, size) catch return null;
    var unknown_count: usize = 0;

    for (0..size) |idx| {
        const val = input[idx];
        if (val <= 8) {
            field[idx] = val;
            known_indices.appendAssumeCapacity(@intCast(idx));
        } else {
            field[idx] = UNKNOWN;
            unknown_count += 1;
            unknown_indices.appendAssumeCapacity(@intCast(idx));
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

    var problem = Problem{
        .field = field,
        .w = w,
        .h = h,
        .total_flags = total_flags,
        .rules = rules,
        .neighbor_indices = neighbor_indices,
        .neighbor_masks = neighbor_masks,
        .known_indices = known_indices.items,
        .unknown_indices = unknown_indices.items,
        .unknown_count = unknown_count,
        .flag_indices = std.ArrayList(u8).initCapacity(allocator, size) catch return null,
        .last_checked_flag_idx = 0,
        .open_indices = std.ArrayList(u8).initCapacity(allocator, size) catch return null,
        .last_checked_open_idx = 0,
        .flags_around = .{0} ** 64,
        .unknowns_around = .{0} ** 64,
    };

    for (0..size) |i| {
        problem.unknowns_around[i] = countNeighbors(field, w, h, i, UNKNOWN);
    }

    // Seed open_indices with known cells (they are open by definition)
    for (known_indices.items) |ki| {
        problem.open_indices.appendAssumeCapacity(ki);
    }

    // Apply initial flags and opens through setFlag/setOpen
    for (0..size) |idx| {
        const val = input[idx];
        if (val == FLAG) problem.setFlag(idx)
        else if (val == OPEN) problem.setOpen(idx);
    }

    return problem;
}

// Parses user-facing format like
//
//   [V]5x5-10-10181 ..2.. .3... .3... ...2. ...2.
//
// . means uknown, - means open, F means flag, number is number of mines
fn parsePlayerProblem(input: []const u8, allocator: std.mem.Allocator) ?Problem {
    const meta = parseId(input) orelse return null;
    var pos = meta.pos;
    const w = meta.w;
    const h = meta.h;
    const size = w * h;
    const field = allocator.alloc(u8, size) catch return null;

    var idx: usize = 0;
    while (pos < input.len and idx < size) {
        const ch = input[pos];
        if (isWhitespace(ch)) {
            pos += 1;
            continue;
        }
        field[idx] = parseCell(ch);
        idx += 1;
        pos += 1;
    }

    if (idx != size) return null; // Not enough cells

    return buildProblem(field, w, h, meta.total_flags, meta.rules, allocator);
}

// Parses user-facing format like
//
//   [C]5x5-10-1458D qfOOffffOfqoqfooqfqooOqff
//
// q means unknown open, f means unknown flag, o means unknown known,
// Q means open, F means flag, O means known (will be replaced with number of mines)
fn parseRawProblem(line: []const u8, allocator: std.mem.Allocator) ?Problem {
    const meta = parseId(line) orelse return null;
    const w = meta.w;
    const h = meta.h;
    const size = w * h;

    var field_end = meta.pos;
    while (field_end < line.len and !isWhitespace(line[field_end])) field_end += 1;
    const encoded = line[meta.pos..field_end];
    if (encoded.len != size) return null;

    var field: [64]u8 = undefined;

    // Step 1: f and F → FLAG, O → marker (0b10000000), rest → OPEN
    for (0..size) |i| {
        field[i] = switch (encoded[i]) {
            'f', 'F' => FLAG,
            'O' => 0b10000000,
            else => OPEN,
        };
    }

    // Step 2: Replace O markers with count of neighboring FLAGs
    for (0..size) |i| {
        if (field[i] != 0b10000000) continue;
        field[i] = @intCast(countNeighbors(field[0..size], w, h, i, FLAG));
    }

    // Step 3: all lowercase → UNKNOWN (after O counting is done)
    for (0..size) |i| {
        if (encoded[i] >= 'a' and encoded[i] <= 'z') field[i] = UNKNOWN;
    }

    return buildProblem(field[0..size], w, h, meta.total_flags, meta.rules, allocator);
}

pub fn solve(input: []const u8, out: []u8) ?[]const u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var problem = parsePlayerProblem(input, arena.allocator()) orelse return null;

    if (!beginSolving(&problem)) return null;

    return fieldToStr(&problem, out);
}

// Benchmark solving puzzles. Invoke as:
//
//     solver bench [--iters N] --puzzle "..."
//
fn cmdBenchOne(input: []const u8, iters: usize, stdout: anytype) !void {
    const name = if (std.mem.indexOfAny(u8, input, " \n")) |ws| input[0..ws] else input;
    try stdout.print("{s}", .{name});
    var buf: [256]u8 = undefined;
    // Warmup
    for (0..iters) |_| {
        _ = solve(input, &buf);
    }
    var timer = try std.time.Timer.start();
    for (0..iters) |_| {
        _ = solve(input, &buf);
    }
    const bench_ns = timer.read();
    try stdout.print("\t{d:.3} ms/solve\t{d} iters\n", .{
        @as(f64, @floatFromInt(bench_ns)) / @as(f64, @floatFromInt(iters)) / 1_000_000.0,
        iters,
    });
}

// Benchmark solving puzzles. Invoke as:
//
//     solver bench [--iters N]
//
fn cmdBenchAll(iters: usize, stdout: anytype) !void {
    const content = std.fs.cwd().readFileAlloc(std.heap.page_allocator, "dev/tests.txt", 1024 * 1024) catch |err| {
        try stdout.print("Failed to read dev/tests.txt: {}\n", .{err});
        return;
    };
    defer std.heap.page_allocator.free(content);

    var pos: usize = 0;
    while (std.mem.indexOfPos(u8, content, pos, "Given:")) |given_idx| {
        pos = given_idx + "Given:".len;
        const expect_idx = std.mem.indexOfPos(u8, content, pos, "Expect:") orelse break;
        const input = std.mem.trim(u8, content[pos..expect_idx], " \n\r");
        pos = expect_idx;
        try cmdBenchOne(input, iters, stdout);
    }
}

// Visualize solving a single puzzle. Invoke as:
//
//     solver visualize --puzzle "..." [--erase false]
//
fn cmdVisualize(input: []const u8, erase: bool) void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var problem = parsePlayerProblem(input, arena.allocator()) orelse {
        var bw = std.fs.File.stdout().writer(&.{});
        bw.interface.writeAll("Failed to parse puzzle\n") catch {};
        return;
    };
    visualize = true;
    visualize_erase = erase;
    printField(&problem);

    const solved = beginSolving(&problem);

    eraseField(&problem);
    if (solved) {
        printField(&problem);
    } else {
        var stdout = std.fs.File.stdout().writer(&.{});
        stdout.interface.writeAll("No solution found\n") catch {};
    }
}

// Solve a puzzle and print solution. Invoke as:
//
//     solver solve --puzzle "..."
//
fn cmdSolveOne(input: []const u8, stdout: anytype) !void {
    var buf: [512]u8 = undefined;
    if (solve(input, &buf)) |result| {
        try stdout.print("{s}\n", .{result});
    } else {
        try stdout.print("No solution found\n", .{});
    }
}

// Solve all puzzles from public/puzzles. Invoke as:
//
//     solver solve-all
//
fn cmdSolveAll(stdout: anytype) !void {
    const allocator = std.heap.page_allocator;
    var dir = std.fs.cwd().openDir("public/puzzles", .{ .iterate = true }) catch |err| {
        try stdout.print("Failed to open public/puzzles: {}\n", .{err});
        return;
    };
    defer dir.close();

    // Collect .txt filenames
    var files = try std.ArrayList([]const u8).initCapacity(allocator, 16);
    defer {
        for (files.items) |name| allocator.free(name);
        files.deinit(allocator);
    }

    var it = dir.iterate();
    while (try it.next()) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".txt")) continue;
        const name = try allocator.dupe(u8, entry.name);
        try files.append(allocator, name);
    }

    std.mem.sort([]const u8, files.items, {}, struct {
        fn f(_: void, a: []const u8, b: []const u8) bool {
            return std.mem.order(u8, a, b) == .lt;
        }
    }.f);

    for (files.items) |filename| {
        const content = dir.readFileAlloc(allocator, filename, 10 * 1024 * 1024) catch |err| {
            try stdout.print("{s}: failed to read: {}\n", .{ filename, err });
            continue;
        };
        defer allocator.free(content);

        var timer = try std.time.Timer.start();
        var solved: usize = 0;
        var failed: usize = 0;

        var line_iter = std.mem.splitScalar(u8, content, '\n');
        while (line_iter.next()) |line| {
            if (line.len == 0) continue;

            var arena = std.heap.ArenaAllocator.init(allocator);
            defer arena.deinit();

            var problem = parseRawProblem(line, arena.allocator()) orelse {
                failed += 1;
                const id_end = std.mem.indexOfAny(u8, line, " \t") orelse line.len;
                try stdout.print("  PARSE ERROR: {s}\n", .{line[0..id_end]});
                continue;
            };

            var puzzle_timer = try std.time.Timer.start();
            const solved_ok = beginSolving(&problem);
            const puzzle_ns = puzzle_timer.read();
            const puzzle_ms = @as(f64, @floatFromInt(puzzle_ns)) / 1_000_000.0;
            if (solved_ok) {
                solved += 1;
                if (puzzle_ms > 5.0) {
                    const id_end = std.mem.indexOfAny(u8, line, " \t") orelse line.len;
                    try stdout.print("  SLOW: {s}\t{d:.1} ms\n", .{ line[0..id_end], puzzle_ms });
                }
            } else {
                failed += 1;
                const id_end = std.mem.indexOfAny(u8, line, " \t") orelse line.len;
                try stdout.print("  NO SOLUTION: {s}\n", .{line[0..id_end]});
                var fbuf: [512]u8 = undefined;
                try stdout.print("  field:\n{s}\n", .{fieldToStr(&problem, &fbuf)});
            }
        }

        const elapsed_ns = timer.read();
        const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;
        if (failed > 0) {
            try stdout.print("{s}\t{d} solved\t{d} FAILED\t{d:.1} ms\n", .{ filename, solved, failed, elapsed_ms });
        } else {
            try stdout.print("{s}\t{d} solved\t{d:.1} ms\n", .{ filename, solved, elapsed_ms });
        }
    }
}

pub fn main() !void {
    var w = std.fs.File.stdout().writer(&.{});
    const stdout = &w.interface;

    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);

    if (args.len < 2) {
        try stdout.print("Usage: solver bench [--puzzle \"...\"] [--iters N]\n       solver visualize --puzzle \"...\"\n       solver solve-all\n", .{});
        return;
    }

    const command = args[1];
    var iters: usize = 1000;
    var puzzle: ?[]const u8 = null;
    var erase: bool = true;

    var i: usize = 2;
    while (i < args.len) : (i += 1) {
        if (std.mem.eql(u8, args[i], "--iters")) {
            i += 1;
            if (i >= args.len) {
                try stdout.print("--iters requires a number\n", .{});
                return;
            }
            iters = std.fmt.parseInt(usize, args[i], 10) catch {
                try stdout.print("--iters: invalid number '{s}'\n", .{args[i]});
                return;
            };
        } else if (std.mem.eql(u8, args[i], "--puzzle")) {
            i += 1;
            if (i >= args.len) {
                try stdout.print("--puzzle requires a puzzle string\n", .{});
                return;
            }
            puzzle = args[i];
        } else if (std.mem.eql(u8, args[i], "--erase")) {
            i += 1;
            if (i >= args.len) {
                try stdout.print("--erase requires true or false\n", .{});
                return;
            }
            erase = std.mem.eql(u8, args[i], "true");
        } else {
            try stdout.print("Unknown argument: {s}\n", .{args[i]});
            return;
        }
    }

    if (std.mem.eql(u8, command, "bench")) {
        if (puzzle) |p| {
            try cmdBenchOne(p, iters, stdout);
        } else {
            try cmdBenchAll(iters, stdout);
        }
    } else if (std.mem.eql(u8, command, "visualize")) {
        if (puzzle) |p| {
            cmdVisualize(p, erase);
        } else {
            try stdout.print("visualize requires --puzzle\n", .{});
        }
    } else if (std.mem.eql(u8, command, "solve")) {
        if (puzzle) |p| {
            try cmdSolveOne(p, stdout);
        } else {
            try stdout.print("solve requires --puzzle\n", .{});
        }
    } else if (std.mem.eql(u8, command, "solve-all")) {
        try cmdSolveAll(stdout);
    } else {
        try stdout.print("Unknown command: {s}\n", .{command});
    }
}

test "solve puzzles from tests.txt" {
    const content = std.fs.cwd().readFileAlloc(std.testing.allocator, "dev/tests.txt", 1024 * 1024) catch |err| {
        std.debug.print("\nFailed to read dev/tests.txt: {}\n", .{err});
        return err;
    };
    defer std.testing.allocator.free(content);
    var buf: [256]u8 = undefined;
    var pos: usize = 0;
    var test_count: usize = 0;

    while (std.mem.indexOfPos(u8, content, pos, "Given:")) |given_idx| {
        pos = given_idx + "Given:".len;
        const expect_idx = std.mem.indexOfPos(u8, content, pos, "Expect:") orelse break;
        const input = std.mem.trim(u8, content[pos..expect_idx], " \n\r");

        pos = expect_idx + "Expect:".len;
        const next_given = std.mem.indexOfPos(u8, content, pos, "Given:");
        const expect_end = next_given orelse content.len;
        const expected = std.mem.trim(u8, content[pos..expect_end], " \n\r");

        pos = expect_end;

        const result = solve(input, &buf) orelse {
            std.debug.print("\nFAIL: solve returned null for: {s}\n", .{input[0..@min(input.len, 40)]});
            return error.TestUnexpectedResult;
        };

        std.testing.expectEqualStrings(expected, result) catch {
            std.debug.print("\nFAIL: {s}\n", .{input[0..@min(input.len, 40)]});
            return error.TestUnexpectedResult;
        };

        test_count += 1;
    }

    try std.testing.expect(test_count > 0);
}
