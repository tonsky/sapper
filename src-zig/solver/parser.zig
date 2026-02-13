const std = @import("std");
const core = @import("core.zig");

fn parseCell(ch: u8) u8 {
    return switch (ch) {
        'F' => core.FLAG,
        '?', '-' => core.OPEN,
        '.' => core.UNKNOWN,
        '0'...'8' => @intCast(ch - '0'),
        else => unreachable,
    };
}

fn parseNumber(input: []const u8, pos: *usize) ?usize {
    const start = pos.*;
    while (pos.* < input.len and std.ascii.isDigit(input[pos.*])) {
        pos.* += 1;
    }
    if (pos.* == start) return null;
    return std.fmt.parseInt(usize, input[start..pos.*], 10) catch null;
}

fn skipWhitespace(input: []const u8, pos: *usize) void {
    while (pos.* < input.len and std.ascii.isWhitespace(input[pos.*])) {
        pos.* += 1;
    }
}

fn parseId(input: []const u8) ?struct { core.Problem, usize } {
    var pos: usize = 0;

    // Parse rule indicators [T], [V], [T*], etc.
    // All puzzles have total and vanilla checks
    var rules = core.Rules{ .total = true, .vanilla = true };
    while (pos < input.len and input[pos] == '[') {
        pos += 1; // skip '['
        const rule_start = pos;
        while (pos < input.len and input[pos] != ']') {
            pos += 1;
        }
        const rule = input[rule_start..pos];
        if (std.mem.eql(u8, rule, "V")) {
            // vanilla, no extra rule
        } else if (std.mem.eql(u8, rule, "T")) {
            rules.no_triplet = true;
        } else if (std.mem.eql(u8, rule, "T'")) {
            rules.triplet = true;
        } else if (std.mem.eql(u8, rule, "Q")) {
            rules.quad = true;
        } else if (std.mem.eql(u8, rule, "C")) {
            rules.connected = true;
        } else {
            return null; // unknown rule
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
    while (pos < input.len and !std.ascii.isWhitespace(input[pos])) {
        pos += 1;
    }

    // Skip whitespace before field
    skipWhitespace(input, &pos);

    return .{
        .{
            .w = w,
            .h = h,
            .total_flags = total_flags,
            .rules = rules,
        },
        pos,
    };
}

// Parses user-facing format like
//
//   [V]5x5-10-10181 ..2.. .3... .3... ...2. ...2.
//
// . means uknown, - means open, F means flag, number is number of mines
pub fn parsePlayerProblem(input: []const u8) ?core.Problem {
    var problem, var pos = parseId(input) orelse return null;
    const size = problem.w * problem.h;
    if (size > core.MAX_SIZE) return null;

    var idx: usize = 0;
    while (pos < input.len and idx < size) {
        const ch = input[pos];
        if (std.ascii.isWhitespace(ch)) {
            pos += 1;
            continue;
        }
        problem.field[idx] = parseCell(ch);
        idx += 1;
        pos += 1;
    }

    if (idx != size) return null; // Not enough cells

    return problem;
}

// Parses user-facing format like
//
//   [C]5x5-10-1458D qfOOffffOfqoqfooqfqooOqff
//
// q means unknown open, f means unknown flag, o means unknown known,
// Q means open, F means flag, O means known (will be replaced with number of mines)
pub fn parseRawProblem(line: []const u8) ?core.Problem {
    var problem, const pos = parseId(line) orelse return null;
    const size = problem.w * problem.h;
    if (size > core.MAX_SIZE) return null;

    var field_end = pos;
    while (field_end < line.len and !std.ascii.isWhitespace(line[field_end])) field_end += 1;
    const encoded = line[pos..field_end];
    if (encoded.len != size) return null;

    // Step 1: f and F → FLAG, O → marker (core.RESERVED), rest → OPEN
    for (0..size) |i| {
        problem.field[i] = switch (encoded[i]) {
            'f', 'F' => core.FLAG,
            'O' => core.RESERVED,
            else => core.OPEN,
        };
    }

    // Step 2: Replace O markers with count of neighboring FLAGs
    for (0..size) |i| {
        if (problem.field[i] != core.RESERVED) continue;
        problem.field[i] = @intCast(core.countNeighbors(&problem.field, problem.w, problem.h, i, core.FLAG));
    }

    // Step 3: all lowercase → UNKNOWN (after O counting is done)
    for (0..size) |i| {
        if (encoded[i] >= 'a' and encoded[i] <= 'z') problem.field[i] = core.UNKNOWN;
    }

    return problem;
}
