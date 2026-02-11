const parser = @import("solver/parser.zig");
const algorithm = @import("solver/algorithm.zig");
const printer = @import("solver/printer.zig");

pub fn solvePlayerProblem(input: []const u8, out: []u8) ?[]const u8 {
    const problem = parser.parsePlayerProblem(input) orelse return null;

    if (algorithm.solve(&problem)) |field| {
        return printer.fieldToStr(&problem, &field, out);
    } else return null;
}

pub fn hintPlayerProblem(input: []const u8, out: []u8) ?[]const u8 {
    var problem = parser.parsePlayerProblem(input) orelse return null;

    const result = algorithm.hint(&problem);
    return printer.fieldToStr(&problem, &result, out);
}

// WASM exports

var input_buf: [8192]u8 = undefined;
var output_buf: [8192]u8 = undefined;

export fn getInputBuf() [*]u8 {
    return &input_buf;
}

export fn getOutputBuf() [*]const u8 {
    return &output_buf;
}

export fn solve(input_len: usize) usize {
    const input = input_buf[0..input_len];
    if (solvePlayerProblem(input, &output_buf)) |result| {
        return result.len;
    }
    return 0;
}

export fn hint(input_len: usize) usize {
    const input = input_buf[0..input_len];
    if (hintPlayerProblem(input, &output_buf)) |result| {
        return result.len;
    }
    return 0;
}
