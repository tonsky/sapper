const std = @import("std");
const parser = @import("solver/parser.zig");
const algorithm = @import("solver/algorithm.zig");
const printer = @import("solver/printer.zig");

pub fn solvePlayerProblem(input: []const u8, out: []u8) ?[]const u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const problem = parser.parsePlayerProblem(input, arena.allocator()) orelse return null;

    if (algorithm.solve(&problem, arena.allocator())) |field| {
        return printer.fieldToStr(&problem, field, out);
    } else return null;
}
