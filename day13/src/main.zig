const std = @import("std");
const Allocator = std.mem.Allocator;

const Coord = struct { x: f64, y: f64 };

fn find_solution(ba: Coord, bb: Coord, p: Coord, comptime limit: ?f64) ?usize {
    const minb = (p.x * ba.y - p.y * ba.x) / (ba.y * bb.x - ba.x * bb.y);
    const mina = (p.y - minb * bb.y) / ba.y;

    if (mina > 0.0 and minb > 0.0 and mina == @trunc(mina) and minb == @trunc(minb)) {
        if (limit == null or (mina <= limit.? and minb <= limit.?)) {
            return @as(usize, @intFromFloat(mina)) * 3 + @as(usize, @intFromFloat(minb));
        }
    }

    return null;
}

fn solve1(input: []const u8, _: Allocator) !usize {
    var groupIter = std.mem.tokenizeSequence(u8, input, "\n\n");

    var total: usize = 0;

    while (groupIter.next()) |group| {
        var iter = std.mem.tokenizeAny(u8, group, "PrizeButon AB:XY+=,\n");

        const bax = try std.fmt.parseInt(usize, iter.next() orelse return error.InvalidInput, 10);
        const bay = try std.fmt.parseInt(usize, iter.next() orelse return error.InvalidInput, 10);

        const bbx = try std.fmt.parseInt(usize, iter.next() orelse return error.InvalidInput, 10);
        const bby = try std.fmt.parseInt(usize, iter.next() orelse return error.InvalidInput, 10);

        const px = try std.fmt.parseInt(usize, iter.next() orelse return error.InvalidInput, 10);
        const py = try std.fmt.parseInt(usize, iter.next() orelse return error.InvalidInput, 10);

        if (find_solution(.{ .x = @floatFromInt(bax), .y = @floatFromInt(bay) }, .{ .x = @floatFromInt(bbx), .y = @floatFromInt(bby) }, .{ .x = @floatFromInt(px), .y = @floatFromInt(py) }, 100.0)) |solution| {
            total += solution;
        }
    }
    return total;
}

fn solve2(input: []const u8, _: Allocator) !usize {
    var groupIter = std.mem.tokenizeSequence(u8, input, "\n\n");

    var total: usize = 0;

    while (groupIter.next()) |group| {
        var iter = std.mem.tokenizeAny(u8, group, "PrizeButon AB:XY+=,\n");

        const bax = try std.fmt.parseInt(usize, iter.next() orelse return error.InvalidInput, 10);
        const bay = try std.fmt.parseInt(usize, iter.next() orelse return error.InvalidInput, 10);

        const bbx = try std.fmt.parseInt(usize, iter.next() orelse return error.InvalidInput, 10);
        const bby = try std.fmt.parseInt(usize, iter.next() orelse return error.InvalidInput, 10);

        const px = 10000000000000 + try std.fmt.parseInt(usize, iter.next() orelse return error.InvalidInput, 10);
        const py = 10000000000000 + try std.fmt.parseInt(usize, iter.next() orelse return error.InvalidInput, 10);

        if (find_solution(.{ .x = @floatFromInt(bax), .y = @floatFromInt(bay) }, .{ .x = @floatFromInt(bbx), .y = @floatFromInt(bby) }, .{ .x = @floatFromInt(px), .y = @floatFromInt(py) }, null)) |solution| {
            total += solution;
        }
    }
    return total;
}

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = general_purpose_allocator.allocator();

    const file = try std.fs.cwd().openFile(
        "input.txt",
        .{},
    );
    defer file.close();

    const buffer = try file.readToEndAlloc(gpa, 1000000);

    const res = try solve1(buffer, gpa);

    std.debug.print("Part 1: {d}\n", .{res});

    const res2 = try solve2(buffer, gpa);

    std.debug.print("Part 2: {d}\n", .{res2});
}