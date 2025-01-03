const std = @import("std");
const Allocator = std.mem.Allocator;

fn recursiveSum(slice: []usize, sum: usize, target: usize, part2: bool) !?usize {
    if (sum > target) {
        return null;
    }
    if (slice.len == 0) {
        if (sum == target) {
            return sum;
        } else {
            return null;
        }
    }

    if (try recursiveSum(slice[1..], sum + slice[0], target, part2)) |total| {
        return total;
    } else if (try recursiveSum(slice[1..], (if (sum == 0) 1 else sum) * slice[0], target, part2)) |total| {
        return total;
    }

    if (part2) {
        if (sum == 0) {
            const next = slice[0] * try std.math.powi(usize, 10, std.math.log10_int(slice[1]) + 1) + slice[1];
            if (try recursiveSum(slice[2..], next, target, part2)) |total| {
                return total;
            }
        } else {
            const next = sum * try std.math.powi(usize, 10, std.math.log10_int(slice[0]) + 1) + slice[0];
            if (try recursiveSum(slice[1..], next, target, part2)) |total| {
                return total;
            }
        }
    }
    return null;
}

fn solve1(input: []const u8, alloc: Allocator) !usize {
    var rowsIter = std.mem.tokenizeScalar(u8, input, '\n');
    var total: usize = 0;

    while (rowsIter.next()) |row| {
        var numbersIter = std.mem.tokenizeAny(u8, row, ": ");
        const target = try std.fmt.parseInt(usize, numbersIter.next() orelse return error.InvalidInput, 10);
        var list = std.ArrayList(usize).init(alloc);
        defer list.deinit();

        while (numbersIter.next()) |num| {
            const n = try std.fmt.parseInt(usize, num, 10);
            try list.append(n);
        }

        if (try recursiveSum(list.items, 0, target, false)) |_| {
            total += target;
        }
    }

    return total;
}

fn solve2(input: []const u8, alloc: Allocator) !usize {
    var rowsIter = std.mem.tokenizeScalar(u8, input, '\n');
    var total: usize = 0;

    while (rowsIter.next()) |row| {
        var numbersIter = std.mem.tokenizeAny(u8, row, ": ");
        const target = try std.fmt.parseInt(usize, numbersIter.next() orelse return error.InvalidInput, 10);
        var list = std.ArrayList(usize).init(alloc);
        defer list.deinit();

        while (numbersIter.next()) |num| {
            const n = try std.fmt.parseInt(usize, num, 10);
            try list.append(n);
        }

        if (try recursiveSum(list.items, 0, target, true)) |_| {
            total += target;
        }
    }

    return total;
}

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = general_purpose_allocator.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const buffer = try file.readToEndAlloc(gpa, 1000000);

    const res = try solve1(buffer, gpa);

    std.debug.print("Part 1: {d}\n", .{res});

    const res2 = try solve2(buffer, gpa);

    std.debug.print("Part 2: {d}\n", .{res2});
}