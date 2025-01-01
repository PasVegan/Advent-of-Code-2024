const std = @import("std");
const Allocator = std.mem.Allocator;

const Iter = struct {
    n: usize,

    fn next(self: *@This()) usize {
        const mn: usize = @as(u24, @truncate((self.n * 64) ^ self.n));
        const dn: usize = @as(u24, @truncate((mn / 32) ^ mn));
        self.n = @as(u24, @truncate((dn * 2048) ^ dn));
        return self.n;
    }
};

fn make_iter(n: usize) Iter {
    return Iter{
        .n = n,
    };
}

fn solve1(input: []const u8, _: Allocator) !usize {
    var sum: usize = 0;
    var rowIter = std.mem.tokenizeScalar(u8, input, '\n');

    while (rowIter.next()) |row| {
        var num = try std.fmt.parseInt(usize, row, 10);
        var iter = make_iter(num);
        for (0..2000) |_| {
            num = iter.next();
        }

        sum += num;
    }

    return sum;
}

fn solve2(input: []const u8, alloc: Allocator) !usize {
    var rowIter = std.mem.tokenizeScalar(u8, input, '\n');
    var bananas = std.AutoArrayHashMap([4]isize, usize).init(alloc);
    defer bananas.deinit();

    var duplicated_seq = std.AutoArrayHashMap([4]isize, void).init(alloc);
    defer duplicated_seq.deinit();
    var seq = std.ArrayList(isize).init(alloc);
    defer seq.deinit();

    while (rowIter.next()) |row| {
        defer duplicated_seq.clearRetainingCapacity();
        defer seq.clearRetainingCapacity();
        var num = try std.fmt.parseInt(usize, row, 10);
        var iter = make_iter(num);
        for (0..2000) |_| {
            const n = iter.next();
            const curr = n % 10;
            const diff = @as(isize, @intCast(curr)) - @as(isize, @intCast(num % 10));
            num = n;

            try seq.append(diff);
            if (seq.items.len > 4) {
                _ = seq.orderedRemove(0);
            }

            if (seq.items.len == 4) {
                const key = seq.items[0..4].*;
                if (!duplicated_seq.contains(key)) {
                    try duplicated_seq.put(key, {});
                    const entry = try bananas.getOrPutValue(key, 0);
                    entry.value_ptr.* += curr;
                }
            }
        }
    }

    var max: usize = 0;
    for (bananas.values()) |v| {
        if (v > max) {
            max = v;
        }
    }

    return max;
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