const std = @import("std");
const print = std.debug.print;
const ArrayList = std.ArrayList;

const Rule = struct {
    before: u32,
    after: u32,
};

fn isValidOrder(pages: []const u32, rules: []const Rule) bool {
    for (rules) |rule| {
        var found_before: bool = false;
        var found_after: bool = false;
        var before_index: usize = 0;
        var after_index: usize = 0;

        for (pages, 0..) |page, i| {
            if (page == rule.before) {
                found_before = true;
                before_index = i;
            }
            if (page == rule.after) {
                found_after = true;
                after_index = i;
            }
        }

        if (found_before and found_after and before_index > after_index) {
            return false;
        }
    }
    return true;
}

const SortContext = struct {
    rules: []const Rule,

    pub fn lessThan(ctx: @This(), a: u32, b: u32) bool {
        // Check if there's a rule that says a must come before b
        for (ctx.rules) |rule| {
            if (rule.before == a and rule.after == b) return true;
            if (rule.before == b and rule.after == a) return false;
        }
        // If no direct rule exists, default to numerical order
        return a < b;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var rules = ArrayList(Rule).init(allocator);
    defer rules.deinit();

    var updates = ArrayList(ArrayList(u32)).init(allocator);
    defer {
        for (updates.items) |*update| {
            update.deinit();
        }
        updates.deinit();
    }

    var buf: [16384]u8 = undefined;
    var reading_rules = true;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (line.len == 0) {
            reading_rules = false;
            continue;
        }

        if (reading_rules) {
            var parts = std.mem.split(u8, line, "|");
            const before = try std.fmt.parseInt(u32, parts.first(), 10);
            const after = try std.fmt.parseInt(u32, parts.rest(), 10);
            try rules.append(.{ .before = before, .after = after });
        } else {
            var update = ArrayList(u32).init(allocator);
            var numbers = std.mem.split(u8, line, ",");
            while (numbers.next()) |num_str| {
                const num = try std.fmt.parseInt(u32, num_str, 10);
                try update.append(num);
            }
            try updates.append(update);
        }
    }

    // Part 1: Sum of middle numbers in valid updates
    var sum_valid: u32 = 0;
    for (updates.items) |update| {
        if (isValidOrder(update.items, rules.items)) {
            const middle_index = update.items.len / 2;
            sum_valid += update.items[middle_index];
        }
    }
    print("Part 1 - Sum of middle numbers in valid updates: {}\n", .{sum_valid});

    // Part 2: Sum of middle numbers in corrected invalid updates
    var sum_corrected: u32 = 0;
    const sort_context = SortContext{ .rules = rules.items };

    for (updates.items) |update| {
        if (!isValidOrder(update.items, rules.items)) {
            const sorted_update = try allocator.alloc(u32, update.items.len);
            defer allocator.free(sorted_update);
            @memcpy(sorted_update, update.items);

            std.sort.block(u32, sorted_update, sort_context, SortContext.lessThan);

            const middle_index = sorted_update.len / 2;
            sum_corrected += sorted_update[middle_index];
        }
    }
    print("Part 2 - Sum of middle numbers in corrected invalid updates: {}\n", .{sum_corrected});
}