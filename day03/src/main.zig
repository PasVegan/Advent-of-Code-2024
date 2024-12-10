const std = @import("std");
const parseInt = std.fmt.parseInt;
const min_mul_len = 8;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        switch (gpa.deinit()) {
            .leak => std.log.err("Memory Leak!", .{}),
            .ok => {},
        }
    }

    var allocator = gpa.allocator();

    const content = try std.fs.cwd().readFileAlloc(allocator, "input.txt", std.math.maxInt(u16));
    defer allocator.free(content);

    var idx: usize = 0;
    var sum: usize = 0;

    while (idx < content.len) {
        if (content.len >= idx + min_mul_len and std.mem.eql(u8, content[idx .. idx + 3], "mul")) {
            idx += 3;
            if (content[idx] != '(') {
                continue;
            }
            idx += 1;

            const end_nb1_pos = std.mem.indexOfScalarPos(u8, content, idx,',') orelse continue;

            const num1 = parseInt(usize, content[idx..end_nb1_pos], 10) catch {
                continue;
            };

            const end_nb2_pos = std.mem.indexOfScalarPos(u8, content, end_nb1_pos + 1,')') orelse continue;

            const num2 = parseInt(usize, content[end_nb1_pos + 1..end_nb2_pos], 10) catch {
                continue;
            };

            idx = end_nb2_pos + 1;
            sum += num1 * num2;
            continue;
        }
        idx += 1;
    }

    std.log.info("Solution 1: Mul Sum: {}", .{sum});

    idx = 0;
    sum = 0;
    var enabled: bool = true;

    while (idx < content.len) {
        if (content.len >= idx + 4 and std.mem.eql(u8, content[idx .. idx + 4], "do()")) {
            enabled = true;
            idx += 4;
        }

        if (content.len >= idx + 7 and std.mem.eql(u8, content[idx .. idx + 7], "don't()")) {
            enabled = false;
            idx += 7;
        }

        if (enabled and content.len >= idx + min_mul_len and std.mem.eql(u8, content[idx .. idx + 3], "mul")) {
            idx += 3;
            if (content[idx] != '(') {
                continue;
            }
            idx += 1;

            const end_nb1_pos = std.mem.indexOfScalarPos(u8, content, idx,',') orelse continue;

            const num1 = parseInt(usize, content[idx..end_nb1_pos], 10) catch {
                continue;
            };

            const end_nb2_pos = std.mem.indexOfScalarPos(u8, content, end_nb1_pos + 1,')') orelse continue;

            const num2 = parseInt(usize, content[end_nb1_pos + 1..end_nb2_pos], 10) catch {
                continue;
            };

            idx = end_nb2_pos + 1;
            sum += num1 * num2;
            continue;
        }
        idx += 1;
    }

    std.log.info("Solution 2: Checked Mul Sum: {}", .{sum});
}