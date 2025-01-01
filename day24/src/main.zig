const std = @import("std");
const Allocator = std.mem.Allocator;

const Inputs = std.StringArrayHashMap(bool);
const Exprs = std.StringArrayHashMap(Expr);

const Oper = enum {
    XOR,
    AND,
    OR
};

const Expr = struct {
    operator: Oper,
    op1: []const u8,
    op2: []const u8
};

fn resolve(key: []const u8, inputs: *Inputs, exprs: *Exprs) !bool {
    if (inputs.get(key)) |v| {
        return v;
    }

    const ex = exprs.get(key).?;

    const v1 = try resolve(ex.op1, inputs, exprs);
    const v2 = try resolve(ex.op2, inputs, exprs);

    const v = switch (ex.operator) {
        .AND => v1 and v2,
        .OR => v1 or v2,
        .XOR => v1 != v2,
    };

    try inputs.put(key, v);

    return v;
}

fn solve1(input: []const u8, alloc: Allocator) !usize {
    var inputs = Inputs.init(alloc);
    defer inputs.deinit();

    var connections = Exprs.init(alloc);
    defer connections.deinit();

    var left = std.StringArrayHashMap(void).init(alloc);
    defer left.deinit();


    var groupIter = std.mem.tokenizeSequence(u8, input, "\n\n");

    const g1 = groupIter.next() orelse return error.InvalidInput;


    {
        var inputRowIter = std.mem.tokenizeScalar(u8, g1, '\n');

        while (inputRowIter.next()) |in| {
            var inputIter = std.mem.tokenizeAny(u8, in, ": ");

            const key = inputIter.next() orelse return error.InvalidInput;
            const value = inputIter.next() orelse return error.InvalidInput;

            try inputs.put(key, value[0] == '1');
        }
    }

    const g2 = groupIter.next() orelse return error.InvalidInput;

    {
        var rowIter = std.mem.tokenizeScalar(u8, g2, '\n');

        while (rowIter.next()) |in| {
            var inputIter = std.mem.tokenizeAny(u8, in, " ->");

            const in1 = inputIter.next() orelse return error.InvalidInput;
            const op = inputIter.next() orelse return error.InvalidInput;
            const in2 = inputIter.next() orelse return error.InvalidInput;
            const out = inputIter.next() orelse return error.InvalidInput;

            const oper: Oper = switch (op[0]) {
                'X' => .XOR,
                'A' => .AND,
                else => .OR
            };

            try connections.put(out, .{ .op1 = in1, .op2 = in2, .operator = oper });
            if (out[0] == 'z') {
                try left.put(out, {});
            }
        }
    }

    var res = std.bit_set.IntegerBitSet(64).initEmpty();

    for (left.keys()) |zkey| {
        const v = try resolve(zkey, &inputs, &connections);
        const idx = try std.fmt.parseInt(usize, zkey[1..], 10);
        res.setValue(idx, v);
    }

    return res.mask;
}

const Wire = struct {
    val: bool = false,
    connections: std.ArrayList(Gate) = undefined,
};

const Gate = struct {
    operator: Oper,
    in1: []const u8,
    in2: []const u8,
    out: []const u8,
};

const WireMap = std.StringArrayHashMap(*Wire);
const GateList = std.ArrayList(Gate);
const SwapPairs = std.ArrayList([2][]const u8);

fn simulateCircuit(wires: *WireMap, gates: []const Gate, swaps: []const [2][]const u8) !void {
    // Reset wire values but keep initial inputs
    var wireIter = wires.iterator();
    while (wireIter.next()) |entry| {
        if (entry.key_ptr.*.len > 0 and (entry.key_ptr.*[0] != 'x' and entry.key_ptr.*[0] != 'y')) {
            entry.value_ptr.*.val = false;
        }
    }

    // Process gates until no changes
    var changed = true;
    while (changed) {
        changed = false;
        for (gates) |gate| {
            const in1 = wires.get(gate.in1).?.val;
            const in2 = wires.get(gate.in2).?.val;

            var outWire = gate.out;
            // Apply swaps
            for (swaps) |swap| {
                if (std.mem.eql(u8, outWire, swap[0])) {
                    outWire = swap[1];
                    break;
                } else if (std.mem.eql(u8, outWire, swap[1])) {
                    outWire = swap[0];
                    break;
                }
            }

            const newVal = switch (gate.operator) {
                .AND => in1 and in2,
                .OR => in1 or in2,
                .XOR => in1 != in2,
            };

            const wire = wires.getPtr(outWire).?;
            if (wire.*.val != newVal) {
                var newWire: Wire = .{ .val = newVal, .connections = wire.*.connections };
                try wires.put(outWire, &newWire);
                //wire.val = newVal;
                changed = true;
            }
        }
    }
}

fn checkAddition(wires: *WireMap, gates: []const Gate, swaps: []const [2][]const u8) !bool {
    var xbits: u64 = 0;
    var ybits: u64 = 0;
    var zbits: u64 = 0;

    // Get maximum bit positions
    var max_x: usize = 0;
    var max_y: usize = 0;
    var max_z: usize = 0;

    var wireIter = wires.iterator();
    while (wireIter.next()) |entry| {
        const key = entry.key_ptr.*;
        if (key.len < 3) continue;

        std.debug.print("{s}\n", .{key});
        const pos = std.fmt.parseInt(usize, key[2..], 10) catch {continue;};
        if (key[0] == 'x') {
            max_x = @max(max_x, pos);
        } else if (key[0] == 'y') {
            max_y = @max(max_y, pos);
        } else if (key[0] == 'z') {
            max_z = @max(max_z, pos);
        }
    }

    // Test multiple random input combinations
    var prng = std.rand.DefaultPrng.init(0);
    const random = prng.random();

    for (0..100) |_| {
        // Set random inputs
        wireIter = wires.iterator();
        while (wireIter.next()) |entry| {
            const key = entry.key_ptr.*;
            if (key[0] == 'x' or key[0] == 'y') {
                entry.value_ptr.*.val = random.boolean();
            }
        }

        try simulateCircuit(wires, gates, swaps);

        // Read results
        xbits = 0;
        ybits = 0;
        zbits = 0;

        wireIter = wires.iterator();
        while (wireIter.next()) |entry| {
            const key = entry.key_ptr.*;
            if (key.len < 3) continue;

            const pos = try std.fmt.parseInt(usize, key[2..], 10);
            if (entry.value_ptr.*.val) {
                if (key[0] == 'x') {
                    xbits |= @as(u64, 1) << @intCast(pos);
                } else if (key[0] == 'y') {
                    ybits |= @as(u64, 1) << @intCast(pos);
                } else if (key[0] == 'z') {
                    zbits |= @as(u64, 1) << @intCast(pos);
                }
            }
        }

        if (xbits + ybits != zbits) {
            return false;
        }
    }

    return true;
}

// Add this function at the top level
fn stringLessThan(_: void, a: []const u8, b: []const u8) bool {
    return std.mem.lessThan(u8, a, b);
}

fn solve2(input: []const u8, alloc: Allocator) ![]const u8 {
    var wires = WireMap.init(alloc);
    defer wires.deinit();

    var gates = GateList.init(alloc);
    defer gates.deinit();

    // Parse input similar to part 1
    var groupIter = std.mem.tokenizeSequence(u8, input, "\n\n");
    const g1 = groupIter.next() orelse return error.InvalidInput;

    {
        var inputRowIter = std.mem.tokenizeScalar(u8, g1, '\n');
        while (inputRowIter.next()) |in| {
            var inputIter = std.mem.tokenizeAny(u8, in, ": ");
            const key = inputIter.next() orelse return error.InvalidInput;
            const value = inputIter.next() orelse return error.InvalidInput;

            const wire = try alloc.create(Wire);
            wire.* = .{ .val = value[0] == '1' };
            try wires.put(key, wire);
        }
    }

    // Track output wires
    var outputWires = std.StringHashMap(void).init(alloc);
    defer outputWires.deinit();

    const g2 = groupIter.next() orelse return error.InvalidInput;

    {
        var rowIter = std.mem.tokenizeScalar(u8, g2, '\n');
        while (rowIter.next()) |in| {
            var inputIter = std.mem.tokenizeAny(u8, in, " ->");
            const in1 = inputIter.next() orelse return error.InvalidInput;
            const op = inputIter.next() orelse return error.InvalidInput;
            const in2 = inputIter.next() orelse return error.InvalidInput;
            const out = inputIter.next() orelse return error.InvalidInput;

            if (!wires.contains(out)) {
                const wire = try alloc.create(Wire);
                wire.* = .{ .val = false };
                try wires.put(out, wire);
            }

            try outputWires.put(out, {});

            const oper: Oper = switch (op[0]) {
                'X' => .XOR,
                'A' => .AND,
                else => .OR
            };

            try gates.append(.{
                .operator = oper,
                .in1 = in1,
                .in2 = in2,
                .out = out,
            });
        }
    }

    // Find all possible wire pairs to swap (only considering gate outputs)
    var potentialSwaps = std.ArrayList([2][]const u8).init(alloc);
    defer potentialSwaps.deinit();

    var outputWiresList = std.ArrayList([]const u8).init(alloc);
    defer outputWiresList.deinit();

    var outputIter = outputWires.keyIterator();
    while (outputIter.next()) |key| {
        try outputWiresList.append(key.*);
    }

    // Sort output wires for consistent ordering
    std.sort.heap([]const u8, outputWiresList.items, {}, stringLessThan);

    // Generate pairs only from output wires
    for (outputWiresList.items, 0..) |wire1, i| {
        for (outputWiresList.items[i+1..]) |wire2| {
            try potentialSwaps.append(.{ wire1, wire2 });
        }
    }

    // Try combinations of 4 swaps
    var result = std.ArrayList(u8).init(alloc);
    defer result.deinit();

    var swaps = SwapPairs.init(alloc);
    defer swaps.deinit();

    const n = potentialSwaps.items.len;
    for (0..n-3) |i| {
        for (i+1..n-2) |j| {
            for (j+1..n-1) |k| {
                for (k+1..n) |l| {
                    swaps.clearRetainingCapacity();
                    try swaps.append(potentialSwaps.items[i]);
                    try swaps.append(potentialSwaps.items[j]);
                    try swaps.append(potentialSwaps.items[k]);
                    try swaps.append(potentialSwaps.items[l]);

                    if (try checkAddition(&wires, gates.items, swaps.items)) {
                        // Found correct swaps, build result string
                        var wires_involved = std.ArrayList([]const u8).init(alloc);
                        defer wires_involved.deinit();

                        for (swaps.items) |swap| {
                            try wires_involved.append(swap[0]);
                            try wires_involved.append(swap[1]);
                        }
                        std.sort.heap([]const u8, wires_involved.items, {}, stringLessThan);

                        for (wires_involved.items, 0..) |wire, idx| {
                            try result.appendSlice(wire);
                            if (idx < wires_involved.items.len - 1) {
                                try result.append(',');
                            }
                        }

                        return result.toOwnedSlice();
                    }
                }
            }
        }
    }

    return error.NoSolutionFound;
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

    // const res2 = try solve2(buffer, gpa);

    // std.debug.print("Part 2: {d}\n", .{res2});
}
