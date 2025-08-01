pub const Export = struct {
    name: []const u8,
    callable: super.LoxCallable,
};

pub const exports = [_]Export{
    .{ .name = "clock", .callable = clock },
};

pub const clock: super.LoxCallable = .{
    .data = undefined,
    .args = 0,
    .call = clockInner,
};
fn clockInner(
    _: *anyopaque,
    _: *Interpreter,
    _: []super.Value,
    _: Scanner.Token,
) LoxAllocError!super.Value {
    var millis: f64 = @floatFromInt(std.time.milliTimestamp());
    millis /= 1000.0;
    return .{ .number = millis };
}

const root = @import("root");
const std = @import("std");

const super = root.Interpreter;

const Scanner = root.Scanner;
const Interpreter = super;
const LoxAllocError = root.LoxAllocError;
