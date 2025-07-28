pub const Expr = union(enum) {
    binary: struct {
        left: *Expr,
        operator: Scanner.Token,
        right: *Expr,
    },
    unary: struct {
        operator: Scanner.Token,
        right: *Expr,
    },
    grouping: *Expr,

    number: f64,
    string: []const u8,
    boolean: bool,
    nil,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .binary => |binary| {
                try writer.print("({s} ", .{binary.operator.lexeme});
                try binary.left.format(fmt, options, writer);
                try writer.writeAll(" ");
                try binary.right.format(fmt, options, writer);
                try writer.writeAll(")");
            },
            .unary => |unary| {
                try writer.print("({s} ", .{unary.operator.lexeme});
                try unary.right.format(fmt, options, writer);
                try writer.writeAll(")");
            },
            .grouping => |grouping| {
                try writer.writeAll("(group ");
                try grouping.format(fmt, options, writer);
                try writer.writeAll(")");
            },
            .number => |number| try writer.print("{d}", .{number}),
            .string => |string| try writer.print("\"{s}\"", .{string}),
            .boolean => |boolean| try writer.print("{}", .{boolean}),
            .nil => try writer.writeAll("nil"),
        }
    }
};

const root = @import("root");
const std = @import("std");

const Scanner = root.Scanner;
