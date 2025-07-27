pub const Expr = union(enum) {
    binary: struct {
        left: *Expr,
        operator: root.token.Token,
        right: *Expr,
    },
    unary: struct {
        operator: root.token.Token,
        right: *Expr,
    },
    grouping: *Expr,
    literal: root.token.Literal,

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
            .literal => |literal| return literal.format(fmt, options, writer),
        }
    }
};

const root = @import("root");
const std = @import("std");
