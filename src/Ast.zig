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
    assign: struct {
        name: Scanner.Token,
        value: *Expr,
    },
    grouping: *Expr,

    variable: Scanner.Token,

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
        _ = fmt;
        _ = options;
        switch (self) {
            .binary => |binary| {
                try writer.print("({s} {s} {s})", .{ binary.operator.lexeme, binary.left, binary.right });
            },
            .unary => |unary| {
                try writer.print("({s} {s}", .{ unary.operator.lexeme, unary.right });
            },
            .grouping => |grouping| {
                try writer.print("(group {s})", .{grouping});
            },
            .assign => |assign| {
                try writer.print("(assign {s} {s})", .{ assign.name.lexeme, assign.value });
            },
            .variable => |token| try writer.print("{s}", .{token.lexeme}),
            .number => |number| try writer.print("{d}", .{number}),
            .string => |string| try writer.print("\"{s}\"", .{string}),
            .boolean => |boolean| try writer.print("{}", .{boolean}),
            .nil => try writer.writeAll("nil"),
        }
    }
};

pub const Stmt = union(enum) {
    print: *Expr,
    expr: *Expr,
    variable: struct {
        name: Scanner.Token,
        init: ?*Expr,
    },
    block: std.ArrayList(Stmt),

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .print => |expr| try writer.print("(print {s})\n", .{expr}),
            .expr => |expr| try writer.print("(exprStmt {s})\n", .{expr}),
            .variable => |variable| if (variable.init) |init| {
                try writer.print("(var {s} {s})\n", .{ variable.name.lexeme, init });
            } else {
                try writer.print("(var {s})\n", .{variable.name.lexeme});
            },
            .block => |block| {
                try writer.writeAll("(block\n");
                for (block.items) |item| {
                    try writer.print("{s}", .{item});
                }
                try writer.writeAll(")\n");
            },
        }
    }
};

const root = @import("root");
const std = @import("std");

const Scanner = root.Scanner;
