pub const Expr = union(enum) {
    binary: struct {
        left: *Expr,
        operator: Scanner.Token,
        right: *Expr,
    },
    logical: struct {
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

    call: struct {
        callee: *Expr,
        paren: Scanner.Token,
        arguments: std.ArrayList(Expr),
    },

    number: f64,
    string: []const u8,
    boolean: bool,
    nil,

    pub fn deinit(self: *@This(), alloc: std.mem.Allocator) void {
        switch (self) {
            .binary, .logical => |binary| {
                binary.left.deinit(alloc);
                alloc.destroy(binary.left);
                binary.right.deinit(alloc);
                alloc.destroy(binary.right);
            },
            .unary => |unary| {
                unary.right.deinit(alloc);
                alloc.destroy(unary.right);
            },
            .assign => |assign| {
                assign.value.deinit(alloc);
                alloc.destroy(assign.value);
            },
            .grouping => |expr| {
                expr.deinit(alloc);
                alloc.destroy(expr);
            },

            .variable, .number, .string, .boolean, .nil => {},
        }
    }

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
            .logical => |logical| {
                try writer.print("({s} {s} {s})", .{ logical.operator.lexeme, logical.left, logical.right });
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
    print: Expr,
    expr: Expr,
    variable: struct {
        name: Scanner.Token,
        init: ?Expr,
    },
    block: std.ArrayList(Stmt),
    if_else: struct {
        condition: Expr,
        block: *Stmt,
        else_block: ?*Stmt,
    },
    while_loop: struct {
        condition: Expr,
        block: *Stmt,
    },

    pub fn deinit(self: *@This(), alloc: std.mem.Allocator) void {
        switch (self) {
            .print, .expr => |expr| {
                expr.deinit(alloc);
                alloc.destroy(expr);
            },
            .variable => |variable| if (variable.init) |init| {
                init.deinit(alloc);
                alloc.destroy(init);
            },
            .block => |block| {
                for (block.items) |stmt| {
                    stmt.deinit(alloc);
                }
                block.deinit(alloc);
            },
            .if_else => |if_else| {
                if_else.condition.deinit(alloc);
                alloc.destroy(if_else.condition);
                if_else.block.deinit(alloc);
                alloc.destroy(if_else.block);
                if (if_else.else_block) |block| {
                    block.deinit(alloc);
                    alloc.destroy(block);
                }
            },
            .while_loop => |while_loop| {
                while_loop.condition.deinit(alloc);
                alloc.destroy(while_loop.condition);
                while_loop.block.deinit(alloc);
                alloc.destroy(while_loop.block);
            },
        }
    }

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
            .if_else => |if_else| {
                try writer.print("(if {s} {s}", .{ if_else.condition, if_else.block });
                if (if_else.else_block) |block| {
                    try writer.print(" else {s}", .{block});
                }
                try writer.writeAll(")\n");
            },
            .while_loop => |while_loop| {
                try writer.print("(while {s} {s})\n", .{ while_loop.condition, while_loop.block });
            },
        }
    }
};

const root = @import("root");
const std = @import("std");

const Scanner = root.Scanner;
