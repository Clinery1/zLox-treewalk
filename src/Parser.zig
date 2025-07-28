pub const Error = error{
    UnexpectedToken,
    UnexpectedEof,
    ExpectedClosingParen,
};

pub const ErrorDescription = struct {
    ty: Error,
    token: Scanner.Token,
};

ctx: *root.RunContext,
arena: std.heap.ArenaAllocator,
tokens: std.ArrayList(Scanner.Token),
index: usize,
errors: std.ArrayList(ErrorDescription),

/// Takes ownership of `tokens`
pub fn init(ctx: *root.RunContext, tokens: std.ArrayList(Scanner.Token)) @This() {
    return .{
        .ctx = ctx,
        .arena = std.heap.ArenaAllocator.init(ctx.alloc),
        .tokens = tokens,
        .index = 0,
        .errors = std.ArrayList(ErrorDescription).init(ctx.alloc),
    };
}

pub fn getErrors(self: *@This()) []const ErrorDescription {
    return self.errors.items;
}

pub fn printErrors(self: *@This()) !void {
    for (self.errors.items) |err| {
        switch (err.ty) {
            Error.UnexpectedToken => {
                try root.report(
                    self.ctx,
                    err.token.line,
                    "Unexpected token: `{s}`\n",
                    .{err.token.lexeme},
                );
            },
            Error.UnexpectedEof => {
                try root.report(
                    self.ctx,
                    err.token.line,
                    "Unexpected EOF\n",
                    .{},
                );
            },
            Error.ExpectedClosingParen => {
                try root.report(
                    self.ctx,
                    err.token.line,
                    "Expected `)` here: `{s}`\n",
                    .{err.token.lexeme},
                );
            },
        }
    }
}

pub fn deinit(self: @This()) std.heap.ArenaAllocator {
    self.tokens.deinit();
    self.errors.deinit();

    return self.arena;
}

pub fn parse(self: *@This()) !*Ast.Expr {
    // For now, we don't do anything. This WILL change.
    return self.expr();
}

fn logError(self: *@This(), ty: Error) !void {
    try self.errors.append(.{ .ty = ty, .token = self.peek() orelse unreachable });
}

fn allocExpr(self: *@This(), raw_expr: Ast.Expr) !*Ast.Expr {
    const ptr = try self.arena.allocator().create(Ast.Expr);
    ptr.* = raw_expr;
    return ptr;
}

fn isAtEnd(self: *@This()) bool {
    return self.index >= self.tokens.items.len;
}

fn peek(self: *@This()) ?Scanner.Token {
    if (self.isAtEnd()) return null;
    return self.tokens.items[self.index];
}

fn next(self: *@This()) ?Scanner.Token {
    if (self.isAtEnd()) return null;
    const tok = self.tokens.items[self.index];
    self.index += 1;
    return tok;
}

// TODO: iterate these things
fn match(self: *@This(), comptime types: anytype) ?Scanner.Token {
    const token = self.peek() orelse return null;
    inline for (types) |ty| {
        if (ty == token.ty) {
            return self.next();
        }
    }
    return null;
}

inline fn expr(self: *@This()) LoxAllocError!*Ast.Expr {
    return self.equality();
}

const BinaryOpFn = *const fn (*@This()) LoxAllocError!*Ast.Expr;
/// A helper function to reduce written code for left-associative binary operations
inline fn leftAssocBinaryOp(
    self: *@This(),
    comptime types: anytype,
    comptime left: BinaryOpFn,
    comptime right: BinaryOpFn,
) !*Ast.Expr {
    var left_expr = try left(self);
    while (self.match(types)) |operator| {
        const right_expr = try right(self);
        left_expr = try self.allocExpr(.{ .binary = .{
            .left = left_expr,
            .operator = operator,
            .right = right_expr,
        } });
    }
    return left_expr;
}

fn equality(self: *@This()) LoxAllocError!*Ast.Expr {
    return self.leftAssocBinaryOp(.{ .BANG_EQUAL, .EQUAL_EQUAL }, comparison, equality);
}

fn comparison(self: *@This()) LoxAllocError!*Ast.Expr {
    return self.leftAssocBinaryOp(.{ .LESS, .GREATER, .LESS_EQUAL, .GREATER_EQUAL }, term, comparison);
}

fn term(self: *@This()) LoxAllocError!*Ast.Expr {
    return self.leftAssocBinaryOp(.{ .PLUS, .MINUS }, factor, term);
}

fn factor(self: *@This()) LoxAllocError!*Ast.Expr {
    return self.leftAssocBinaryOp(.{ .SLASH, .STAR }, unary, factor);
}

fn unary(self: *@This()) LoxAllocError!*Ast.Expr {
    if (self.match(.{ .MINUS, .BANG })) |operator| {
        const right = try self.unary();
        return self.allocExpr(.{ .unary = .{ .operator = operator, .right = right } });
    } else {
        return self.primary();
    }
}

fn primary(self: *@This()) LoxAllocError!*Ast.Expr {
    if (self.next()) |token| {
        switch (token.ty) {
            .NUMBER => {
                // SAFETY: We know based on the scanner code that these are guaranteed to be
                // Some(literal) and Literal.number
                const lit = token.literal orelse unreachable;
                const num = lit.number;
                return self.allocExpr(.{ .number = num });
            },
            .STRING => {
                // SAFETY: We know based on the scanner code that these are guaranteed to be
                // Some(literal) and Literal.number
                const lit = token.literal orelse unreachable;
                const string = lit.string;
                return self.allocExpr(.{ .string = string });
            },
            .NIL => {
                return self.allocExpr(.nil);
            },
            .TRUE => {
                return self.allocExpr(.{ .boolean = true });
            },
            .FALSE => {
                return self.allocExpr(.{ .boolean = false });
            },
            .LEFT_PAREN => {
                const inner = try self.expr();
                const ret = try self.allocExpr(.{ .grouping = inner });
                if (self.match(.{.RIGHT_PAREN})) |_| {
                    return ret;
                } else {
                    try self.logError(Error.ExpectedClosingParen);
                    return error.LoxError;
                }
            },
            else => {
                try self.logError(Error.UnexpectedToken);
                return error.LoxError;
            },
        }
    } else {
        try self.logError(Error.UnexpectedEof);
        return error.LoxError;
    }
}

fn synchronize(self: *@This()) !void {
    while (self.peek()) |token| {
        switch (token.ty) {
            .SEMICOLON => {
                _ = self.next();
                return;
            },
            .CLASS, .FUN, .VAR, .FOR, .IF, .WHILE, .PRINT, .RETURN => return,
        }

        _ = self.next();
    }
}

const root = @import("root");
const std = @import("std");

const Ast = root.Ast;
const Scanner = root.Scanner;
const LoxAllocError = root.LoxAllocError;
