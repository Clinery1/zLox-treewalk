pub const Error = error{
    UnexpectedToken,
    UnexpectedEof,
    ExpectedClosingParen,
    ExpectedSemicolon,
    ExpectedIdentifier,
    ExpectedClosingCurly,
    InvalidLhs,
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
            Error.ExpectedSemicolon => {
                try root.report(
                    self.ctx,
                    err.token.line,
                    "Expected `;` here: `{s}`\n",
                    .{err.token.lexeme},
                );
            },
            Error.ExpectedIdentifier => {
                try root.report(
                    self.ctx,
                    err.token.line,
                    "Expected identifier here: `{s}`\n",
                    .{err.token.lexeme},
                );
            },
            Error.InvalidLhs => {
                try root.report(
                    self.ctx,
                    err.token.line,
                    "Invalid assignment LHS: `{s}`; expected Identifier\n",
                    .{err.token.lexeme},
                );
            },
            Error.ExpectedClosingCurly => {
                try root.report(
                    self.ctx,
                    err.token.line,
                    "Expected `}}` here: `{s}`\n",
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

pub fn parse(self: *@This()) !std.ArrayList(Ast.Stmt) {
    var statements = std.ArrayList(Ast.Stmt).init(self.arena.allocator());

    while (!self.isAtEnd()) {
        const stmt_opt = self.declaration() catch |err| {
            // for now, just return the error.
            return err;
        };

        if (stmt_opt) |stmt| {
            try statements.append(stmt);
        }
    }

    if (self.errors.items.len > 0) {
        return error.LoxError;
    }

    return statements;
}

fn declaration(self: *@This()) LoxAllocError!?Ast.Stmt {
    // unreachable because we are only called from a method that verifies there are tokens
    const peek_token = self.peek() orelse unreachable;
    const ret = switch (peek_token.ty) {
        .VAR => self.varStatement(),
        else => self.statement(),
    } catch |err| {
        switch (err) {
            error.LoxError => {
                try self.synchronize();
                return null;
            },
            else => return err,
        }
    };

    return ret;
}

fn varStatement(self: *@This()) !Ast.Stmt {
    if (self.match(.{.VAR})) |_| {
        const name = self.match(.{.IDENTIFIER}) orelse {
            try self.logError(Error.ExpectedIdentifier);
            return error.LoxError;
        };
        if (self.match(.{.EQUAL})) |_| {
            const expr = try self.expression();
            try self.expectSemicolon();
            return .{ .variable = .{ .name = name, .init = expr } };
        } else {
            try self.expectSemicolon();
            return .{ .variable = .{ .name = name, .init = null } };
        }
    } else unreachable;
}

fn statement(self: *@This()) !Ast.Stmt {
    // unreachable because we are only called from a method that verifies there are tokens
    const peek_token = self.peek() orelse unreachable;
    switch (peek_token.ty) {
        .PRINT => return self.printStatement(),
        .LEFT_BRACE => return self.blockStatement(),
        else => return self.expressionStatement(),
    }
}

fn blockStatement(self: *@This()) !Ast.Stmt {
    return .{ .block = try self.block() };
}

fn block(self: *@This()) !std.ArrayList(Ast.Stmt) {
    if (self.match(.{.LEFT_BRACE})) |_| {
        var block_stmts = std.ArrayList(Ast.Stmt).init(self.arena.allocator());

        while (self.peek()) |peek_token| {
            switch (peek_token.ty) {
                .RIGHT_BRACE => break,
                else => {
                    if (try self.declaration()) |stmt| {
                        try block_stmts.append(stmt);
                    }
                },
            }
        }

        if (self.match(.{.RIGHT_BRACE})) |_| {
            return block_stmts;
        }

        try self.logError(Error.UnexpectedEof);
        return error.LoxError;
    } else unreachable;
}

fn expectSemicolon(self: *@This()) !void {
    if (self.match(.{.SEMICOLON})) |_| {
        return;
    } else {
        try self.logError(Error.ExpectedSemicolon);
        return error.LoxError;
    }
}

fn printStatement(self: *@This()) !Ast.Stmt {
    if (self.match(.{.PRINT})) |_| {
        const expr = try self.expression();
        try self.expectSemicolon();
        return .{ .print = expr };
    } else unreachable;
}

fn expressionStatement(self: *@This()) !Ast.Stmt {
    const expr = try self.expression();
    try self.expectSemicolon();
    return .{ .expr = expr };
}

fn logError(self: *@This(), ty: Error) !void {
    if (self.isAtEnd()) {
        const items = self.tokens.items;
        const token = items[items.len - 1];
        try self.logErrorToken(ty, token);
    } else {
        const token = self.peek() orelse unreachable;
        try self.logErrorToken(ty, token);
    }
}

fn logErrorToken(self: *@This(), ty: Error, token: Scanner.Token) !void {
    try self.errors.append(.{ .ty = ty, .token = token });
}

fn alloc(self: *@This(), value: anytype) !*@TypeOf(value) {
    const ptr = try self.arena.allocator().create(@TypeOf(value));
    ptr.* = value;
    return ptr;
}

fn isAtEnd(self: *@This()) bool {
    return self.tokens.items[self.index].ty == .EOF or self.index >= self.tokens.items.len;
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

inline fn expression(self: *@This()) LoxAllocError!*Ast.Expr {
    return self.assignment();
}

fn assignment(self: *@This()) LoxAllocError!*Ast.Expr {
    const expr = try self.equality();

    if (self.match(.{.EQUAL})) |equals| {
        const value = try self.assignment();
        switch (expr.*) {
            .variable => |name| {
                return self.alloc(Ast.Expr{ .assign = .{ .name = name, .value = value } });
            },
            else => {
                try self.logErrorToken(Error.InvalidLhs, equals);
            },
        }
    }

    return expr;
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
        left_expr = try self.alloc(Ast.Expr{ .binary = .{
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
        return self.alloc(Ast.Expr{ .unary = .{ .operator = operator, .right = right } });
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
                return self.alloc(Ast.Expr{ .number = num });
            },
            .STRING => {
                // SAFETY: We know based on the scanner code that these are guaranteed to be
                // Some(literal) and Literal.number
                const lit = token.literal orelse unreachable;
                const string = lit.string;
                return self.alloc(Ast.Expr{ .string = string });
            },
            .NIL => {
                const ret: Ast.Expr = .nil;
                return self.alloc(ret);
            },
            .TRUE => {
                return self.alloc(Ast.Expr{ .boolean = true });
            },
            .FALSE => {
                return self.alloc(Ast.Expr{ .boolean = false });
            },
            .LEFT_PAREN => {
                const inner = try self.expression();
                const ret = try self.alloc(Ast.Expr{ .grouping = inner });
                if (self.match(.{.RIGHT_PAREN})) |_| {
                    return ret;
                } else {
                    try self.logError(Error.ExpectedClosingParen);
                    return error.LoxError;
                }
            },
            .IDENTIFIER => {
                return self.alloc(Ast.Expr{ .variable = token });
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
            .CLASS, .FUN, .VAR, .FOR, .IF, .WHILE, .PRINT, .RETURN, .EOF => return,
            else => {},
        }

        _ = self.next();
    }
}

const root = @import("root");
const std = @import("std");

const Ast = root.Ast;
const Scanner = root.Scanner;
const LoxAllocError = root.LoxAllocError;
