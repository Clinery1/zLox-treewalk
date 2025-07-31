//! All allocations go through the AST's arena, so the entire AST can be freed all at once.
//! We may not be efficient with memory usage, but we sure make it easy to free the memory.

pub const Error = error{
    UnexpectedToken,
    UnexpectedEof,
    ExpectedOpeningParen,
    ExpectedClosingParen,
    ExpectedSemicolon,
    ExpectedIdentifier,
    ExpectedClosingCurly,
    InvalidLhs,
    Unreachable,
    TooManyArgs,
};

pub const ErrorDescription = struct {
    ty: Error,
    token: Scanner.Token,
};

arena: std.heap.ArenaAllocator,
tokens: std.ArrayList(Scanner.Token),
index: usize,
errors: std.ArrayList(ErrorDescription),

/// Takes ownership of `tokens`
pub fn init(ctx: *root.RunContext, tokens: std.ArrayList(Scanner.Token)) @This() {
    return .{
        .arena = std.heap.ArenaAllocator.init(ctx.alloc),
        .tokens = tokens,
        .index = 0,
        .errors = std.ArrayList(ErrorDescription).init(ctx.alloc),
    };
}

pub fn getErrors(self: *@This()) []const ErrorDescription {
    return self.errors.items;
}

pub fn printErrors(self: *@This(), ctx: *root.RunContext) !void {
    for (self.errors.items) |err| {
        switch (err.ty) {
            Error.UnexpectedToken => {
                try root.report(
                    ctx,
                    err.token.line,
                    "Unexpected token: `{s}`\n",
                    .{err.token.lexeme},
                );
            },
            Error.UnexpectedEof => {
                try root.report(
                    ctx,
                    err.token.line,
                    "Unexpected EOF\n",
                    .{},
                );
            },
            Error.ExpectedClosingParen => {
                try root.report(
                    ctx,
                    err.token.line,
                    "Expected `)` here: `{s}`\n",
                    .{err.token.lexeme},
                );
            },
            Error.ExpectedSemicolon => {
                try root.report(
                    ctx,
                    err.token.line,
                    "Expected `;` here: `{s}`\n",
                    .{err.token.lexeme},
                );
            },
            Error.ExpectedIdentifier => {
                try root.report(
                    ctx,
                    err.token.line,
                    "Expected identifier here: `{s}`\n",
                    .{err.token.lexeme},
                );
            },
            Error.InvalidLhs => {
                try root.report(
                    ctx,
                    err.token.line,
                    "Invalid assignment LHS: `{s}`; expected Identifier\n",
                    .{err.token.lexeme},
                );
            },
            Error.ExpectedClosingCurly => {
                try root.report(
                    ctx,
                    err.token.line,
                    "Expected `}}` here: `{s}`\n",
                    .{err.token.lexeme},
                );
            },
            Error.ExpectedOpeningParen => {
                try root.report(
                    ctx,
                    err.token.line,
                    "Expected `)` here: `{s}`\n",
                    .{err.token.lexeme},
                );
            },
            Error.Unreachable => unreachable,
        }
    }
}

pub fn deinit(self: @This()) std.heap.ArenaAllocator {
    self.tokens.deinit();
    self.errors.deinit();

    return self.arena;
}

pub fn parse(self: *@This()) !std.ArrayList(Ast.Stmt) {
    var statements = std.ArrayList(Ast.Stmt).init(self.allocator());

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

fn statement(self: *@This()) LoxAllocError!Ast.Stmt {
    // unreachable because we are only called from a method that verifies there are tokens
    const peek_token = self.peek() orelse unreachable;
    switch (peek_token.ty) {
        .PRINT => return self.printStatement(),
        .LEFT_BRACE => return self.blockStatement(),
        .IF => return self.ifStatement(),
        .WHILE => return self.whileStatement(),
        .FOR => return self.forStatement(),
        else => return self.expressionStatement(),
    }
}

fn ifStatement(self: *@This()) !Ast.Stmt {
    try self.expect(.{.IF}, Error.Unreachable);

    try self.expect(.{.LEFT_PAREN}, Error.ExpectedOpeningParen);
    const condition = try self.expression();
    try self.expect(.{.RIGHT_PAREN}, Error.ExpectedClosingParen);

    const if_block = try self.statement();

    var else_block: ?*Ast.Stmt = null;
    if (self.match(.{.ELSE})) |_| {
        else_block = try self.alloc(try self.statement());
    }

    return .{ .if_else = .{
        .condition = condition,
        .block = try self.alloc(if_block),
        .else_block = else_block,
    } };
}

/// TODO: This thing
fn forStatement(self: *@This()) !Ast.Stmt {
    try self.expect(.{.FOR}, Error.Unreachable);

    try self.expect(.{.LEFT_PAREN}, Error.ExpectedOpeningParen);

    var initializer: ?Ast.Stmt = null;
    if (self.peek()) |token| {
        switch (token.ty) {
            .SEMICOLON => {},
            .VAR => initializer = try self.varStatement(),
            else => initializer = try self.expressionStatement(),
        }
    } else {
        try self.logError(Error.UnexpectedEof);
        return error.LoxError;
    }
    // we don't need a `;` expect here because the above statement parsers already consume it

    var condition = Ast.Expr{ .boolean = true };
    if (self.peek()) |token| {
        switch (token.ty) {
            .SEMICOLON => {},
            else => condition = try self.expression(),
        }
    } else {
        try self.logError(Error.UnexpectedEof);
        return error.LoxError;
    }
    try self.expect(.{.SEMICOLON}, Error.ExpectedSemicolon);

    var increment: ?Ast.Expr = null;
    if (self.peek()) |token| {
        switch (token.ty) {
            .RIGHT_PAREN => {},
            else => increment = try self.expression(),
        }
    } else {
        try self.logError(Error.UnexpectedEof);
        return error.LoxError;
    }

    try self.expect(.{.RIGHT_PAREN}, Error.ExpectedClosingParen);

    const for_block = try self.statement();

    var while_block: Ast.Stmt = undefined;
    if (increment) |cap| {
        while_block = Ast.Stmt{ .block = std.ArrayList(Ast.Stmt).init(self.allocator()) };
        try while_block.block.append(for_block);
        try while_block.block.append(Ast.Stmt{ .expr = cap });
    } else {
        while_block = for_block;
    }

    const while_loop = Ast.Stmt{ .while_loop = .{
        .condition = condition,
        .block = try self.alloc(while_block),
    } };

    if (initializer) |cap| {
        var root_block = std.ArrayList(Ast.Stmt).init(self.allocator());
        try root_block.append(cap);
        try root_block.append(while_loop);
        return .{ .block = root_block };
    } else {
        return while_loop;
    }
}

fn whileStatement(self: *@This()) !Ast.Stmt {
    try self.expect(.{.WHILE}, Error.Unreachable);
    const condition = try self.expression();
    try self.expect(.{.RIGHT_PAREN}, Error.ExpectedSemicolon);

    const while_block = try self.statement();

    return .{ .while_loop = .{
        .condition = condition,
        .block = try self.alloc(while_block),
    } };
}

fn blockStatement(self: *@This()) !Ast.Stmt {
    return .{ .block = try self.block() };
}

fn block(self: *@This()) !std.ArrayList(Ast.Stmt) {
    try self.expect(.{.LEFT_BRACE}, Error.Unreachable);
    var block_stmts = std.ArrayList(Ast.Stmt).init(self.allocator());

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

    try self.expect(.{.RIGHT_BRACE}, Error.ExpectedClosingCurly);

    return block_stmts;
}

fn expectSemicolon(self: *@This()) !void {
    try self.expect(.{.SEMICOLON}, Error.ExpectedSemicolon);
}

fn printStatement(self: *@This()) !Ast.Stmt {
    try self.expect(.{.PRINT}, Error.Unreachable);
    const expr = try self.expression();
    try self.expectSemicolon();
    return .{ .print = expr };
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

fn allocator(self: *@This()) std.mem.Allocator {
    return self.arena.allocator();
}

fn alloc(self: *@This(), value: anytype) !*@TypeOf(value) {
    const ptr = try self.allocator().create(@TypeOf(value));
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

fn expect(self: *@This(), comptime types: anytype, err: Error) !void {
    if (self.isAtEnd()) {
        try self.logError(Error.UnexpectedEof);
        return error.LoxError;
    }

    if (self.match(types) == null) {
        try self.logError(err);
        return error.LoxError;
    }
}

fn match(self: *@This(), comptime types: anytype) ?Scanner.Token {
    const token = self.peek() orelse return null;
    inline for (types) |ty| {
        if (ty == token.ty) {
            return self.next();
        }
    }
    return null;
}

fn check(self: *@This(), comptime types: anytype) bool {
    const token = self.peek() orelse return null;
    inline for (types) |ty| {
        if (ty == token.ty) {
            return true;
        }
    }
    return false;
}

inline fn expression(self: *@This()) LoxAllocError!Ast.Expr {
    return self.assignment();
}

fn assignment(self: *@This()) LoxAllocError!Ast.Expr {
    const expr = try self.orExpr();

    if (self.match(.{.EQUAL})) |equals| {
        const value = try self.assignment();
        switch (expr) {
            .variable => |name| {
                return .{ .assign = .{
                    .name = name,
                    .value = try self.alloc(value),
                } };
            },
            else => {
                try self.logErrorToken(Error.InvalidLhs, equals);
            },
        }
    }

    return expr;
}

fn orExpr(self: *@This()) LoxAllocError!Ast.Expr {
    return self.leftAssocLogicalOp(.{.OR}, andExpr);
}

fn andExpr(self: *@This()) LoxAllocError!Ast.Expr {
    return self.leftAssocLogicalOp(.{.AND}, equality);
}

const BinaryOpFn = *const fn (*@This()) LoxAllocError!Ast.Expr;
/// A helper function to reduce written code for left-associative binary operations
inline fn leftAssocBinaryOp(
    self: *@This(),
    comptime types: anytype,
    comptime parse_fn: BinaryOpFn,
) !Ast.Expr {
    var left_expr = try parse_fn(self);
    while (self.match(types)) |operator| {
        const right_expr = try parse_fn(self);
        left_expr = .{ .binary = .{
            .left = try self.alloc(left_expr),
            .operator = operator,
            .right = try self.alloc(right_expr),
        } };
    }
    return left_expr;
}
/// A helper function to reduce written code for left-associative binary operations
inline fn leftAssocLogicalOp(
    self: *@This(),
    comptime types: anytype,
    comptime parse_fn: BinaryOpFn,
) !Ast.Expr {
    var left_expr = try parse_fn(self);
    while (self.match(types)) |operator| {
        const right_expr = try parse_fn(self);
        left_expr = .{ .logical = .{
            .left = try self.alloc(left_expr),
            .operator = operator,
            .right = try self.alloc(right_expr),
        } };
    }
    return left_expr;
}

fn equality(self: *@This()) LoxAllocError!Ast.Expr {
    return self.leftAssocBinaryOp(.{ .BANG_EQUAL, .EQUAL_EQUAL }, comparison);
}

fn comparison(self: *@This()) LoxAllocError!Ast.Expr {
    return self.leftAssocBinaryOp(.{ .LESS, .GREATER, .LESS_EQUAL, .GREATER_EQUAL }, term);
}

fn term(self: *@This()) LoxAllocError!Ast.Expr {
    return self.leftAssocBinaryOp(.{ .PLUS, .MINUS }, factor);
}

fn factor(self: *@This()) LoxAllocError!Ast.Expr {
    return self.leftAssocBinaryOp(.{ .SLASH, .STAR }, unary);
}

fn unary(self: *@This()) LoxAllocError!Ast.Expr {
    if (self.match(.{ .MINUS, .BANG })) |operator| {
        const right = try self.unary();
        return .{ .unary = .{
            .operator = operator,
            .right = try self.alloc(right),
        } };
    } else {
        return self.primary();
    }
}

fn call(self: *@This()) LoxAllocError!Ast.Expr {
    var expr = try self.primary();

    while (true) {
        if (self.match(.{.LEFT_PAREN})) |_| {
            expr = self.finishCall(expr);
        } else {
            break;
        }
    }

    return expr;
}

fn finishCall(self: *@This(), callee: Ast.Expr) LoxAllocError!Ast.Expr {
    // we don't have to worry about freeing this on error because we are storing it in an arena that
    // we free all at once
    var args = std.ArrayList(Ast.Expr).init(self.allocator());

    if (!self.check(.{.RIGHT_PAREN})) {
        while (true) {
            const token = self.peek();
            args.append(try self.expression());

            if (args.items.len == 256) {
                try self.logErrorToken(Error.TooManyArgs, token.?);
            }

            if (self.match(.COMMA)) |_| {
                continue;
            } else {
                break;
            }
        }
    }

    if (self.match(.{.RIGHT_PAREN})) |paren| {
        if (args.items.len >= 255) {
            try self.logErrorToken(Error.TooManyArgs, paren);
        }

        return .{ .call = .{
            .callee = try self.alloc(callee),
            .paren = paren,
            .arguments = args,
        } };
    } else {
        try self.logError(Error.ExpectedClosingParen);
        return error.LoxError;
    }
}

fn primary(self: *@This()) LoxAllocError!Ast.Expr {
    if (self.next()) |token| {
        switch (token.ty) {
            .NUMBER => {
                // SAFETY: We know based on the scanner code that these are guaranteed to be
                // Some(literal) and Literal.number
                const lit = token.literal orelse unreachable;
                const num = lit.number;
                return .{ .number = num };
            },
            .STRING => {
                // SAFETY: We know based on the scanner code that these are guaranteed to be
                // Some(literal) and Literal.number
                const lit = token.literal orelse unreachable;
                const string = lit.string;
                return .{ .string = string };
            },
            .NIL => {
                const ret: Ast.Expr = .nil;
                return ret;
            },
            .TRUE => {
                return .{ .boolean = true };
            },
            .FALSE => {
                return .{ .boolean = false };
            },
            .LEFT_PAREN => {
                const inner = try self.expression();
                try self.expect(.{.RIGHT_PAREN}, Error.ExpectedClosingParen);
                return .{ .grouping = try self.alloc(inner) };
            },
            .IDENTIFIER => {
                return .{ .variable = token };
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
