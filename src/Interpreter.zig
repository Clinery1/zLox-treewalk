const Interpreter = @This();

pub const ValueType = enum { boolean, number, string, nil };

pub const Value = union(ValueType) {
    boolean: bool,
    number: f64,
    /// Strings are owned by the value, so when the value goes out of scope they need to be
    /// deallocated.
    string: []u8,
    nil,

    pub fn deinit(self: *@This(), ctx: *root.RunContext) void {
        switch (self.*) {
            .string => |slice| ctx.alloc.free(slice),
            else => {},
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

        return switch (self) {
            .boolean => |val| writer.print("{}", .{val}),
            .number => |val| writer.print("{d}", .{val}),
            .string => |val| writer.print("{s}", .{val}),
            .nil => writer.writeAll("nil"),
        };
    }
};

pub const Environment = struct {
    ctx: *root.RunContext,
    next: ?*@This(),
    values: std.StringHashMap(Value),

    pub fn init(ctx: *root.RunContext) !*@This() {
        const ptr = try ctx.alloc.create(@This());
        ptr.* = .{
            .ctx = ctx,
            .next = null,
            .values = std.StringHashMap(Value).init(ctx.alloc),
        };
        return ptr;
    }

    pub fn initChild(ctx: *root.RunContext, parent: *@This()) !*@This() {
        const ptr = try ctx.alloc.create(@This());
        ptr.* = .{
            .ctx = ctx,
            .next = parent,
            .values = std.StringHashMap(Value).init(ctx.alloc),
        };
        return ptr;
    }

    pub fn deinit(self: *@This()) void {
        var iter = self.values.iterator();
        while (iter.next()) |kv| {
            kv.value_ptr.deinit(self.ctx);
            self.ctx.alloc.free(kv.key_ptr.*);
        }
        self.values.deinit();
        if (self.next) |next| {
            next.deinit();
            self.ctx.alloc.destroy(next);
        }
    }

    /// NOTE: Value **must** be allocated by the program allocator, not the Expr Arena. Copies the
    /// key value to an owned allocation.
    pub fn define(self: *@This(), string: []const u8, value: Value) !void {
        const ptr = try self.ctx.alloc.alloc(u8, string.len);
        std.mem.copyForwards(u8, ptr, string);

        if (self.values.fetchRemove(ptr)) |kv| {
            self.ctx.alloc.free(kv.key);
            var val = kv.value;
            val.deinit(self.ctx);
        }

        try self.values.put(ptr, value);
    }

    /// NOTE: Value **must** be allocated by the program allocator, not the Expr Arena. Copies the
    /// key value to an owned allocation.
    pub fn assign(self: *@This(), string: []const u8, value: Value) Error!void {
        if (self.values.getPtr(string)) |stored_val| {
            stored_val.deinit(self.ctx);
            stored_val.* = value;
        } else if (self.next) |next| {
            return next.assign(string, value);
        } else {
            return Error.UndefinedVariable;
        }
    }

    /// NOTE: Returns a copy of the internal value. This environment still owns it, so just discard
    /// the value when finished.
    pub fn get(self: *@This(), string: []const u8) Error!Value {
        if (self.values.get(string)) |val| {
            return val;
        } else if (self.next) |next| {
            return next.get(string);
        }
        return Error.UndefinedVariable;
    }
};

pub const Error = error{
    InvalidTypeCast,
    InvalidAddTypes,
    DivideByZero,
    UndefinedVariable,
};

pub const ErrorDescription = struct {
    ty: Error,
    token: ?Scanner.Token,
};

ctx: *root.RunContext,
error_desc: ?ErrorDescription,
expr_arena: std.heap.ArenaAllocator,
env: *Environment,

pub fn init(ctx: *root.RunContext) !Interpreter {
    return .{
        .ctx = ctx,
        .error_desc = null,
        .expr_arena = std.heap.ArenaAllocator.init(ctx.alloc),
        .env = try Environment.init(ctx),
    };
}

pub fn deinit(self: *Interpreter) void {
    self.env.deinit();
    self.ctx.alloc.destroy(self.env);
    self.expr_arena.deinit();
}

pub fn interpret(self: *Interpreter, stmts: []const Ast.Stmt) !void {
    return self.evaluateBlockInner(stmts);
}

pub fn printError(self: *Interpreter) !void {
    if (self.error_desc) |err| {
        const writer = self.ctx.stdout.writer();
        switch (err.ty) {
            Error.InvalidTypeCast => {
                if (err.token) |token| {
                    try writer.print(
                        "[{d}] Invalid type cast for operation `{s}`\n",
                        .{ token.line, token.lexeme },
                    );
                } else {
                    try writer.writeAll("Invalid type cast for operation\n");
                }
            },
            Error.InvalidAddTypes => {
                if (err.token) |token| {
                    try writer.print(
                        "[{d}] Invalid left or right type for add. Must be String or Number\n",
                        .{token.line},
                    );
                } else {
                    try writer.writeAll("Invalid left or right type for add. Must be String or Number\n");
                }
            },
            Error.DivideByZero => {
                if (err.token) |token| {
                    try writer.print(
                        "[{d}] Divide by zero\n",
                        .{token.line},
                    );
                } else {
                    try writer.writeAll("Divide by zero\n");
                }
            },
            Error.UndefinedVariable => {
                if (err.token) |token| {
                    try writer.print(
                        "[{d}] Variable `{s}` is undefined\n",
                        .{ token.line, token.lexeme },
                    );
                } else {
                    try writer.writeAll("Variable is undefined\n");
                }
            },
        }
    }
    self.error_desc = null;
}

fn evaluateBlockInner(self: *Interpreter, stmts: []const Ast.Stmt) !void {
    for (stmts) |*stmt| {
        try self.evaluateStatement(stmt);
    }
}

fn evaluateStatement(self: *Interpreter, stmt: *const Ast.Stmt) !void {
    switch (stmt.*) {
        .print => |expr| {
            var val = try self.evaluateExpr(&expr);
            try self.ctx.stdout.writer().print("{s}\n", .{val});
            val.deinit(self.ctx);
        },
        .expr => |expr| {
            var val = try self.evaluateExpr(&expr);
            val.deinit(self.ctx);
        },
        .variable => |variable| {
            var val: Value = .nil;
            if (variable.init) |var_init| {
                val = try self.evaluateExpr(&var_init);
            }
            // std.debug.print("Define var `{s}` with value `{s}`\n", .{ variable.name.lexeme, val });
            try self.env.define(variable.name.lexeme, val);
        },
        .block => |block| {
            const old_env = self.env;
            const new_env = try Environment.initChild(self.ctx, old_env);
            self.env = new_env;
            // release the environment on success or error. Defer is really nice.
            defer {
                self.env = old_env;

                new_env.next = null;
                new_env.deinit();
                self.ctx.alloc.destroy(new_env);
            }

            for (block.items) |*inner_stmt| {
                try self.evaluateStatement(inner_stmt);
            }
        },
        .if_else => |if_else| {
            const val = try self.evaluateExpr(&if_else.condition);
            if (isTruthy(val)) {
                return self.evaluateStatement(if_else.block);
            } else if (if_else.else_block) |else_block| {
                return self.evaluateStatement(else_block);
            }
        },
        .while_loop => |while_loop| {
            var condition = try self.evaluateExpr(&while_loop.condition);
            while (isTruthy(condition)) : (condition = try self.evaluateExpr(&while_loop.condition)) {
                try self.evaluateStatement(while_loop.block);
            }
        },
    }
}

fn logError(self: *Interpreter, ty: Error, token: ?Scanner.Token) void {
    self.error_desc = .{ .ty = ty, .token = token };
}

fn allocValue(self: *Interpreter, val: Value) !*Value {
    const ptr = try self.ctx.alloc.create(Value);
    ptr.* = val;
    return ptr;
}

/// If the value has an allocation, then it reallocates it in the program allocator. This bypasses
/// the resetting of the expression allocator
fn reallocValue(self: *Interpreter, val: Value) !Value {
    if (std.meta.activeTag(val) == .string) {
        const ptr = try self.allocString(val.string);
        return .{ .string = ptr };
    } else {
        return val;
    }
}

fn exprAllocString(self: *Interpreter, string: []const u8) ![]u8 {
    const ptr = try self.expr_arena.allocator().alloc(u8, string.len);
    std.mem.copyForwards(u8, ptr, string);
    return ptr;
}

fn allocString(self: *Interpreter, string: []const u8) ![]u8 {
    const ptr = try self.ctx.alloc.alloc(u8, string.len);
    std.mem.copyForwards(u8, ptr, string);
    return ptr;
}

/// Returns a value allocated with the program allocator
pub fn evaluateExpr(self: *Interpreter, expr_param: *const Ast.Expr) LoxAllocError!Value {
    const expr = try self.evaluateExprInner(expr_param);
    const realloc = try self.reallocValue(expr);
    _ = self.expr_arena.reset(.retain_capacity);
    return realloc;
}

fn evaluateExprInner(self: *Interpreter, expr_param: *const Ast.Expr) LoxAllocError!Value {
    var expr = expr_param;
    while (true) {
        switch (expr.*) {
            .binary => return self.binaryExpr(expr),
            .logical => return self.logicalExpr(expr),
            .unary => return self.unaryExpr(expr),
            .assign => return self.assignExpr(expr),
            .grouping => |inner| expr = inner,
            .call => return self.callExpr(expr),

            .number, .string, .boolean, .nil, .variable => return self.literalExpr(expr),
        }
    }
}

fn callExpr(self: *Interpreter, expr: *const Ast.Expr) !Value {
}

fn assignExpr(self: *Interpreter, expr: *const Ast.Expr) !Value {
    const assign = expr.assign;
    // This one gives us a program-allocated value that we can use in the environment
    const value = try self.evaluateExpr(assign.value);
    self.env.assign(assign.name.lexeme, value) catch |err| {
        self.logError(err, assign.name);
        return error.LoxError;
    };

    return value;
}

fn unaryExpr(self: *Interpreter, expr: *const Ast.Expr) !Value {
    const unary = expr.unary;
    const right = try self.evaluateExprInner(unary.right);

    switch (unary.operator.ty) {
        .MINUS => {
            return .{ .number = -try self.cast(.number, right, unary.operator) };
        },
        .BANG => {
            switch (right) {
                .nil => return .{ .boolean = true },
                .boolean => |val| return .{ .boolean = !val },
                else => return .{ .boolean = false },
            }
        },
        // Unreachable due to the parser producing good output.
        else => unreachable,
    }
}

fn literalExpr(self: *Interpreter, lit: *const Ast.Expr) !Value {
    switch (lit.*) {
        .nil => return .nil,
        .boolean => |val| return .{ .boolean = val },
        .number => |val| return .{ .number = val },
        .string => |val| {
            const str = try self.exprAllocString(val);
            return .{ .string = str };
        },
        .variable => |token| {
            // std.debug.print("Get variable `{s}`\n", .{token.lexeme});
            const value = self.env.get(token.lexeme) catch |err| {
                self.logError(err, token);
                return error.LoxError;
            };
            return value;
        },
        else => unreachable,
    }
}

/// Generic cast value types and log an error if it fails
inline fn cast(self: *Interpreter, comptime ty: ValueType, val: Value, token: ?Scanner.Token) !@FieldType(Value, @tagName(ty)) {
    switch (val) {
        ty => |cap| return cap,
        else => {
            self.logError(Error.InvalidTypeCast, token);
            return error.LoxError;
        },
    }
}

/// Generic cast value that returns null on failure
inline fn tryCast(comptime ty: ValueType, val: Value) ?@FieldType(Value, @tagName(ty)) {
    switch (val) {
        ty => |cap| return cap,
        else => return null,
    }
}

fn logicalExpr(self: *Interpreter, expr: *const Ast.Expr) !Value {
    const logical = expr.logical;
    const left_val = try self.evaluateExprInner(logical.left);
    switch (logical.operator.ty) {
        .AND => if (!isTruthy(left_val)) {
            return left_val;
        },
        .OR => if (isTruthy(left_val)) {
            return left_val;
        },
        else => unreachable,
    }

    return try self.evaluateExprInner(logical.right);
}

fn binaryExpr(self: *Interpreter, expr: *const Ast.Expr) !Value {
    const binary = expr.binary;
    const left_val = try self.evaluateExprInner(binary.left);
    const right_val = try self.evaluateExprInner(binary.right);

    switch (binary.operator.ty) {
        .MINUS => {
            const left = try self.cast(.number, left_val, binary.operator);
            const right = try self.cast(.number, right_val, binary.operator);
            return .{ .number = left - right };
        },
        .SLASH => {
            const left = try self.cast(.number, left_val, binary.operator);
            const right = try self.cast(.number, right_val, binary.operator);
            if (right == 0.0) {
                self.logError(Error.DivideByZero, binary.operator);
                return error.LoxError;
            }
            return .{ .number = left / right };
        },
        .STAR => {
            const left = try self.cast(.number, left_val, binary.operator);
            const right = try self.cast(.number, right_val, binary.operator);
            return .{ .number = left * right };
        },
        .PLUS => {
            if (tryCast(.number, left_val)) |left| {
                const right = try self.cast(.number, right_val, binary.operator);
                return .{ .number = left + right };
            } else if (tryCast(.string, left_val)) |left| {
                const right = try self.cast(.string, right_val, binary.operator);
                const slices = .{ left, right };

                const both = try std.mem.concat(self.expr_arena.allocator(), u8, &slices);
                // We don't need to deallocate memory because we either had a global value or a
                // temporary Arena allocated string

                return .{ .string = both };
            }

            self.logError(Error.InvalidAddTypes, binary.operator);
            return error.LoxError;
        },
        .LESS => {
            const left = try self.cast(.number, left_val, binary.operator);
            const right = try self.cast(.number, right_val, binary.operator);
            return .{ .boolean = left < right };
        },
        .LESS_EQUAL => {
            const left = try self.cast(.number, left_val, binary.operator);
            const right = try self.cast(.number, right_val, binary.operator);
            return .{ .boolean = left <= right };
        },
        .GREATER => {
            const left = try self.cast(.number, left_val, binary.operator);
            const right = try self.cast(.number, right_val, binary.operator);
            return .{ .boolean = left > right };
        },
        .GREATER_EQUAL => {
            const left = try self.cast(.number, left_val, binary.operator);
            const right = try self.cast(.number, right_val, binary.operator);
            return .{ .boolean = left >= right };
        },
        .EQUAL_EQUAL => return .{ .boolean = valueEql(left_val, right_val) },
        .BANG_EQUAL => return .{ .boolean = !valueEql(left_val, right_val) },
        else => unreachable,
    }
}

fn valueEql(left: Value, right: Value) bool {
    if (std.meta.activeTag(left) == std.meta.activeTag(right)) {
        switch (left) {
            .number => |left_num| return left_num == right.number,
            .boolean => |left_bool| return left_bool == right.boolean,
            .string => |left_str| return std.mem.eql(u8, left_str, right.string),
            .nil => return true,
        }
    } else {
        return false;
    }
}

fn isTruthy(val: Value) bool {
    return switch (val) {
        .nil => false,
        .boolean => |b| b,
        else => true,
    };
}

const root = @import("root");
const std = @import("std");

const Ast = root.Ast;
const Scanner = root.Scanner;
const LoxAllocError = root.LoxAllocError;
