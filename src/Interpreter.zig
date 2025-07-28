const Interpreter = @This();

pub const ValueType = enum { boolean, number, string, nil };

pub const Value = union(ValueType) {
    boolean: bool,
    number: f64,
    /// Strings are owned by the value, so when the value goes out of scope they need to be
    /// deallocated.
    string: []u8,
    nil,

    pub fn deinit(self: @This(), ctx: *root.RunContext) void {
        switch (self) {
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

pub const Error = error{
    InvalidTypeCast,
    InvalidAddTypes,
    DivideByZero,
};

pub const ErrorDescription = struct {
    ty: Error,
    value: ?Value,
    token: ?Scanner.Token,
};

ctx: *root.RunContext,
error_desc: ?ErrorDescription,
expr_arena: std.heap.ArenaAllocator,

pub fn init(ctx: *root.RunContext) Interpreter {
    return .{ .ctx = ctx, .error_desc = null };
}

pub fn interpret(self: *Interpreter, expr: *Ast.Expr) !Value {
    return self.evaluateExprInner(expr);
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
        }

        if (err.value) |val| {
            val.deinit(self.ctx);
        }
    }
    self.error_desc = null;
}

fn logError(self: *Interpreter, ty: Error, value: ?Value, token: ?Scanner.Token) void {
    self.error_desc = .{ .ty = ty, .value = value, .token = token };
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
        const ptr = self.allocString(val.string);
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

fn evaluateExpr(self: *Interpreter, expr_param: *Ast.Expr) LoxAllocError!Value {
    const expr = try self.evaluateExprInner(expr_param);
    const realloc = try self.reallocValue(expr);
    _ = self.expr_arena.reset(.retain_capacity);
    return realloc;
}

fn evaluateExprInner(self: *Interpreter, expr_param: *Ast.Expr) LoxAllocError!Value {
    var expr = expr_param;
    while (true) {
        switch (expr.*) {
            .binary => return self.binaryExpr(expr),
            .unary => return self.unaryExpr(expr),
            .grouping => |inner| expr = inner,

            .number, .string, .boolean, .nil => return self.literalExpr(expr),
        }
    }
}

fn unaryExpr(self: *Interpreter, expr: *Ast.Expr) !Value {
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

fn literalExpr(self: *Interpreter, lit: *Ast.Expr) !Value {
    switch (lit.*) {
        .nil => return .nil,
        .boolean => |val| return .{ .boolean = val },
        .number => |val| return .{ .number = val },
        .string => |val| {
            const str = try self.exprAllocString(val);
            return .{ .string = str };
        },
        else => unreachable,
    }
}

/// Generic cast value types and log an error if it fails
inline fn cast(self: *Interpreter, comptime ty: ValueType, val: Value, token: ?Scanner.Token) !@FieldType(Value, @tagName(ty)) {
    switch (val) {
        ty => |cap| return cap,
        else => {
            self.logError(Error.InvalidTypeCast, val, token);
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

fn binaryExpr(self: *Interpreter, expr: *Ast.Expr) !Value {
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
                self.logError(Error.DivideByZero, null, binary.operator);
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
                errdefer left_val.deinit(self.ctx);

                const right = try self.cast(.string, right_val, binary.operator);
                const slices = .{ left, right };

                const both = try std.mem.concat(self.expr_arena.allocator(), u8, &slices);
                // We don't need to deallocate memory because we either had a global value or a
                // temporary Arena allocated string

                return .{ .string = both };
            }

            self.logError(Error.InvalidAddTypes, null, binary.operator);
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
        null => false,
        .boolean => |b| b,
        else => true,
    };
}

const root = @import("root");
const std = @import("std");

const Ast = root.Ast;
const Scanner = root.Scanner;
const LoxAllocError = root.LoxAllocError;
