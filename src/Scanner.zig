//! Supports UTF-8 characters
ctx: *root.RunContext,
source: []const u8,
index: usize,
start: usize,
line: u32,
tokens: std.ArrayList(Token),

/// Returns a new Scanner if the source is valid UTF-8 and an error otherwise.
pub fn init(ctx: *root.RunContext, source: []const u8) !@This() {
    if (!std.unicode.utf8ValidateSlice(source)) {
        return error.InvalidUtf8;
    }

    return .{
        .ctx = ctx,
        .source = source,
        .index = 0,
        .start = 0,
        .line = 1,
        .tokens = std.ArrayList(Token).init(ctx.alloc),
    };
}

pub fn scanTokens(self: *@This()) !std.ArrayList(Token) {
    while (!self.isAtEnd()) {
        self.start = self.index;
        try self.scanToken();
    }

    try self.tokens.append(Token.init(.EOF, "", null, self.line));

    return self.tokens;
}

fn isAtEnd(self: *@This()) bool {
    return self.index >= self.source.len;
}

/// Assumes self.source is valid UTF-8
fn peekChar(self: *@This()) ?u21 {
    if (self.isAtEnd()) return null;

    const char_len = std.unicode.utf8ByteSequenceLength(self.source[self.index]) catch unreachable;
    return switch (char_len) {
        1 => self.source[self.index],
        2 => std.unicode.utf8Decode2([2]u8{
            self.source[self.index],
            self.source[self.index + 1],
        }) catch unreachable,
        3 => std.unicode.utf8Decode3([3]u8{
            self.source[self.index],
            self.source[self.index + 1],
            self.source[self.index + 2],
        }) catch unreachable,
        4 => std.unicode.utf8Decode4([4]u8{
            self.source[self.index],
            self.source[self.index + 1],
            self.source[self.index + 2],
            self.source[self.index + 3],
        }) catch unreachable,
        else => unreachable, // Unicode can only be 1, 2, 3, or 4 bytes long
    };
}
/// Assumes self.source is valid UTF-8
fn peekNextChar(self: *@This()) ?u21 {
    if (self.isAtEnd()) return null;

    const char_len = std.unicode.utf8ByteSequenceLength(self.source[self.index]) catch unreachable;
    const after = self.source[self.index + char_len ..];
    if (after.len == 0) { // check if we are at the last character
        return null;
    }
    const char_next_len = std.unicode.utf8ByteSequenceLength(after[0]) catch unreachable;
    return switch (char_next_len) {
        1 => self.source[self.index + char_len],
        2 => std.unicode.utf8Decode2([2]u8{
            self.source[self.index + char_len],
            self.source[self.index + 1 + char_len],
        }) catch unreachable,
        3 => std.unicode.utf8Decode3([3]u8{
            self.source[self.index + char_len],
            self.source[self.index + 1 + char_len],
            self.source[self.index + 2 + char_len],
        }) catch unreachable,
        4 => std.unicode.utf8Decode4([4]u8{
            self.source[self.index + char_len],
            self.source[self.index + 1 + char_len],
            self.source[self.index + 2 + char_len],
            self.source[self.index + 3 + char_len],
        }) catch unreachable,
        else => unreachable, // Unicode can only be 1, 2, 3, or 4 bytes long
    };
}

/// Assumes self.source is valid UTF-8
fn nextChar(self: *@This()) ?u21 {
    if (self.isAtEnd()) return null;

    const char_len = std.unicode.utf8ByteSequenceLength(self.source[self.index]) catch unreachable;
    const char = switch (char_len) {
        1 => self.source[self.index],
        2 => std.unicode.utf8Decode2([2]u8{
            self.source[self.index],
            self.source[self.index + 1],
        }) catch unreachable,
        3 => std.unicode.utf8Decode3([3]u8{
            self.source[self.index],
            self.source[self.index + 1],
            self.source[self.index + 2],
        }) catch unreachable,
        4 => std.unicode.utf8Decode4([4]u8{
            self.source[self.index],
            self.source[self.index + 1],
            self.source[self.index + 2],
            self.source[self.index + 3],
        }) catch unreachable,
        else => unreachable, // Unicode can only be 1, 2, 3, or 4 bytes long
    };

    self.index += char_len;
    return char;
}

fn match(self: *@This(), c: u21) bool {
    if (self.peekChar()) |peek| {
        if (peek == c) {
            _ = self.nextChar();
            return true;
        }
    }
    return false;
}

inline fn addToken(self: *@This(), ty: TokenType) !void {
    try self.addTokenLiteral(ty, null);
}

fn addTokenLiteral(self: *@This(), ty: TokenType, literal: ?Literal) !void {
    try self.tokens.append(Token.init(
        ty,
        self.source[self.start..self.index],
        literal,
        self.line,
    ));
}

fn scanToken(self: *@This()) !void {
    if (self.nextChar()) |char| {
        switch (char) {
            '(' => try self.addToken(.LEFT_PAREN),
            ')' => try self.addToken(.RIGHT_PAREN),
            '{' => try self.addToken(.LEFT_BRACE),
            '}' => try self.addToken(.RIGHT_BRACE),
            ',' => try self.addToken(.COMMA),
            '.' => try self.addToken(.DOT),
            '-' => try self.addToken(.MINUS),
            '+' => try self.addToken(.PLUS),
            ';' => try self.addToken(.SEMICOLON),
            '*' => try self.addToken(.STAR),
            '!' => {
                if (self.match('=')) {
                    try self.addToken(.BANG_EQUAL);
                } else {
                    try self.addToken(.BANG);
                }
            },
            '=' => {
                if (self.match('=')) {
                    try self.addToken(.EQUAL_EQUAL);
                } else {
                    try self.addToken(.EQUAL);
                }
            },
            '<' => {
                if (self.match('=')) {
                    try self.addToken(.LESS_EQUAL);
                } else {
                    try self.addToken(.LESS);
                }
            },
            '>' => {
                if (self.match('=')) {
                    try self.addToken(.GREATER_EQUAL);
                } else {
                    try self.addToken(.GREATER);
                }
            },
            '/' => {
                if (self.match('/')) {
                    while (self.peekChar()) |c| {
                        if (c == '\n') break;
                        _ = self.nextChar();
                    }
                } else {
                    try self.addToken(.SLASH);
                }
            },
            ' ', '\t', '\r' => {},
            '\n' => {
                self.line += 1;
            },
            '"' => try self.string(),
            else => {
                if (isDigit(char)) {
                    try self.number();
                } else if (isAlpha(char)) {
                    while (self.peekChar()) |c| {
                        if (!isAlphaNumeric(c)) break;
                        _ = self.nextChar();
                    }

                    try self.matchIdentifier();
                } else {
                    var buf = [4]u8{ 0, 0, 0, 0 };
                    // We have a sufficiently sized buffer and we are not supporting surrogate
                    // pairs, so we can do an unreachable
                    _ = std.unicode.utf8Encode(char, &buf) catch unreachable;
                    try root.report(self.ctx, self.line, "Unexpected character: `{s}`", .{std.unicode.fmtUtf8(&buf)});
                }
            },
        }
    }
}

fn isDigit(char: u21) bool {
    return char >= '0' and char <= '9';
}

fn isAlpha(char: u21) bool {
    const upper = char >= 'A' and char <= 'Z';
    const lower = char >= 'a' and char <= 'z';
    return upper or lower or char == '_';
}

inline fn isAlphaNumeric(char: u21) bool {
    return isAlpha(char) or isDigit(char);
}

fn matchIdentifier(self: *@This()) !void {
    const lexeme = self.source[self.start..self.index];
    const KEYWORDS = [_]struct { []const u8, TokenType }{
        .{ "and", .AND },
        .{ "class", .CLASS },
        .{ "else", .ELSE },
        .{ "false", .FALSE },
        .{ "for", .FOR },
        .{ "fun", .FUN },
        .{ "if", .IF },
        .{ "nil", .NIL },
        .{ "or", .OR },
        .{ "print", .PRINT },
        .{ "return", .RETURN },
        .{ "super", .SUPER },
        .{ "this", .THIS },
        .{ "true", .TRUE },
        .{ "var", .VAR },
        .{ "while", .WHILE },
    };

    // test if lexeme is a keyword
    for (KEYWORDS) |tuple| {
        const keyword, const ty = tuple;
        if (std.mem.eql(u8, lexeme, keyword)) {
            try self.addToken(ty);
            return;
        }
    }

    // otherwise it is a user identifier
    try self.addToken(.IDENTIFIER);
}

fn number(self: *@This()) !void {
    while (self.peekChar()) |char| {
        if (!isDigit(char)) {
            break;
        }
        _ = self.nextChar();
    }

    if (self.peekChar()) |peek| {
        if (self.peekNextChar()) |next_peek| {
            if (peek == '.' and isDigit(next_peek)) {
                _ = self.nextChar();

                while (self.peekChar()) |char| {
                    if (!isDigit(char)) {
                        break;
                    }
                    _ = self.nextChar();
                }
            }
        }
    }

    const lexeme = self.source[self.start..self.index];
    try self.addTokenLiteral(.NUMBER, .{ .number = try std.fmt.parseFloat(f64, lexeme) });
}

fn string(self: *@This()) !void {
    // eat chars till we get the ending quote. If a newline is encountered, then increment
    // `self.line`
    var good = false;
    while (self.nextChar()) |char| {
        if (char == '"') {
            good = true;
            break;
        }
        if (char == '\n') {
            self.line += 1;
        }
    }

    // EOF detection
    if (!good and self.isAtEnd()) {
        try root.report(self.ctx, self.line, "Unterminated string", .{});
    }

    const str = self.source[self.start + 1 .. self.index - 1];
    try self.addTokenLiteral(.STRING, .{ .string = str });
}

// Caps to allow Zig keywords and copy the style of the book
pub const TokenType = enum {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    EOF,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try switch (self) {
            // Single-character tokens.
            .LEFT_PAREN => writer.writeAll("left paren"),
            .RIGHT_PAREN => writer.writeAll("right paren"),
            .LEFT_BRACE => writer.writeAll("left brace"),
            .RIGHT_BRACE => writer.writeAll("right brace"),
            .COMMA => writer.writeAll("comma"),
            .DOT => writer.writeAll("dot"),
            .MINUS => writer.writeAll("minus"),
            .PLUS => writer.writeAll("plus"),
            .SEMICOLON => writer.writeAll("semicolon"),
            .SLASH => writer.writeAll("slash"),
            .STAR => writer.writeAll("star"),

            // One or two character tokens.
            .BANG => writer.writeAll("bang"),
            .BANG_EQUAL => writer.writeAll("bang equal"),
            .EQUAL => writer.writeAll("equal"),
            .EQUAL_EQUAL => writer.writeAll("equal equal"),
            .GREATER => writer.writeAll("greater"),
            .GREATER_EQUAL => writer.writeAll("greater equal"),
            .LESS => writer.writeAll("less"),
            .LESS_EQUAL => writer.writeAll("less equal"),

            // Literals.
            .IDENTIFIER => writer.writeAll("identifier"),
            .STRING => writer.writeAll("string"),
            .NUMBER => writer.writeAll("number"),

            // Keywords.
            .AND => writer.writeAll("and"),
            .CLASS => writer.writeAll("class"),
            .ELSE => writer.writeAll("else"),
            .FALSE => writer.writeAll("false"),
            .FUN => writer.writeAll("fun"),
            .FOR => writer.writeAll("for"),
            .IF => writer.writeAll("if"),
            .NIL => writer.writeAll("nil"),
            .OR => writer.writeAll("or"),
            .PRINT => writer.writeAll("print"),
            .RETURN => writer.writeAll("return"),
            .SUPER => writer.writeAll("super"),
            .THIS => writer.writeAll("this"),
            .TRUE => writer.writeAll("true"),
            .VAR => writer.writeAll("var"),
            .WHILE => writer.writeAll("while"),

            .EOF => writer.writeAll("EOF"),
        };
    }
};

pub const Literal = union(enum) {
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
            .number => |val| try writer.print("{d}", .{val}),
            .string => |val| try writer.print("{s}", .{val}),
            .boolean => |val| try writer.print("{}", .{val}),
            .nil => try writer.writeAll("nil"),
        }
    }
};

pub const Token = struct {
    ty: TokenType,
    lexeme: []const u8,
    literal: ?Literal,
    line: u32,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s} \"{s}\"", .{ self.ty, self.lexeme });
        if (self.literal) |literal| {
            try writer.print(" = {s}", .{literal});
        }
    }

    pub fn init(ty: TokenType, lexeme: []const u8, literal: ?Literal, line: u32) Token {
        return .{ .ty = ty, .lexeme = lexeme, .literal = literal, .line = line };
    }
};

const std = @import("std");
const root = @import("root");
