/// Max file size of 64MiB. Seems reasonable for now.
const MAX_FILE_SIZE = 1024 * 1024 * 64;

pub fn main() !u8 {
    var stdout = std.io.getStdOut();

    if (std.os.argv.len > 2) {
        // If we can't write to StdOut, then we can't do anything, so ignore errors.
        stdout.writeAll("Usage: zlog [script]\n") catch {};
        return 64;
    } else if (std.os.argv.len == 2) {
        const ptr = std.mem.span(std.os.argv[0]);
        const code = runFile(ptr) catch |err| {
            stdout.writer().print("Error running file: {any}", .{err}) catch {};
            return 65;
        };
        return code;
    } else {
        const code = runPrompt() catch |err| {
            stdout.writer().print("Error running REPL: {any}", .{err}) catch {};
            return 65;
        };
        return code;
    }
}

fn runPrompt() !u8 {
    var gp_alloc = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gp_alloc.deinit();
    const stdout = std.io.getStdOut();
    const stdin = std.io.getStdIn();

    var ctx = RunContext{
        .stdout = stdout,
        .stdin = stdin,
        .alloc = gp_alloc.allocator(),
    };

    var line_buf = std.ArrayList(u8).init(ctx.alloc);
    defer line_buf.deinit();

    while (true) {
        try ctx.stdout.writeAll("> ");
        readLine(stdin.reader(), &line_buf) catch |err| switch (err) {
            error.EndOfStream => break,
            else => {
                try stdout.writer().print("Error reading Stdin: {any}", .{err});
            },
        };

        const val = run(&ctx, line_buf.items) catch |err| switch (err) {
            error.LoxError => continue,
            else => return err,
        };

        try ctx.stdout.writer().print("{s}\n", .{val});
        val.deinit(&ctx);
    }

    try ctx.stdout.writeAll("\n");
    return 0;
}

fn runFile(path: []const u8) !u8 {
    const stdout = std.io.getStdOut();
    const writer = stdout.writer();
    var gp_alloc = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gp_alloc.deinit();

    var ctx = RunContext{
        .stdout = stdout,
        .stdin = std.io.getStdIn(),
        .alloc = gp_alloc.allocator(),
    };

    var cwd = std.fs.cwd();
    const contents = try cwd.readFileAlloc(ctx.alloc, path, MAX_FILE_SIZE);
    defer ctx.alloc.destroy(&contents);

    const val = run(&ctx, contents) catch |err| switch (err) {
        error.LoxError => return 65,
        else => {
            try writer.print("Error running file: {any}", .{err});
            return 1;
        },
    };
    val.deinit(&ctx);

    return 0;
}

fn run(ctx: *RunContext, source: []const u8) !Interpreter.Value {
    var s = try Scanner.init(ctx, source);
    const tokens = try s.scanTokens();

    const ast_arena, const ast_value = parse: {
        var parser = Parser.init(ctx, tokens);
        errdefer {
            const arena = parser.deinit();
            arena.deinit();
        }

        const ast_inner = parser.parse() catch |err| {
            switch (err) {
                error.LoxError => {
                    try parser.printErrors();
                    return error.LoxError;
                },
                else => return err,
            }
        };

        break :parse .{ parser.deinit(), ast_inner };
    };
    defer ast_arena.deinit();

    // NOTE: Debug print the AST
    try ctx.stdout.writer().print("{s}\n", .{ast_value});

    var interpreter = Interpreter.init(ctx);
    const ret = interpreter.interpret(ast_value) catch |err| {
        switch (err) {
            error.LoxError => {
                try interpreter.printError();
                return error.LoxError;
            },
            else => return err,
        }
    };

    return ret;
}

fn readLine(reader: anytype, buf: *std.ArrayList(u8)) !void {
    buf.clearRetainingCapacity();
    try reader.readUntilDelimiterArrayList(buf, '\n', 1024 * 4);
}

pub fn report(ctx: *RunContext, line: u32, comptime msg: []const u8, args: anytype) !void {
    const writer = ctx.stdout.writer();
    try writer.print("[{}] Error: ", .{line});
    try writer.print(msg, args);
}
/// Never returns
pub inline fn todo(comptime msg: []const u8, args: anytype) noreturn {
    std.debug.print("TODO: ", .{});
    std.debug.print(msg, args);
    unreachable; // Not implemented
}

pub const RunContext = struct {
    stdout: std.fs.File,
    stdin: std.fs.File,
    alloc: std.mem.Allocator,
};

pub const LoxAllocError = std.mem.Allocator.Error || error{LoxError};

const std = @import("std");

pub const Scanner = @import("Scanner.zig");
pub const Ast = @import("Ast.zig");
pub const Parser = @import("Parser.zig");
pub const Interpreter = @import("Interpreter.zig");
