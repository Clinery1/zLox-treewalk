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
        .had_error = false,
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

        try run(&ctx, line_buf.items);
        ctx.had_error = false;
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
        .had_error = false,
    };

    var cwd = std.fs.cwd();
    const contents = try cwd.readFileAlloc(ctx.alloc, path, MAX_FILE_SIZE);
    defer ctx.alloc.destroy(&contents);

    run(&ctx, contents) catch |err| {
        try writer.print("Error running file: {any}", .{err});
        return 1;
    };

    if (ctx.had_error) {
        return 65;
    }

    return 0;
}

fn run(ctx: *RunContext, source: []const u8) !void {
    var scanner = try token.Scanner.init(ctx, source);
    const tokens = try scanner.scanTokens();
    defer tokens.deinit();

    const writer = ctx.stdout.writer();
    var line: u32 = 9999; // some arbitrary number that isn't 0 or 1

    for (tokens.items) |tok| {
        if (line != tok.line) {
            line = tok.line;
            try writer.print("\nLine {}\n", .{line});
        }
        try writer.print("{s}\n", .{tok});
    }
}

fn readLine(reader: anytype, buf: *std.ArrayList(u8)) !void {
    buf.clearRetainingCapacity();
    try reader.readUntilDelimiterArrayList(buf, '\n', 1024 * 4);
}

pub fn report(ctx: *RunContext, line: u32, comptime msg: []const u8, args: anytype) !void {
    const writer = ctx.stdout.writer();
    try writer.print("[{}] Error: ", .{line});
    try writer.print(msg, args);

    ctx.had_error = true;
}

pub const RunContext = struct {
    stdout: std.fs.File,
    stdin: std.fs.File,
    alloc: std.mem.Allocator,
    had_error: bool,
};

const std = @import("std");
const token = @import("token.zig");
