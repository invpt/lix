const std = @import("std");
const lex = @import("lex.zig");
const ast = @import("ast.zig");
const parse = @import("parse.zig");
const rt = @import("rt.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();
    var args = try std.process.argsWithAllocator(allocator);
    _ = args.skip(); // skip over own path
    const filename = args.next();

    if (filename) |f| {
        const file = try std.fs.cwd().openFile(f, std.fs.File.OpenFlags{
            .mode = .read_only,
        });
        const lexer = lex.Lexer{
            .allocator = allocator,
            .src = file,
        };
        var parser = parse.Parser{
            .allocator = allocator,
            .lexer = lexer,
        };
        var context = std.StringHashMap(ast.Value).init(std.heap.page_allocator);
        for (rt.builtins.list) |builtin| {
            try context.put(builtin.name, ast.Value{ .builtin = builtin });
        }
        try context.put("true", .{ .string = .{ .data = "true", .quoted = false } });
        try context.put("false", .{ .list = null });
        var runtime = rt.Runtime{
            .allocator = std.heap.page_allocator,
            .context = rt.Context{
                .stack = b: {
                    var l = std.ArrayList(std.StringHashMap(ast.Value)).init(allocator);
                    try l.append(context);
                    break :b l;
                },
                .allocator = allocator,
            },
        };
        var list = try parser.parse_list();
        const stdout = std.io.getStdOut().writer();
        const stderr = std.io.getStdErr().writer();
        while (list) |current| {
            list = current.rest;

            const result = runtime.eval(current.value);
            switch (result) {
                .ok => |value| try output(stdout, value),
                .err => |err| try output(stderr, err),
            }
            try stdout.writeByte('\n');
        }
    }
}

fn output(stdout: std.fs.File.Writer, value: ast.Value) error{
    UnexpectedEof,
    UnexpectedToken,
    MismatchedTagBrackets,
    AccessDenied,
    Unexpected,
    SystemResources,
    IsDir,
    WouldBlock,
    InputOutput,
    OperationAborted,
    BrokenPipe,
    ConnectionResetByPeer,
    ConnectionTimedOut,
    NotOpenForReading,
    SocketNotConnected,
    OutOfMemory,
    UnrecognizedEscape,
    FileTooBig,
    NoSpaceLeft,
    DeviceBusy,
    DiskQuota,
    InvalidArgument,
    NotOpenForWriting,
    LockViolation,
}!void {
    switch (value) {
        .node => |node| {
            try stdout.writeAll(switch (node.kind) {
                .normal, .empty => "<",
                .bang => "<!",
                .interro => "<?",
            });
            try stdout.writeAll(node.name);
            if (node.attrs) |attrs| {
                try stdout.writeByte(' ');
                try outputList(stdout, attrs, ' ');
            }
            try stdout.writeAll(switch (node.kind) {
                .normal, .bang => ">",
                .empty => "/>",
                .interro => "?>",
            });
            if (node.children) |children| {
                try stdout.writeByte('\n');
                try outputList(stdout, children, '\n');
                try stdout.writeByte('\n');
            }
            switch (node.kind) {
                .normal => {
                    try stdout.writeAll("</");
                    try stdout.writeAll(node.name);
                    try stdout.writeAll(">");
                },
                else => {},
            }
        },
        .attr => |attr| {
            try stdout.writeAll(attr.name);
            try stdout.writeByte('=');
            try output(stdout, attr.value.*);
        },
        .list => |top| {
            try stdout.writeByte('(');
            try outputList(stdout, top, ' ');
            try stdout.writeByte(')');
        },
        .builtin => |builtin| try stdout.writeAll(builtin.name),
        .lambda => |lambda| {
            try stdout.writeByte('(');
            try output(stdout, lambda.param.*);
            try stdout.writeByte(' ');
            try output(stdout, lambda.body.*);
            try stdout.writeByte(')');
        },
        .string => |string| if (string.quoted) {
            // TODO: escape stuff
            try stdout.print("\"{s}\"", .{string.data});
        } else {
            try stdout.print("{s}", .{string.data});
        },
    }
}

fn outputList(stdout: std.fs.File.Writer, top: ast.List, sep: u8) !void {
    var list = top;
    var first = true;
    while (list) |current| {
        if (!first) {
            try stdout.writeByte(sep);
        }
        try output(stdout, current.value);
        first = false;
        list = current.rest;
    }
}
