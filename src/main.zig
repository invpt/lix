const std = @import("std");
const lex = @import("lex.zig");
const ast = @import("ast.zig");
const parse = @import("parse.zig");
const rt = @import("rt.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var args = try std.process.argsWithAllocator(allocator);
    _ = args.skip(); // skip over own path
    const filename = args.next();

    if (filename) |f| {
        var context = std.StringHashMap(ast.Value).init(allocator);
        for (rt.builtins.list) |builtin| {
            try context.put(builtin.name, ast.Value{ .builtin = builtin });
        }
        try context.put("true", .{ .string = .{ .data = "true", .quoted = false } });
        try context.put("false", .{ .list = null });
        var runtime = rt.Runtime{
            .allocator = allocator,
            .context = rt.Context{
                .stack = b: {
                    var l = std.ArrayList(std.StringHashMap(ast.Value)).init(allocator);
                    try l.append(context);
                    try l.append(std.StringHashMap(ast.Value).init(allocator));
                    break :b l;
                },
                .allocator = allocator,
            },
        };
        const stdout = std.io.getStdOut().writer();
        const stderr = std.io.getStdErr().writer();
        const value = switch (runtime.import(f)) {
            .ok => |l| l,
            .err => |err| {
                _ = try output(stderr, err, .keep_quotes);
                return;
            },
        };
        switch (value) {
            .list => |list| _ = try outputChildren(stdout, list),
            else => _ = try output(stdout, value, .keep_quotes),
        }
        try stdout.writeByte('\n');
    }
}

const OutputError = error{
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
};

const QuoteBehavior = enum { keep_quotes, unquote };

fn output(stdout: std.fs.File.Writer, value: ast.Value, quote_behavior: QuoteBehavior) OutputError!u8 {
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
                try outputList(stdout, attrs, .unquote);
            }
            try stdout.writeAll(switch (node.kind) {
                .normal, .bang => ">",
                .empty => " />",
                .interro => "?>",
            });
            if (node.children) |children| {
                _ = try outputChildren(stdout, children);
            }
            switch (node.kind) {
                .normal => {
                    try stdout.writeAll("</");
                    try stdout.writeAll(node.name);
                    try stdout.writeAll(">");
                },
                else => {},
            }

            return '\n';
        },
        .attr => |attr| {
            try stdout.writeAll(attr.name);
            try stdout.writeByte('=');
            _ = try output(stdout, attr.value.*, .keep_quotes);

            return ' ';
        },
        .list => |top| {
            try stdout.writeByte('(');
            try outputList(stdout, top, .keep_quotes);
            try stdout.writeByte(')');

            return ' ';
        },
        .builtin => |builtin| {
            try stdout.writeAll(builtin.name);
            return ' ';
        },
        .lambda => |lambda| {
            try stdout.writeByte('(');
            _ = try output(stdout, lambda.param.*, .keep_quotes);
            try stdout.writeByte(' ');
            _ = try output(stdout, lambda.body.*, .keep_quotes);
            try stdout.writeByte(')');
            return ' ';
        },
        .string => |string| if (string.quoted and quote_behavior == .keep_quotes) {
            // TODO: escape stuff
            try stdout.print("\"{s}\"", .{string.data});
            return ' ';
        } else {
            try stdout.print("{s}", .{string.data});
            return ' ';
        },
    }
}

fn outputList(stdout: std.fs.File.Writer, top: ast.List, quote_behavior: QuoteBehavior) !void {
    var list = top;
    var sep: ?u8 = null;
    while (list) |current| {
        if (sep) |found| {
            try stdout.writeByte(found);
        }
        sep = try output(stdout, current.value, quote_behavior);
        list = current.rest;
    }
}

fn outputChildren(stdout: std.fs.File.Writer, top: ast.List) !?u8 {
    var list = top;
    var sep: ?u8 = null;
    while (list) |current| {
        sep = switch (current.value) {
            .list => |nested| try outputChildren(stdout, nested),
            else => try output(stdout, current.value, .unquote),
        };
        list = current.rest;
    }
    return sep;
}
