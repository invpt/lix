const std = @import("std");

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
        const lexer = Lexer{
            .allocator = allocator,
            .src = file,
        };
        var parser = Parser{
            .allocator = allocator,
            .lexer = lexer,
        };
        var context = Context.init(std.heap.page_allocator);
        for (builtins.list) |builtin| {
            try context.put(builtin.name, Value{ .builtin = builtin });
        }
        try context.put("true", Value{ .string = String{ .data = "true", .quoted = false } });
        try context.put("false", Value{ .list = null });
        var runtime = Runtime{
            .allocator = std.heap.page_allocator,
            .context = context,
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

const Runtime = struct {
    allocator: std.mem.Allocator,
    context: Context,

    pub fn eval(self: *Runtime, value: Value) Result {
        switch (value) {
            .node => |node| return Result{ .ok = Value{ .node = Node{
                .name = node.name,
                .kind = node.kind,
                .attrs = switch (self.eval_all(node.attrs)) {
                    .ok => |l| l,
                    .err => |err| return Result{ .err = err },
                },
                .children = switch (self.eval_all(node.children)) {
                    .ok => |l| l,
                    .err => |err| return Result{ .err = err },
                },
            } } },
            .attr => |attr| {
                const new_value = self.allocator.create(Value) catch return Result{ .err = Value{ .string = String{ .data = "out of memory" } } };
                new_value.* = switch (self.eval(attr.value.*)) {
                    .ok => |v| v,
                    .err => |err| return Result{ .err = err },
                };
                return Result{ .ok = Value{ .attr = Attr{
                    .name = attr.name,
                    .value = new_value,
                } } };
            },
            .list => |list| if (list) |head| {
                const f = self.eval(head.value);
                switch (f) {
                    .ok => |v| switch (v) {
                        .builtin => |builtin| return builtin.func(self, head.rest),
                        else => return Result{ .err = Value{ .string = String{ .data = "not callable" } } },
                    },
                    .err => |err| return Result{ .err = err },
                }
            } else {
                return Result{ .ok = Value{ .list = null } };
            },
            .builtin => return Result{ .ok = value },
            .string => |string| if (string.quoted) {
                return Result{ .ok = Value{ .string = string } };
            } else if (self.context.get(string.data)) |found| {
                return Result{ .ok = found };
            } else {
                return Result{ .err = Value{ .string = String{ .data = "symbol not found" } } };
            },
        }
    }

    fn eval_all(self: *Runtime, src: List) ResultOfList {
        var attrs: List = null;
        var current: List = null;
        var src_currents = src;
        while (src_currents) |src_current| {
            src_currents = src_current.rest;

            const v = switch (self.eval(src_current.value)) {
                .ok => |vv| vv,
                .err => |err| return ResultOfList{ .err = err },
            };

            const attr = self.allocator.create(ListItem) catch return ResultOfList{ .err = Value{ .string = String{ .data = "out of memory" } } };
            attr.* = ListItem{
                .value = v,
                .spread = false,
                .rest = null,
            };

            if (current) |c| {
                c.rest = attr;
                current = attr;
            } else {
                attrs = attr;
                current = attr;
            }
        }
        return ResultOfList{ .ok = attrs };
    }
};

const Context = std.StringHashMap(Value);

const builtins = struct {
    const list = [_]Builtin{
        Builtin{
            .name = "if",
            .func = do_if,
        },
    };

    fn do_if(runtime: *Runtime, args: List) Result {
        if (args) |first| if (first.rest) |second| if (second.rest) |third| {
            const cond = first.value;
            const on_true = second.value;
            const on_false = third.value;

            switch (runtime.eval(cond)) {
                .ok => |value| return runtime.eval(switch (value) {
                    .list => |l| if (l == null) on_false else on_true,
                    else => on_true,
                }),
                .err => |err| return Result{ .err = err },
            }
        };

        return Result{ .err = Value{ .string = String{
            .data = "incorrect number of arguments for if expression",
        } } };
    }
};

fn output(stdout: std.fs.File.Writer, value: Value) error{
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
        .string => |string| if (string.quoted) {
            // TODO: escape stuff
            try stdout.print("\"{s}\"", .{string.data});
        } else {
            try stdout.print("{s}", .{string.data});
        },
    }
}

fn outputList(stdout: std.fs.File.Writer, top: List, sep: u8) !void {
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

pub const Parser = struct {
    allocator: std.mem.Allocator,
    lexer: Lexer,

    pub fn parse_list(self: *Parser) !List {
        var list: List = null;
        var tail: List = null;
        while (try self.lexer.peek()) |peek| {
            switch (peek) {
                .close, .close_tag => break,
                .open_tag => |open| if (open.kind == OpenTagKind.closing) {
                    _ = try self.lexer.next();
                    if (try self.lexer.next()) |peek2| switch (peek2) {
                        .close_tag => break,
                        else => return error.UnexpectedToken,
                    } else return error.UnexpectedEof;
                },
                else => {},
            }
            const value = try self.parse();
            const item = try self.allocator.create(ListItem);
            item.* = ListItem{
                .value = value,
                .spread = false,
                .rest = null,
            };
            if (tail) |t| {
                t.rest = item;
                tail = item;
            } else {
                list = item;
                tail = item;
            }
        }

        return list;
    }

    fn parse(self: *Parser) error{
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
    }!Value {
        const t = try self.lexer.next();
        if (t) |token| switch (token) {
            .open => {
                const list = try self.parse_list();
                const t2 = try self.lexer.next();
                if (t2) |t22| switch (t22) {
                    .close => {},
                    else => return error.UnexpectedToken,
                } else return error.UnexpectedEof;
                return Value{ .list = list };
            },
            .open_tag => |open| return try self.parse_tag(open.kind, open.name),
            .word => |word| {
                if (try self.lexer.peek()) |peek| switch (peek) {
                    .eq => {
                        _ = try self.lexer.next();
                        const value = try self.allocator.create(Value);
                        value.* = try self.parse();
                        return Value{ .attr = Attr{ .name = word, .value = value } };
                    },
                    else => {},
                };
                return Value{ .string = String{ .data = word, .quoted = false } };
            },
            .literal => |word| return Value{ .string = String{ .data = word, .quoted = true } },
            else => return error.UnexpectedToken,
        } else return error.UnexpectedEof;
    }

    fn parse_tag(self: *Parser, open_kind: OpenTagKind, name: []u8) !Value {
        const attrs = try self.parse_list();
        const kind = if (try self.lexer.next()) |next| switch (next) {
            .close_tag => |c| b: {
                if (open_kind == OpenTagKind.opening and c == CloseTagKind.empty) {
                    break :b NodeKind.empty;
                } else if (open_kind == OpenTagKind.opening and c == CloseTagKind.normal) {
                    break :b NodeKind.normal;
                } else if (open_kind == OpenTagKind.bang and c == CloseTagKind.normal) {
                    break :b NodeKind.bang;
                } else if (open_kind == OpenTagKind.interro and c == CloseTagKind.interro) {
                    break :b NodeKind.interro;
                } else return error.MismatchedTagBrackets;
            },
            else => return error.UnexpectedToken,
        } else return error.UnexpectedEof;
        const children = if (kind == NodeKind.normal) try self.parse_list() else null;
        return Value{ .node = Node{
            .name = name,
            .kind = kind,
            .attrs = attrs,
            .children = children,
        } };
    }
};

pub const Result = union(enum) {
    ok: Value,
    err: Value,
};

pub const ResultOfList = union(enum) {
    ok: List,
    err: Value,
};

pub const Value = union(enum) {
    node: Node,
    attr: Attr,
    list: List,
    builtin: Builtin,
    string: String,
};

pub const Node = struct {
    name: []u8,
    kind: NodeKind,
    attrs: List,
    children: List,
};

pub const NodeKind = enum {
    normal,
    empty,
    bang,
    interro,
};

pub const Attr = struct {
    name: []const u8,
    value: *Value,
};

pub const List = ?*ListItem;

pub const ListItem = struct {
    value: Value,
    spread: bool,
    rest: List,
};

pub const Builtin = struct {
    name: []const u8,
    func: *const fn (*Runtime, List) Result,
};

pub const String = struct {
    data: []const u8,
    quoted: bool = true,
};

pub const Token = union(enum) {
    open,
    close,
    open_tag: struct {
        kind: OpenTagKind,
        name: []u8,
    },
    /// NOTE: close_tag only follows an unclosed open_tag!
    close_tag: CloseTagKind,
    eq,
    word: []u8,
    literal: []u8,
};

const OpenTagKind = enum {
    opening,
    closing,
    interro,
    bang,
};

const CloseTagKind = enum {
    normal,
    empty,
    interro,
};

pub const Lexer = struct {
    allocator: std.mem.Allocator,
    src: std.fs.File,
    peeked_ch: ?u8 = null,
    peeked_token: ?Token = null,
    inside_tag: bool = false,

    pub fn peek(self: *Lexer) !?Token {
        if (self.peeked_token) |peeked| {
            return peeked;
        } else {
            self.peeked_token = try self.next();
            return self.peeked_token;
        }
    }

    pub fn next(self: *Lexer) !?Token {
        if (self.peeked_token) |peeked| {
            self.peeked_token = null;
            return peeked;
        }

        while (true) {
            return switch (try self.next_ch() orelse 0) {
                0 => null,
                ' ', '\t', '\r', '\n' => continue,
                '(' => .open,
                ')' => .close,
                '<' => try self.open_tag(),
                '"' => try self.literal(),
                '=' => .eq,
                else => |first| {
                    if (self.inside_tag) out: {
                        const t = switch (first) {
                            '>' => Token{ .close_tag = .normal },
                            '/' => if (try self.peek_ch() == '>') b: {
                                _ = try self.next_ch();
                                break :b Token{ .close_tag = .empty };
                            } else {
                                break :out;
                            },
                            '?' => if (try self.peek_ch() == '>') b: {
                                _ = try self.next_ch();
                                break :b Token{ .close_tag = .interro };
                            } else {
                                break :out;
                            },
                            else => break :out,
                        };

                        self.inside_tag = false;

                        return t;
                    }

                    return Token{ .word = try self.word(first) };
                },
            };
        }
    }

    fn literal(self: *Lexer) !Token {
        var chars = std.ArrayList(u8).init(self.allocator);
        while (true) {
            const nullable = try self.next_ch();
            if (nullable == null) {
                break;
            }
            const c = nullable.?;
            switch (c) {
                '"' => break,
                '\\' => {
                    const nullable2 = try self.next_ch();
                    if (nullable2 == null) {
                        break;
                    }
                    const c2 = nullable2.?;
                    switch (c2) {
                        '"' => try chars.append('"'),
                        'n' => try chars.append('\n'),
                        'r' => try chars.append('\r'),
                        't' => try chars.append('\t'),
                        '\\' => try chars.append('\\'),
                        '0' => try chars.append(0),
                        else => return error.UnrecognizedEscape,
                    }
                },
                else => try chars.append(c),
            }
        }
        return Token{ .literal = chars.items };
    }

    fn open_tag(self: *Lexer) !Token {
        const kind: OpenTagKind = switch (try self.peek_ch() orelse 0) {
            '?' => b: {
                _ = try self.next_ch();
                break :b .interro;
            },
            '!' => b: {
                _ = try self.next_ch();
                break :b .bang;
            },
            '/' => b: {
                _ = try self.next_ch();
                break :b .closing;
            },
            else => .opening,
        };

        const name = try self.word(null);

        self.inside_tag = true;

        return .{ .open_tag = .{ .kind = kind, .name = name } };
    }

    fn word(self: *Lexer, first: ?u8) ![]u8 {
        var chars = std.ArrayList(u8).init(self.allocator);
        if (first) |f| {
            try chars.append(f);
        }
        while (true) {
            const c = try self.peek_ch() orelse 0;
            switch (c) {
                0, ' ', '\t', '\r', '\n', '(', ')', '<', '>', '=' => break,
                else => try chars.append(c),
            }
            _ = try self.next_ch();
        }
        return chars.items;
    }

    fn next_ch(self: *Lexer) !?u8 {
        if (self.peeked_ch) |c| {
            self.peeked_ch = null;
            return c;
        }

        var buffer = [1]u8{0};
        const n = try self.src.readAll(&buffer);
        if (n == 1) {
            return buffer[0];
        } else {
            return null;
        }
    }

    fn peek_ch(self: *Lexer) !?u8 {
        if (self.peeked_ch) |c| {
            return c;
        } else {
            self.peeked_ch = try self.next_ch();
            return self.peeked_ch;
        }
    }
};
