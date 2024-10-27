const std = @import("std");
const lex = @import("lex.zig");
const ast = @import("ast.zig");

pub const ParseError = error{
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
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    lexer: lex.Lexer,

    pub fn parse_list(self: *Parser) !ast.List {
        return try self.parse_list_inner(struct {
            fn parse_item(p: *Parser) !ast.Value {
                return try p.parse();
            }
        }, lex.LexOptions.all);
    }

    fn parse_list_inner(self: *Parser, comptime item_parser: anytype, comptime opts: lex.LexOptions) !ast.List {
        var list: ast.List = null;
        var tail: ast.List = null;
        while (try self.lexer.peek_kind(opts)) |peek_kind| {
            switch (peek_kind) {
                .close, .close_tag => break,
                .open_tag => |open| if (open.kind == lex.OpenTagKind.closing) {
                    _ = try self.lexer.next(opts);
                    if (try self.lexer.next(opts)) |peek2| switch (peek2) {
                        .close_tag => break,
                        else => return error.UnexpectedToken,
                    } else return error.UnexpectedEof;
                },
                else => {},
            }
            const value = try item_parser.parse_item(self);
            const item = try self.allocator.create(ast.ListItem);
            item.* = ast.ListItem{
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

    fn parse(self: *Parser) ParseError!ast.Value {
        switch (try self.lexer.require(lex.LexOptions.all)) {
            .open => return try self.parse_open(),
            .open_tag => |open| return try self.parse_open_tag(open.kind, open.name),
            .word => |word| return try self.parse_word(word, .{ .quoted = false, .parse_attrs = true }),
            .literal => |literal| return try self.parse_word(literal, .{ .quoted = true, .parse_attrs = true }),
            else => return error.UnexpectedToken,
        }
    }

    fn parse_open(self: *Parser) !ast.Value {
        const list = try self.parse_list();
        switch (try self.lexer.require(lex.LexOptions.all)) {
            .close => {},
            else => return error.UnexpectedToken,
        }
        if (list) |first| if (first.rest == null) {
            return first.value;
        };
        return .{ .list = list };
    }

    fn parse_open_tag(self: *Parser, open_kind: lex.OpenTagKind, name: []u8) ParseError!ast.Value {
        const attrs = try self.parse_list_inner(struct {
            fn parse_item(p: *Parser) !ast.Value {
                switch (try p.lexer.require(lex.LexOptions.all)) {
                    .open => return try p.parse_open(),
                    .word => |word| return try p.parse_word(word, .{ .quoted = true, .parse_attrs = true }),
                    .literal => |literal| return try p.parse_word(literal, .{ .quoted = true, .parse_attrs = true }),
                    else => return error.UnexpectedToken,
                }
            }
        }, lex.LexOptions.all);

        const kind: ast.NodeKind = switch (try self.lexer.require(.{ .tags = true })) {
            .close_tag => |close_kind| b: {
                if (open_kind == .opening and close_kind == .empty) {
                    break :b .empty;
                } else if (open_kind == .opening and close_kind == .normal) {
                    break :b .normal;
                } else if (open_kind == .bang and close_kind == .normal) {
                    break :b .bang;
                } else if (open_kind == .interro and close_kind == .interro) {
                    break :b .interro;
                } else return error.MismatchedTagBrackets;
            },
            else => return error.UnexpectedToken,
        };
        const children = if (kind == ast.NodeKind.normal) try self.parse_list_inner(struct {
            fn parse_item(p: *Parser) !ast.Value {
                switch (try p.lexer.require(.{ .parens = true, .tags = true, .space = true })) {
                    .open => return try p.parse_open(),
                    .open_tag => |open| return try p.parse_open_tag(open.kind, open.name),
                    .word => |word| return try p.parse_word(word, .{ .quoted = true, .parse_attrs = true }),
                    .space => |space| return .{ .string = .{ .data = space } },
                    else => return error.UnexpectedToken,
                }
            }
        }, .{ .parens = true, .tags = true, .space = true }) else null;
        return .{ .node = .{
            .name = name,
            .kind = kind,
            .attrs = attrs,
            .children = children,
        } };
    }

    fn parse_word(self: *Parser, word: []u8, comptime opts: struct { quoted: bool, parse_attrs: bool }) !ast.Value {
        if (opts.parse_attrs) if (try self.lexer.peek_kind(.{ .eq = true })) |peek_kind| switch (peek_kind) {
            .eq => {
                _ = try self.lexer.next(.{ .eq = true });
                const value = try self.allocator.create(ast.Value);
                value.* = try self.parse();
                return .{ .attr = .{ .name = word, .value = value } };
            },
            else => {},
        };
        return .{ .string = .{ .data = word, .quoted = opts.quoted } };
    }
};
