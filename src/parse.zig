const std = @import("std");
const lex = @import("lex.zig");
const ast = @import("ast.zig");

pub const Parser = struct {
    allocator: std.mem.Allocator,
    lexer: lex.Lexer,

    pub fn parse_list(self: *Parser) !ast.List {
        var list: ast.List = null;
        var tail: ast.List = null;
        while (try self.lexer.peek()) |peek| {
            switch (peek) {
                .close, .close_tag => break,
                .open_tag => |open| if (open.kind == lex.OpenTagKind.closing) {
                    _ = try self.lexer.next();
                    if (try self.lexer.next()) |peek2| switch (peek2) {
                        .close_tag => break,
                        else => return error.UnexpectedToken,
                    } else return error.UnexpectedEof;
                },
                else => {},
            }
            const value = try self.parse();
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
    }!ast.Value {
        const t = try self.lexer.next();
        if (t) |token| switch (token) {
            .open => {
                const list = try self.parse_list();
                const t2 = try self.lexer.next();
                if (t2) |t22| switch (t22) {
                    .close => {},
                    else => return error.UnexpectedToken,
                } else return error.UnexpectedEof;
                if (list) |first| if (first.rest == null) {
                    return first.value;
                };
                return ast.Value{ .list = list };
            },
            .open_tag => |open| return try self.parse_tag(open.kind, open.name),
            .word => |word| {
                if (try self.lexer.peek()) |peek| switch (peek) {
                    .eq => {
                        _ = try self.lexer.next();
                        const value = try self.allocator.create(ast.Value);
                        value.* = try self.parse();
                        return ast.Value{ .attr = ast.Attr{ .name = word, .value = value } };
                    },
                    else => {},
                };
                return ast.Value{ .string = ast.String{ .data = word, .quoted = false } };
            },
            .literal => |word| return ast.Value{ .string = ast.String{ .data = word, .quoted = true } },
            else => return error.UnexpectedToken,
        } else return error.UnexpectedEof;
    }

    fn parse_tag(self: *Parser, open_kind: lex.OpenTagKind, name: []u8) !ast.Value {
        const attrs = try self.parse_list();
        const kind: ast.NodeKind = if (try self.lexer.next()) |next| switch (next) {
            .close_tag => |c| b: {
                if (open_kind == .opening and c == .empty) {
                    break :b .empty;
                } else if (open_kind == .opening and c == .normal) {
                    break :b .normal;
                } else if (open_kind == .bang and c == .normal) {
                    break :b .bang;
                } else if (open_kind == .interro and c == .interro) {
                    break :b .interro;
                } else return error.MismatchedTagBrackets;
            },
            else => return error.UnexpectedToken,
        } else return error.UnexpectedEof;
        const children = if (kind == ast.NodeKind.normal) try self.parse_list() else null;
        return ast.Value{ .node = ast.Node{
            .name = name,
            .kind = kind,
            .attrs = attrs,
            .children = children,
        } };
    }
};
