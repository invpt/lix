const std = @import("std");

pub const TokenKind = enum {
    space,
    open,
    close,
    open_tag,
    close_tag,
    eq,
    word,
    literal,
};

pub const TokenPeek = union(TokenKind) {
    space,
    open,
    close,
    open_tag: struct { kind: OpenTagKind },
    close_tag: CloseTagKind,
    eq,
    word,
    literal,
};

pub const Token = union(TokenKind) {
    space: []u8,
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

pub const LexOptions = struct {
    parens: bool = false,
    tags: bool = false,
    literals: bool = false,
    eq: bool = false,
    space: bool = false,

    pub const all = LexOptions{
        .parens = true,
        .tags = true,
        .literals = true,
        .eq = true,
    };

    pub const all_with_space = LexOptions{
        .parens = true,
        .tags = true,
        .literals = true,
        .eq = true,
        .space = true,
    };
};

pub const OpenTagKind = enum {
    opening,
    closing,
    interro,
    bang,
};

pub const CloseTagKind = enum {
    normal,
    empty,
    interro,
};

pub const Lexer = struct {
    allocator: std.mem.Allocator,
    src: std.fs.File,
    peek1: ?u8 = null,
    peek2: ?u8 = null,
    inside_tag: bool = false,
    space_buffer: std.ArrayList(u8),

    pub fn require(self: *Lexer, comptime opts: LexOptions) !Token {
        if (try self.next(opts)) |token| return token else return error.UnexpectedEof;
    }

    pub fn peek_kind(self: *Lexer, comptime opts: LexOptions) !?TokenPeek {
        while (try self.peek_1_ch()) |char| return switch (char) {
            ' ', '\t', '\r', '\n' => if (opts.space) .space else {
                _ = try self.next_ch();
                try self.space_buffer.append(char);
                continue;
            },
            '(' => if (opts.parens) .open else .word,
            ')' => if (opts.parens) .close else .word,
            '<' => if (opts.tags) .{ .open_tag = .{ .kind = if (try self.peek_2_ch()) |char2| switch (char2) {
                '/' => .closing,
                '?' => .interro,
                '!' => .bang,
                else => .opening,
            } else return error.UnexpectedEof } } else .word,
            '"' => if (opts.literals) .literal else .word,
            '=' => if (opts.eq) .eq else .word,
            else => |first| {
                if (self.inside_tag) switch (first) {
                    '>' => return .{ .close_tag = .normal },
                    '/' => return .{ .close_tag = .empty },
                    '?' => return .{ .close_tag = .interro },
                    else => {},
                };

                return .word;
            },
        };

        return null;
    }

    pub fn next(self: *Lexer, comptime opts: LexOptions) !?Token {
        if (opts.space) {
            while (try self.peek_1_ch()) |char| if (is_whitespace(char)) {
                _ = try self.next_ch();
                try self.space_buffer.append(char);
            } else break;
            if (self.space_buffer.items.len != 0) {
                const space = self.space_buffer.items;
                self.space_buffer = std.ArrayList(u8).init(self.allocator);
                return .{ .space = space };
            }
        } else {
            try self.space_buffer.resize(0);
            while (try self.peek_1_ch()) |char| if (is_whitespace(char)) {
                _ = try self.next_ch();
            } else break;
        }

        if (try self.next_ch()) |char| {
            if (opts.parens and char == '(') return .open;
            if (opts.parens and char == ')') return .close;
            if (opts.tags and char == '<') return try self.open_tag(opts);
            if (opts.literals and char == '"') return try self.literal();
            if (opts.eq and char == '=') return .eq;

            if (self.inside_tag) out: {
                const t = switch (char) {
                    '>' => Token{ .close_tag = .normal },
                    '/' => if (try self.peek_1_ch() == '>') b: {
                        _ = try self.next_ch();
                        break :b Token{ .close_tag = .empty };
                    } else {
                        break :out;
                    },
                    '?' => if (try self.peek_1_ch() == '>') b: {
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

            return .{ .word = try self.word(char, opts) };
        }

        return null;
    }

    fn literal(self: *Lexer) !Token {
        var chars = std.ArrayList(u8).init(self.allocator);
        while (try self.next_ch()) |char| switch (char) {
            '"' => break,
            '\\' => if (try self.next_ch()) |char2| switch (char2) {
                '"' => try chars.append('"'),
                'n' => try chars.append('\n'),
                'r' => try chars.append('\r'),
                't' => try chars.append('\t'),
                '\\' => try chars.append('\\'),
                '0' => try chars.append(0),
                else => return error.UnrecognizedEscape,
            } else return error.UnexpectedEof,
            else => try chars.append(char),
        };
        return Token{ .literal = chars.items };
    }

    fn open_tag(self: *Lexer, comptime opts: LexOptions) !Token {
        const kind: OpenTagKind = if (try self.peek_1_ch()) |char| switch (char) {
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
        } else return error.UnexpectedEof;

        const name = try self.word(null, opts);

        self.inside_tag = true;

        return .{ .open_tag = .{ .kind = kind, .name = name } };
    }

    fn word(self: *Lexer, first: ?u8, comptime opts: LexOptions) ![]u8 {
        var chars = std.ArrayList(u8).init(self.allocator);
        if (first) |f| try chars.append(f);
        while (try self.peek_1_ch()) |char| {
            if (is_whitespace(char)) break;
            if (opts.parens and (char == '(' or char == ')')) break;
            if (opts.tags and (char == '<' or char == '>')) break;
            if (opts.literals and char == '"') break;
            if (opts.eq and char == '=') break;

            try chars.append((try self.next_ch()).?);
        }
        return chars.items;
    }

    fn next_ch(self: *Lexer) !?u8 {
        if (self.pop_peek_ch()) |c| return c;

        return try self.next_ch_inner();
    }

    fn next_ch_inner(self: *Lexer) !?u8 {
        var buffer = [1]u8{0};
        const n = try self.src.readAll(&buffer);
        if (n == 1) {
            return buffer[0];
        } else {
            return null;
        }
    }

    fn peek_1_ch(self: *Lexer) !?u8 {
        if (self.peek1 == null) self.peek1 = try self.next_ch_inner();
        return self.peek1;
    }

    fn peek_2_ch(self: *Lexer) !?u8 {
        if (self.peek1 == null) self.peek1 = try self.next_ch_inner();
        if (self.peek2 == null) self.peek2 = try self.next_ch_inner();
        return self.peek2;
    }

    fn pop_peek_ch(self: *Lexer) ?u8 {
        if (self.peek1) |p| {
            self.peek1 = self.peek2;
            self.peek2 = null;
            return p;
        } else {
            return null;
        }
    }
};

fn is_whitespace(char: u8) bool {
    return switch (char) {
        ' ', '\t', '\r', '\n' => true,
        else => false,
    };
}
