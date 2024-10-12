const std = @import("std");

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
