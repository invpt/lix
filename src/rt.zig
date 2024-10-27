const std = @import("std");
const lex = @import("lex.zig");
const ast = @import("ast.zig");
const parse = @import("parse.zig");

const oom_error = ast.Value{ .string = ast.String{ .data = "out of memory" } };

pub const Runtime = struct {
    allocator: std.mem.Allocator,
    context: Context,

    pub fn import(self: *Runtime, sub_path: []const u8) ast.Result(ast.Value) {
        const file = std.fs.cwd().openFile(sub_path, std.fs.File.OpenFlags{
            .mode = .read_only,
        }) catch return .{ .err = .{ .string = .{ .data = "failed to open file" } } };
        const lexer = lex.Lexer{
            .allocator = self.allocator,
            .src = file,
            .space_buffer = std.ArrayList(u8).init(self.allocator),
        };
        var parser = parse.Parser{
            .allocator = self.allocator,
            .lexer = lexer,
        };
        const list = parser.parse_list() catch return .{ .err = .{ .string = .{ .data = "failed to load/lex/parse file" } } };
        return self.eval(list2value(list));
    }

    fn eval(self: *Runtime, value: ast.Value) ast.Result(ast.Value) {
        switch (value) {
            .node => |node| return ast.Result(ast.Value){ .ok = ast.Value{ .node = ast.Node{
                .name = node.name,
                .kind = node.kind,
                .attrs = switch (self.eval_all(node.attrs)) {
                    .ok => |l| l,
                    .err => |err| return ast.Result(ast.Value){ .err = err },
                },
                .children = switch (self.eval_all(node.children)) {
                    .ok => |l| l,
                    .err => |err| return ast.Result(ast.Value){ .err = err },
                },
            } } },
            .attr => |attr| {
                const new_value = self.allocator.create(ast.Value) catch return .{ .err = oom_error };
                new_value.* = switch (self.eval(attr.value.*)) {
                    .ok => |v| v,
                    .err => |err| return ast.Result(ast.Value){ .err = err },
                };
                return ast.Result(ast.Value){ .ok = .{ .attr = .{
                    .name = attr.name,
                    .value = new_value,
                } } };
            },
            .list => |list| if (list) |head| {
                const f = self.eval(head.value);
                switch (f) {
                    .ok => |v| switch (v) {
                        .builtin => |builtin| return builtin.func(self, head.rest),
                        .lambda => |lambda| return self.eval_lambda_call(lambda, list2value(head.rest)),
                        else => {
                            // eval the tail
                            const tail = switch (self.eval_all(head.rest)) {
                                .ok => |t| t,
                                .err => |err| return .{ .err = err },
                            };
                            // allocate a new list item to hold the head
                            const evaled = self.allocator.create(ast.ListItem) catch return .{ .err = oom_error };
                            evaled.* = ast.ListItem{
                                .value = v,
                                .spread = false,
                                .rest = tail,
                            };
                            // return this list!
                            return .{ .ok = .{ .list = evaled } };
                        },
                    },
                    .err => |err| return .{ .err = err },
                }
            } else {
                return .{ .ok = .{ .list = null } };
            },
            .string => |string| if (string.quoted) {
                return .{ .ok = .{ .string = string } };
            } else if (self.context.get(string.data)) |found| {
                return .{ .ok = found };
            } else {
                return .{ .err = .{ .string = .{ .data = "symbol not found" } } };
            },
            else => return .{ .ok = value },
        }
    }

    fn eval_lambda_call(self: *Runtime, lambda: ast.Lambda, arg: ast.Value) ast.Result(ast.Value) {
        self.context.push() catch return .{ .err = oom_error };
        switch (self.bind_eval(lambda.param.*, arg)) {
            .ok => {},
            .err => |err| return .{ .err = err },
        }
        const result = self.eval(lambda.body.*);
        self.context.pop();
        return result;
    }

    fn bind_eval(self: *Runtime, pat: ast.Value, arg: ast.Value) ast.Result(void) {
        switch (pat) {
            .string => |string| if (!string.quoted) {
                const evaled = switch (self.eval(arg)) {
                    .ok => |value| value,
                    .err => |err| return .{ .err = err },
                };
                self.context.put(string.data, evaled) catch return .{ .err = oom_error };
            },
            .list => |plist| switch (arg) {
                .list => |alist| {
                    var phead = plist;
                    var ahead = alist;
                    while (true) {
                        if (phead) |pcur| {
                            if (ahead) |acur| {
                                switch (self.bind_eval(pcur.value, acur.value)) {
                                    .ok => {},
                                    .err => |err| return .{ .err = err },
                                }
                                phead = pcur.rest;
                                ahead = acur.rest;
                            } else break;
                        } else break;
                    }
                    if ((phead == null) != (ahead == null)) {
                        return .{ .err = .{ .string = .{ .data = "arg cannot go into param" } } };
                    }
                },
                else => return .{ .err = .{ .string = .{ .data = "arg cannot go into param" } } },
            },
            else => return .{ .err = .{ .string = .{ .data = "invalid param" } } },
        }

        return .ok;
    }

    fn eval_all(self: *Runtime, src: ast.List) ast.Result(ast.List) {
        var attrs: ast.List = null;
        var current: ast.List = null;
        var src_currents = src;
        while (src_currents) |src_current| {
            src_currents = src_current.rest;

            const v = switch (self.eval(src_current.value)) {
                .ok => |vv| vv,
                .err => |err| return .{ .err = err },
            };

            const attr = self.allocator.create(ast.ListItem) catch return .{ .err = oom_error };
            attr.* = ast.ListItem{
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
        return .{ .ok = attrs };
    }
};

pub const Context = struct {
    allocator: std.mem.Allocator,
    stack: std.ArrayList(std.StringHashMap(ast.Value)),

    pub fn get(self: *Context, key: []const u8) ?ast.Value {
        var i = self.stack.items.len;
        while (i > 0) {
            i -= 1;

            if (self.stack.items[i].get(key)) |value| return value;
        }

        return null;
    }

    pub fn put(self: *Context, key: []const u8, value: ast.Value) std.mem.Allocator.Error!void {
        try self.stack.items[self.stack.items.len - 1].put(key, value);
    }

    pub fn push(self: *Context) std.mem.Allocator.Error!void {
        try self.stack.append(std.StringHashMap(ast.Value).init(self.allocator));
    }

    pub fn pop(self: *Context) void {
        if (self.stack.popOrNull()) |removed| {
            var r = removed;
            r.deinit();
        }
    }
};

pub const builtins = struct {
    pub const list = [_]ast.Builtin{
        .{
            .name = "import",
            .func = do_import,
        },
        .{
            .name = "let",
            .func = do_let,
        },
        .{
            .name = "do",
            .func = do_do,
        },
        .{
            .name = "if",
            .func = do_if,
        },
        .{
            .name = "fn",
            .func = do_fn,
        },
        .{
            .name = "list",
            .func = do_list,
        },
    };

    fn do_import(runtime: *Runtime, args: ast.List) ast.Result(ast.Value) {
        if (args) |first| {
            if (first.rest != null) {
                return .{ .err = .{ .string = .{ .data = "import cannot take more than one arg" } } };
            }
            const name = switch (runtime.eval(first.value)) {
                .ok => |evaled| switch (evaled) {
                    .string => |string| string.data,
                    else => return .{ .err = .{ .string = .{ .data = "import arg must be a string" } } },
                },
                .err => |err| return .{ .err = err },
            };

            return switch (runtime.import(name)) {
                .ok => |l| .{ .ok = l },
                .err => |err| .{ .err = err },
            };
        }

        return .{ .err = .{ .string = .{ .data = "import must take at least one arg" } } };
    }

    fn do_let(runtime: *Runtime, args: ast.List) ast.Result(ast.Value) {
        if (args) |first| {
            const pat = first.value;
            const val = list2value(first.rest);
            return switch (runtime.bind_eval(pat, val)) {
                .ok => .{ .ok = .{ .list = null } },
                .err => |err| .{ .err = err },
            };
        }

        return .{ .err = .{ .string = .{ .data = "not enough args for let" } } };
    }

    fn do_do(runtime: *Runtime, args: ast.List) ast.Result(ast.Value) {
        var current = args;
        var value = ast.Value{ .list = null };
        while (current) |item| {
            value = switch (runtime.eval(item.value)) {
                .ok => |v| v,
                .err => |err| return .{ .err = err },
            };
            current = item.rest;
        }
        return .{ .ok = value };
    }

    fn do_if(runtime: *Runtime, args: ast.List) ast.Result(ast.Value) {
        if (args) |first| if (first.rest) |second| if (second.rest) |third| {
            const cond = first.value;
            const on_true = second.value;
            const on_false = third.value;

            switch (runtime.eval(cond)) {
                .ok => |value| return runtime.eval(switch (value) {
                    .list => |l| if (l == null) on_false else on_true,
                    else => on_true,
                }),
                .err => |err| return .{ .err = err },
            }
        };

        return .{ .err = .{ .string = .{
            .data = "incorrect number of arguments for if expression",
        } } };
    }

    fn do_fn(runtime: *Runtime, args: ast.List) ast.Result(ast.Value) {
        if (args) |first| if (first.rest) |second| {
            const pat = runtime.allocator.create(ast.Value) catch return .{ .err = .{ .string = .{ .data = "out of memory" } } };
            const body = runtime.allocator.create(ast.Value) catch return .{ .err = .{ .string = .{ .data = "out of memory" } } };
            pat.* = first.value;
            if (second.rest == null) {
                body.* = second.value;
            } else {
                body.* = .{ .list = second };
            }

            return .{ .ok = .{ .lambda = .{
                .param = pat,
                .body = body,
            } } };
        };

        return .{ .err = .{ .string = .{
            .data = "incorrect number of arguments for fn expression",
        } } };
    }

    fn do_list(runtime: *Runtime, args: ast.List) ast.Result(ast.Value) {
        return switch (runtime.eval_all(args)) {
            .ok => |l| .{ .ok = .{ .list = l } },
            .err => |err| .{ .err = err },
        };
    }
};

fn list2value(list: ast.List) ast.Value {
    if (list) |item| if (item.rest == null) return item.value;
    return .{ .list = list };
}
