const Runtime = @import("rt.zig").Runtime;

pub fn Result(comptime T: type) type {
    return union(enum) {
        ok: T,
        err: Value,
    };
}

pub const Value = union(enum) {
    node: Node,
    attr: Attr,
    list: List,
    builtin: Builtin,
    lambda: Lambda,
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
    func: *const fn (*Runtime, List) Result(Value),
};

pub const Lambda = struct {
    param: *Value,
    body: *Value,
};

pub const String = struct {
    data: []const u8,
    quoted: bool = true,
};
