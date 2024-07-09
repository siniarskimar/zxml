const std = @import("std");

pub const XmlDecl = struct {
    version: std.SemanticVersion,
    encoding: ?[]const u8,

    pub const Error = error{
        InvalidTag,
        BadTagName,
        MissingTagName,
        BadXmlVersion,
        MissingXmlVersion,
        EndOfStream,
    };

    const ParseFnError = Error || ParseNameError || Attribute.Error || std.mem.Allocator.Error;

    pub fn parse(allocator: std.mem.Allocator, reader: anytype) (ParseFnError || @TypeOf(reader).Error)!@This() {
        var buffer = std.ArrayList(u8).init(allocator);
        defer buffer.deinit();

        parseName(reader, buffer.writer()) catch |err| switch (err) {
            error.InvalidChar,
            error.InvalidStartChar,
            error.Equals,
            error.ForwardSlash,
            error.GreaterThan,
            => {
                // std.log.debug("{s}", .{buffer.items});
                return error.BadTagName;
            },

            error.QuestionMark => {},
            else => return err,
        };
        if (buffer.items.len == 0) return error.MissingTagName;

        if (!std.mem.eql(u8, buffer.items, "xml")) {
            std.debug.panic("Unsuppoted tag <?{s} ?>", .{buffer.items});
        }

        var got_version = false;
        var tag: @This() = undefined;
        tag.encoding = null;

        while (true) {
            buffer.clearRetainingCapacity();
            const char = try skipWhitespace(reader);
            if (char == '?') {
                break;
            }
            try buffer.append(char);
            const attribute = try Attribute.parse(reader, &buffer);
            if (std.mem.eql(u8, attribute.name, "version")) {
                var split_it = std.mem.splitScalar(u8, attribute.value, '.');
                const major: usize = std.fmt.parseInt(usize, split_it.next() orelse return error.BadXmlVersion, 10) catch return error.BadXmlVersion;
                const minor: usize = std.fmt.parseInt(usize, split_it.next() orelse return error.BadXmlVersion, 10) catch return error.BadXmlVersion;

                if (split_it.next()) |_| return error.BadXmlVersion;
                tag.version = .{ .major = major, .minor = minor, .patch = 0 };
                got_version = true;
                continue;
            }
            if (std.mem.eql(u8, attribute.name, "encoding")) {
                const lower = std.ascii.lowerString(@constCast(attribute.value), attribute.value);
                tag.encoding = try allocator.dupe(u8, lower);
                continue;
            }
        }

        if (!got_version) return error.MissingXmlVersion;
        return tag;
    }

    pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
        if (self.encoding) |encoding| allocator.free(encoding);
    }
};

pub const Attribute = struct {
    name: []const u8,
    value: []const u8,

    pub const Error = error{
        BadAttributeName,
        MissingAttributeName,
        MissingAttributeValue,
        ExpectedEquals,
        BadValueQuote,
        UnenclosedAttributeValue,
    };

    const BufferError = std.ArrayList(u8).Writer.Error;
    const ParseFnError = Error || BufferError || error{EndOfStream};

    pub fn parse(reader: anytype, buffer: *std.ArrayList(u8)) (ParseFnError || @TypeOf(reader).Error)!@This() {
        var encoutered_equals = false;

        var tag: @This() = undefined;

        const parse_name_result = if (buffer.items.len == 0)
            parseName(reader, buffer.writer())
        else
            parseNamePastStart(reader, buffer.writer());

        parse_name_result catch |err| switch (err) {
            error.InvalidChar,
            error.InvalidStartChar,
            error.QuestionMark,
            => return error.BadAttributeName,

            error.Equals => encoutered_equals = true,

            error.ForwardSlash,
            error.GreaterThan,
            => return error.MissingAttributeValue,

            error.EndOfStream => return error.EndOfStream,
            error.OutOfMemory => return error.OutOfMemory,
        };

        if (buffer.items.len == 0) return error.MissingAttributeName;
        const name_end_idx = buffer.items.len;

        // Many edge cases
        // name='value'
        // name ='value'
        // name= 'value'
        // name = 'value'
        // name  ='value' (reject double+ ws)
        // name =  'value' (reject double+ ws)
        // name (reject no equals)
        // name = (reject no value)
        if (!encoutered_equals) {
            // Consumed whitespace
            if (try reader.readByte() != '=') return error.ExpectedEquals;
        }
        var quote = reader.readByte() catch |err| switch (err) {
            error.EndOfStream => return error.MissingAttributeValue,
            else => return err,
        };
        if (isWhitespace(quote)) {
            if (quote == '\n') return error.MissingAttributeValue;
            quote = reader.readByte() catch |err| switch (err) {
                error.EndOfStream => return error.MissingAttributeValue,
                else => return err,
            };
        }
        if (quote != '\'' and quote != '"') return error.BadValueQuote;
        const value_start_idx = buffer.items.len;

        reader.streamUntilDelimiter(buffer.writer(), quote, null) catch |err| switch (err) {
            error.EndOfStream => return error.UnenclosedAttributeValue,
            // No boundary
            error.StreamTooLong => {},
            error.OutOfMemory => return error.OutOfMemory,
        };
        const value_end_idx = buffer.items.len;

        tag.name = buffer.items[0..name_end_idx];
        tag.value = buffer.items[value_start_idx..value_end_idx];
        return tag;
    }
};

pub const StartTag = struct {
    name: []const u8,
    attributes: []const Attribute,
    self_closing: bool = false,

    pub const Error = error{
        BadTagName,
        MissingTagName,
        BadTag,
        DuplicateAttributes,
    };

    const ParseFnError = Error || std.mem.Allocator.Error;

    fn parse(allocator: std.mem.Allocator, reader: anytype, first_char: ?u8) (ParseFnError || @TypeOf(reader).Error)!@This() {
        var buffer = std.ArrayList(u8).init(allocator);
        defer buffer.deinit();

        if (first_char) |char| try buffer.append(char);

        var tag: @This() = undefined;
        tag.attributes.len = 0;

        const parse_name_result = if (buffer.items.len == 0)
            parseName(reader, buffer.writer())
        else
            parseNamePastStart(reader, buffer.writer());

        parse_name_result catch |err| switch (err) {
            error.InvalidChar,
            error.InvalidStartChar,
            error.Equals,
            error.QuestionMark,
            => return error.BadTagName,

            error.ForwardSlash, error.GreaterThan => {
                if (err == error.ForwardSlash) {
                    tag.self_closing = true;
                }
                tag.name = try buffer.toOwnedSlice();
                return tag;
            },

            error.EndOfStream => return error.EndOfStream,
            error.OutOfMemory => return error.OutOfMemory,
        };
        if (buffer.items.len == 0) return error.MissingTagName;
        tag.name = try buffer.toOwnedSlice();

        var attribute_set = std.StringHashMap(void).init(allocator);
        defer attribute_set.deinit();

        var attributes = std.ArrayList(Attribute).init(allocator);
        defer attributes.deinit();

        while (true) {
            buffer.clearRetainingCapacity();
            const past_ws = try skipWhitespace(reader);
            if (past_ws == '/') {
                tag.self_closing = true;
                if (try reader.readByte() != '>') return error.BadTag;
                break;
            }
            if (past_ws == '>') {
                break;
            }
            try buffer.append(past_ws);
            var attribute = try Attribute.parse(reader, &buffer);

            attribute.name = try allocator.dupe(u8, attribute.name);
            errdefer allocator.free(attribute.name);

            attribute.value = try allocator.dupe(u8, attribute.value);
            errdefer allocator.free(attribute.value);

            const set_result = try attribute_set.getOrPut(attribute.name);
            if (set_result.found_existing) return error.DuplicateAttributes;

            try attributes.append(attribute);
        }
        tag.attributes = try attributes.toOwnedSlice();
        return tag;
    }

    pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        for (self.attributes) |attribute| {
            allocator.free(attribute.name);
            allocator.free(attribute.value);
        }
        if (self.attributes.len != 0) {
            allocator.free(self.attributes);
        }
    }
};

pub const EndTag = struct {
    name: []const u8,

    pub const Error = error{
        MissingTagName,
        BadTag,
    };

    const ParseFnError = Error || std.mem.Allocator.Error;

    pub fn parse(allocator: std.mem.Allocator, reader: anytype) (ParseFnError || @TypeOf(reader).Error)!@This() {
        var buffer = std.ArrayList(u8).init(allocator);
        defer buffer.deinit();

        var tag: @This() = undefined;

        parseName(reader, buffer.writer()) catch |err| switch (err) {
            error.InvalidChar,
            error.InvalidStartChar,
            error.Equals,
            error.ForwardSlash,
            error.QuestionMark,
            => return error.BadTagName,

            error.GreaterThan => {
                tag.name = try buffer.toOwnedSlice();
                return tag;
            },

            error.EndOfStream,
            error.OufOfMemory,
            => |e| return e,
        };
        if (buffer.items.len == 0) return error.MissingTagName;
        tag.name = try buffer.toOwnedSlice();

        while (true) {
            const char = try reader.readByte();
            if (char == '>') {
                break;
            }
            if (!isWhitespace(char)) return error.BadTag;
        }
        return tag;
    }

    pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
        allocator.free(self.name);
    }
};

pub fn isWhitespace(char: u8) bool {
    return switch (char) {
        0x20, 0x9, 0xD, 0xA => true,
        else => false,
    };
}

fn isNameStartChar(char: u8) bool {
    return switch (char) {
        ':',
        'A'...'Z',
        'a'...'z',
        '_',
        0xC0...0xD6,
        0xD8...0xF6,
        => true,
        else => false,
    };
}

fn isNameChar(char: u8) bool {
    if (isNameStartChar(char)) {
        return true;
    }
    return switch (char) {
        '-',
        '.',
        '0'...'9',
        0xBF,
        => true,
        else => false,
    };
}

fn skipWhitespace(reader: anytype) (@TypeOf(reader).Error || error{EndOfStream})!u8 {
    while (true) {
        const c = try reader.readByte();
        if (!isWhitespace(c)) {
            return c;
        }
    }
    unreachable;
}

pub const ParseNameError = error{
    ForwardSlash,
    GreaterThan,
    Equals,
    QuestionMark,
    InvalidChar,
    InvalidStartChar,
};

const ParseNameFnError = (ParseNameError || error{EndOfStream});

fn parseNamePastStart(reader: anytype, writer: anytype) (ParseNameFnError || @TypeOf(reader).Error || @TypeOf(writer).Error)!void {
    while (true) switch (try reader.readByte()) {
        '/' => return error.ForwardSlash,
        '>' => return error.GreaterThan,
        '=' => return error.Equals,
        '?' => return error.QuestionMark,
        else => |c| if (isWhitespace(c))
            break
        else if (isNameChar(c))
            try writer.writeByte(c)
        else
            return error.InvalidChar,
    };
}

fn parseName(reader: anytype, writer: anytype) (ParseNameFnError || @TypeOf(reader).Error || @TypeOf(writer).Error)!void {
    switch (try reader.readByte()) {
        '/' => return error.ForwardSlash,
        '>' => return error.GreaterThan,
        else => |c| if (isNameStartChar(c))
            try writer.writeByte(c)
        else {
            // std.log.debug("{x}", .{c});
            return error.InvalidStartChar;
        },
    }
    try parseNamePastStart(reader, writer);
}

pub const ParseCommentError = error{
    UnterminatedComment,
};

fn parseComment(allocator: std.mem.Allocator, reader: anytype) (ParseCommentError || std.mem.Allocator.Error || @TypeOf(reader).Error)![]const u8 {
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    while (!std.mem.endsWith(u8, buffer.items, "-->")) {
        reader.streamUntilDelimiter(buffer.writer(), '>', null) catch |err| switch (err) {
            error.EndOfStream => return error.UnterminatedComment,
            else => return err,
        };
        try buffer.append('>');
    }
    _ = buffer.pop(); // pop '>'
    _ = buffer.pop(); // '-'
    _ = buffer.pop(); // '-'

    return try buffer.toOwnedSlice();
}

pub const XmlTag = union(enum) {
    start_tag: StartTag,
    end_tag: EndTag,
    comment: []const u8,
    xml_decl: XmlDecl,

    pub const Error = error{
        BadTag,
    };

    const ParseFnError = Error || std.mem.Allocator.Error || error{EndOfStream};

    pub fn parse(allocator: std.mem.Allocator, reader: anytype) (ParseFnError || @TypeOf(reader).Error)!@This() {
        while (true) {
            const c = reader.readByte() catch |err| switch (err) {
                error.EndOfStream => return error.EndOfStream,
                else => return err,
            };
            switch (c) {
                '?' => return .{ .xml_decl = try XmlDecl.parse(allocator, reader) },
                '!' => switch (try reader.readByte()) {
                    '-' => if (try reader.readByte() != '-')
                        return error.InvalidTag
                    else
                        return .{ .comment = try parseComment(allocator, reader) },
                    else => try reader.skipUntilDelimiterOrEof('>'),
                },
                '/' => return .{ .end_tag = try EndTag.parse(allocator, reader) },
                else => {
                    return .{ .start_tag = try StartTag.parse(allocator, reader, c) };
                },
            }
        }
        unreachable;
    }

    pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
        switch (self) {
            .start_tag => |unwrapped| unwrapped.deinit(allocator),
            .end_tag => |unwrapped| unwrapped.deinit(allocator),
            .xml_decl => |unwrapped| unwrapped.deinit(allocator),
            .comment => |unwrapped| allocator.free(unwrapped),
        }
    }
};

test {
    _ = std.testing.refAllDecls(@This());
    _ = std.testing.refAllDecls(XmlTag);
    _ = std.testing.refAllDecls(XmlDecl);
    _ = std.testing.refAllDecls(Attribute);
    _ = std.testing.refAllDecls(StartTag);
    _ = std.testing.refAllDecls(EndTag);
}
