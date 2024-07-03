const std = @import("std");

pub const XmlTag = union(enum) {
    start_tag: StartTag,
    end_tag: EndTag,
    comment: []const u8,
    xml_decl: XmlDecl,

    pub const Attribute = struct {
        name: []const u8,
        value: []const u8,

        pub fn parse(reader: anytype, buffer: *std.ArrayList(u8)) !@This() {
            var encoutered_equals = false;

            var tag: @This() = undefined;

            if (buffer.items.len == 0) {
                parseName(reader, buffer.writer()) catch |err| switch (err) {
                    error.InvalidChar,
                    error.InvalidStartChar,
                    => return error.BadAttributeName,

                    error.Equals => encoutered_equals = true,

                    error.ForwardSlash,
                    error.GreaterThan,
                    => return error.MissingAttributeValue,
                    else => return err,
                };
            } else {
                parseNamePastStart(reader, buffer.writer()) catch |err| switch (err) {
                    error.InvalidChar,
                    => return error.BadAttributeName,

                    error.Equals => encoutered_equals = true,

                    error.ForwardSlash,
                    error.GreaterThan,
                    => return error.MissingAttributeValue,
                    else => return err,
                };
            }
            if (buffer.items.len == 0) return error.MissingAttributeName;
            const name_end_idx = buffer.items.len;

            if (!encoutered_equals) {
                // Consumed whitespace
                if (try reader.readByte() != '=') return error.ExpectedEquals;
            }
            var quote = try reader.readByte();
            if (isWhitespace(quote)) {
                if (quote == '\n') return error.MissingAttributeValue;
                quote = try reader.readByte();
            }
            if (quote != '\'' and quote != '"') return error.BadValueQuote;
            const value_start_idx = buffer.items.len;

            reader.streamUntilDelimiter(buffer.writer(), quote, null) catch |err| switch (err) {
                error.EndOfStream => return error.UnenclosedAttributeValue,
                else => return err,
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

        fn parse(allocator: std.mem.Allocator, reader: anytype, first_char: ?u8) !@This() {
            var buffer = std.ArrayList(u8).init(allocator);
            defer buffer.deinit();

            if (first_char) |char| try buffer.append(char);

            var tag: @This() = undefined;
            tag.attributes.len = 0;

            parseName(reader, buffer.writer()) catch |err| switch (err) {
                error.InvalidChar,
                error.InvalidStartChar,
                error.Equals,
                => return error.BadTagName,

                error.ForwardSlash, error.GreaterThan => {
                    if (err == error.ForwardSlash) {
                        tag.self_closing = true;
                    }
                    tag.name = try buffer.toOwnedSlice();
                    return tag;
                },
                else => return err,
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

        pub fn parse(allocator: std.mem.Allocator, reader: anytype) !@This() {
            var buffer = std.ArrayList(u8).init(allocator);
            defer buffer.deinit();

            var tag: @This() = undefined;

            parseName(reader, buffer.writer()) catch |err| switch (err) {
                error.InvalidChar,
                error.InvalidStartChar,
                error.Equals,
                error.ForwardSlash,
                => return error.BadTagName,

                error.GreaterThan => {
                    tag.name = try buffer.toOwnedSlice();
                    return tag;
                },
                else => return err,
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

    pub const XmlDecl = struct {
        version: std.SemanticVersion,
        encoding: ?[]const u8,

        pub fn parse(allocator: std.mem.Allocator, reader: anytype) !@This() {
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
                const char = try reader.readByte();
                if (char == '?') {
                    break;
                }
                try buffer.append(char);
                const attribute = try Attribute.parse(reader, &buffer);
                if (std.mem.eql(u8, attribute.name, "version")) {
                    var split_it = std.mem.splitScalar(u8, attribute.value, '.');
                    const major: usize = try std.fmt.parseInt(usize, split_it.next() orelse return error.BadXmlVersion, 10);
                    const minor: usize = try std.fmt.parseInt(usize, split_it.next() orelse return error.BadXmlVersion, 10);

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

    fn isWhitespace(char: u8) bool {
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

    fn skipWhitespace(reader: anytype) !u8 {
        while (true) {
            const c = try reader.readByte();
            if (!isWhitespace(c)) {
                return c;
            }
        }
        unreachable;
    }

    fn parseNamePastStart(reader: anytype, writer: anytype) !void {
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

    fn parseName(reader: anytype, writer: anytype) !void {
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

    fn parseComment(allocator: std.mem.Allocator, reader: anytype) ![]const u8 {
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

    pub fn parse(allocator: std.mem.Allocator, reader: anytype) !@This() {
        while (true) {
            const c = reader.readByte() catch |err| switch (err) {
                error.EndOfStream => break,
                else => return err,
            };
            switch (c) {
                '?' => return .{
                    .xml_decl = XmlDecl.parse(allocator, reader) catch |err| switch (err) {
                        error.BadValueQuote => {
                            // std.log.debug("buffer: {s}", .{buffered_reader.buf[buffered_reader.start..]});
                            return err;
                        },
                        else => return err,
                    },
                },
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
