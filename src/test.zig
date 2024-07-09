const std = @import("std");
const xml = @import("./xml.zig");

const XmlDecl = xml.XmlDecl;
const XmlTag = xml.XmlTag;
const XmlStartTag = xml.StartTag;
const XmlEndTag = xml.EndTag;
const XmlAttribute = xml.Attribute;

const allocator = std.testing.allocator;

fn printTabularTagCase(case: []const u8, stream_pos: usize) void {
    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();
    const stderr = std.io.getStdErr();

    stderr.writeAll("\ncase: ") catch return;
    std.fmt.fmtSliceEscapeLower(case).format("", .{}, stderr.writer()) catch return;
    stderr.writer().print("\nstream pos: {}\n", .{stream_pos}) catch return;
}

test XmlAttribute {
    const cases = .{
        .{
            .buffer = "name=\"value\"",
            .expected = XmlAttribute{ .name = "name", .value = "value" },
        },
        .{
            .buffer = "name = 'value'",
            .expected = XmlAttribute{ .name = "name", .value = "value" },
        },
        .{
            .buffer = "= 'value'",
            .expected = XmlAttribute.Error.BadAttributeName,
        },
        .{
            .buffer = "name = ",
            .expected = XmlAttribute.Error.MissingAttributeValue,
        },
        .{
            .buffer = "name",
            .expected = error.EndOfStream,
        },
        .{
            .buffer = "name  'value'",
            .expected = XmlAttribute.Error.ExpectedEquals,
        },
    };

    inline for (std.meta.fields(@TypeOf(cases))) |field| {
        const case = @field(cases, field.name);
        var stream = std.io.fixedBufferStream(case.buffer);
        errdefer printTabularTagCase(case.buffer, stream.pos);

        var attribute_buffer = std.ArrayList(u8).init(allocator);
        defer attribute_buffer.deinit();

        const result = XmlAttribute.parse(stream.reader(), &attribute_buffer);

        const type_info_expected = @typeInfo(@TypeOf(case.expected));
        switch (type_info_expected) {
            .ErrorSet => try std.testing.expectError(case.expected, result),
            .Struct => {
                const attr = try result;

                try std.testing.expectEqualSlices(u8, case.expected.name, attr.name);
                try std.testing.expectEqualSlices(u8, case.expected.value, attr.value);
            },
            else => @compileError("Unsupported expected type"),
        }
    }
}

test XmlDecl {
    const cases = .{
        .{
            .buffer = "xml version=\"1.0\"?>",
            .expected = XmlDecl{ .version = .{ .major = 1, .minor = 0, .patch = 0 }, .encoding = null },
        },
        .{
            .buffer = "xml\nversion=\"1.1\"\nencoding ='UTF-8'\n?>",
            .expected = XmlDecl{ .version = .{ .major = 1, .minor = 1, .patch = 0 }, .encoding = "utf-8" },
        },
        .{
            .buffer = "xml ?>",
            .expected = XmlDecl.Error.MissingXmlVersion,
        },
        .{
            .buffer = "?>",
            .expected = XmlDecl.Error.BadTagName,
        },
        .{
            .buffer = "xml >",
            .expected = error.EndOfStream,
        },
    };

    inline for (std.meta.fields(@TypeOf(cases))) |field| {
        const case = @field(cases, field.name);
        var stream = std.io.fixedBufferStream(case.buffer);
        errdefer printTabularTagCase(case.buffer, stream.pos);

        const result = XmlDecl.parse(allocator, stream.reader());

        const type_info_expected = @typeInfo(@TypeOf(case.expected));
        switch (type_info_expected) {
            .ErrorSet => try std.testing.expectError(case.expected, result),
            .Struct => {
                const xmldecl = try result;
                defer xmldecl.deinit(allocator);

                if (case.expected.encoding) |expected| {
                    try std.testing.expect(xmldecl.encoding != null);
                    try std.testing.expectEqualSlices(u8, expected, xmldecl.encoding.?);
                }

                try std.testing.expectEqual(case.expected.version, xmldecl.version);
            },
            else => @compileError("Unsupported expected type"),
        }
    }
}
