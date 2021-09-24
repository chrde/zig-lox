const std = @import("std");
const expect = @import("std").testing.expect;
const Precedence = @import("compiler.zig").Precedence;

const first_keyword = 40;

pub const TokenType = enum {
    const Self = @This();
    // Single-character tokens.
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    comma,
    dot,
    minus,
    plus,
    semicolon,
    slash,
    star,
    // One or two character tokens.
    bang,
    bang_equal,
    equal,
    equal_equal,
    greater,
    greater_equal,
    less,
    less_equal,
    // Literals.
    identifier,
    string,
    number,
    // Other
    eof,
    // Keywords.
    @"and" = first_keyword,
    class,
    @"else",
    @"false",
    @"for",
    fun,
    @"if",
    nil,
    @"or",
    print,
    @"return",
    super,
    this,
    @"true",
    @"var",
    @"while",
    @"error",

    // const keywords1: []const TokenType = blk: {
    //     comptime var kws: []const TokenType = &.{};
    //     inline for (std.meta.fields(TokenType)) |f| {
    //         if (f.value >= first_keyword) {
    //             kws = kws ++ &[_]TokenType {@field(TokenType, f.name)};
    //         }
    //     }
    //     break :blk kws;
    // };

    pub fn infix_prec(self: Self) ?Precedence {
        return switch (self) {
            .plus, .minus => .term,
            .slash, .star => .factor,
            .equal_equal => .equality,
            .greater, .greater_equal, .less, .less_equal => .comparison,
            else => null,
        };
    }

    pub fn prefix_prec(self: Self) Precedence {
        return switch (self) {
            else => unreachable,
        };
    }

    fn keywords() []const TokenType {
        comptime var es: []const TokenType = &.{};
        inline for (std.meta.fields(TokenType)) |f| {
            if (f.value >= first_keyword) {
                es = es ++ &[_]TokenType{@field(TokenType, f.name)};
            }
        }
        return es;
    }

    // fn keywords() []const TokenType {
    //     comptime var keyword_count: usize = 0;
    //     inline for (std.meta.fields(TokenType)) |f| {
    //         if (f.value >= first_keyword) {
    //             keyword_count += 1;
    //         }
    //     }
    //     const xs: [keyword_count]TokenType = undefined;
    //     var current: usize = 0;
    //     inline for (std.meta.fields(TokenType)) |f| {
    //         if (f.value >= first_keyword) {
    //             xs[current] = @field(TokenType, f.name);
    //             current += 1;
    //         }
    //     }
    //     return &xs;
    // }
};

test "keywords" {
    const kws = TokenType.keywords();
    try expect(17 == kws.len);
    try expect(std.mem.eql(u8, "and", @tagName(kws[0])));
    try expect(std.mem.eql(u8, "error", @tagName(kws[16])));
}

pub const Token = struct {
    // TODO(chrde): declare enum in here
    ty: TokenType,
    lexeme: []const u8,
    line: usize = 1,
};

pub const Scanner = struct {
    const Self = @This();
    source: []const u8,
    start: usize = 0,
    current: usize = 0,
    line: usize = 1,

    pub fn init(source: []const u8) Self {
        return Self{
            .source = source,
        };
    }

    pub fn advance(self: *Self) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    pub fn expression(self: *Self) Token {}

    pub fn consume(self: *Self, msg: []const u8) void {}

    pub fn nextToken(self: *Self) ?Token {
        self.skipWhitespace();
        self.start = self.current;
        if (self.isAtEnd()) {
            return null;
        }

        const c = self.advance();
        if (isDigit(c)) {
            return self.makeNumber();
        } else if (isAlpha(c)) {
            return self.makeIdentifier();
        }

        return switch (c) {
            '(' => self.makeToken(.left_paren),
            ')' => self.makeToken(.right_paren),
            '{' => self.makeToken(.left_brace),
            '}' => self.makeToken(.right_brace),
            ';' => self.makeToken(.semicolon),
            ',' => self.makeToken(.comma),
            '.' => self.makeToken(.dot),
            '-' => self.makeToken(.minus),
            '+' => self.makeToken(.plus),
            '/' => self.makeToken(.slash),
            '*' => self.makeToken(.star),
            '!' => {
                const ty: TokenType = if (self.match('=')) .bang_equal else .bang;
                return self.makeToken(ty);
            },
            '=' => {
                const ty: TokenType = if (self.match('=')) .equal_equal else .bang;
                return self.makeToken(ty);
            },
            '<' => {
                const ty: TokenType = if (self.match('=')) .less_equal else .less;
                return self.makeToken(ty);
            },
            '>' => {
                const ty: TokenType = if (self.match('=')) .greater_equal else .greater;
                return self.makeToken(ty);
            },
            '"' => self.makeString(),
            else => return self.errorToken("Unexpected character."),
        };
    }

    fn makeIdentifier(self: *Self) Token {
        while (!self.isAtEnd() and (isAlpha(self.peek()) or isDigit(self.peek()))) _ = self.advance();
        var token = self.makeToken(.identifier);
        for (TokenType.keywords()) |kw| {
            if (std.mem.eql(u8, @tagName(kw), token.lexeme)) {
                token.ty = kw;
                break;
            }
        }
        return token;
    }

    fn makeNumber(self: *Self) Token {
        while (!self.isAtEnd() and isDigit(self.peek())) {
            _ = self.advance();
        }
        if (!self.isAtEnd() and self.peek() == '.') {
            const next_is_digit = if (self.peekNext()) |n| isDigit(n) else false;
            if (next_is_digit) {
                _ = self.advance();
            }
            while (!self.isAtEnd() and isDigit(self.peek())) {
                _ = self.advance();
            }
        }
        return self.makeToken(.number);
    }

    fn makeString(self: *Self) Token {
        while (!self.isAtEnd() and self.peek() != '"') {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }
        if (self.isAtEnd()) {
            return self.errorToken("Unterminated string.");
        } else {
            _ = self.advance();
            return self.makeToken(.string);
        }
    }

    fn skipWhitespace(self: *Self) void {
        while (!self.isAtEnd()) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => _ = self.advance(),
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    const line_comment = if (self.peekNext()) |n|
                        n == '/'
                    else
                        false;
                    if (line_comment) {
                        while (!self.isAtEnd() and self.peek() != '\n') {
                            _ = self.advance();
                        }
                    }
                },
                else => return,
            }
        }
    }

    fn peek(self: Self) u8 {
        return self.source[self.current];
    }

    fn peekNext(self: Self) ?u8 {
        if (self.isAtEnd()) {
            return undefined;
        } else {
            return self.source[self.current + 1];
        }
    }

    fn match(self: *Self, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;
        self.current += 1;
        return true;
    }

    fn isAtEnd(self: Self) bool {
        return self.current >= self.source.len;
    }

    fn makeToken(self: *Self, ty: TokenType) Token {
        const token = Token{
            .ty = ty,
            .lexeme = self.source[self.start..self.current],
            .line = self.line,
        };
        return token;
    }

    fn errorToken(self: Self, message: []const u8) Token {
        return Token{
            .ty = .@"error",
            .lexeme = message,
            .line = self.line,
        };
    }
};

fn isDigit(char: u8) bool {
    return char >= '0' and char <= '9';
}

fn isAlpha(char: u8) bool {
    return (char >= 'a' and char <= 'z') or (char >= 'A' and char <= 'Z') or (char == '_');
}
