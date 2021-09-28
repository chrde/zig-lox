const std = @import("std");
const debug = std.debug;
const Scanner = @import("scanner.zig").Scanner;
const Token = @import("scanner.zig").Token;

pub const Parser = struct {
    const Self = @This();
    scanner: Scanner,
    current: Token = undefined,
    previous: Token = undefined,
    had_error: bool = false,
    panic_mode: bool = false,

    pub fn init(source: []const u8) Self {
        return Self{
            .scanner = Scanner.init(source),
        };
    }

    pub fn advance(self: *Self) void {
        self.previous = self.current;
        while (true) {
            const next = self.scanner.nextToken();
            if (next.ty == .@"error") {
                self.errorAtCurrent(next.lexeme);
            } else {
                self.current = next;
                break;
            }
        }
    }

    pub fn synchronize(self: *Self) void {
        self.panic_mode = false;
        while (self.current.ty != .eof) {
            if (self.previous.ty == .semicolon) return;
            switch (self.current.ty) {
                .class, .fun, .@"var", .@"for", .@"if", .@"while", .print, .@"return" => break,
                else => self.advance(),
            }
        }
    }

    pub fn errorHere(self: *Self, message: []const u8) void {
        self.errorAt(self.previous, message);
    }

    pub fn errorAt(self: *Self, token: Token, message: []const u8) void {
        if (self.panic_mode) {
            return;
        } else {
            self.panic_mode = true;
        }
        debug.print("[line {d} Error", .{token.line});
        switch (token.ty) {
            .eof => debug.print(" at end", .{}),
            .@"error" => {},
            else => debug.print(" at '{s}'", .{token.lexeme}),
        }
        debug.print(": {s}\n", .{message});
        self.had_error = true;
    }

    fn errorAtCurrent(self: *Self, message: []const u8) void {
        self.errorAt(self.current, message);
    }

    pub fn consume(self: *Self, ty: Token.Type, message: []const u8) void {
        if (self.current.ty != ty) {
            self.errorAtCurrent(message);
        } else {
            self.advance();
        }
    }

    pub fn match(self: *Self, ty: Token.Type) bool {
        if (!self.check(ty)) return false;
        self.advance();
        return true;
    }

    pub fn check(self: Self, ty: Token.Type) bool {
        return self.current.ty == ty;
    }
};
