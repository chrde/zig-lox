const std = @import("std");
const debug = std.debug;
const Scanner = @import("scanner.zig").Scanner;
const Token = @import("scanner.zig").Token;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const TokenType = @import("scanner.zig").TokenType;
const d = @import("debug.zig");

pub const Compiler = struct {
    const Self = @This();
    scanner: Scanner,
    chunk: *Chunk,
    current: Token = undefined,
    previous: Token = undefined,
    had_error: bool = false,
    panic_mode: bool = false,

    pub fn init(source: []const u8, chunk: *Chunk) Self {
        return Self{
            .scanner = Scanner.init(source),
            .chunk = chunk,
        };
    }

    pub fn compile(self: *Self) error{Compile}!void {
        self.advance();
        self.expression();
        if (self.had_error) {
            return error.Compile;
        }
        return self.end();
    }

    fn advance(self: *Self) void {
        self.previous = self.current;
        while (self.scanner.nextToken()) |token| {
            if (token.ty == .@"error") {
                self.errorAtCurrent(token.lexeme);
            } else {
                self.current = token;
                break;
            }
        }
    }

    fn consume(self: *Self, ty: TokenType, message: []const u8) void {
        if (self.current.ty != ty) {
            self.errorAtCurrent(message);
        } else {
            self.advance();
        }
    }

    pub fn errorAtCurrent(self: *Self, message: []const u8) void {
        self.errorAt(self.current, message);
    }

    pub fn errorHere(self: *Self, message: []const u8) void {
        self.errorAt(self.previous, message);
    }

    fn errorAt(self: *Self, token: Token, message: []const u8) void {
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

    fn expression(self: *Self) void {
        self.parsePrecedence(.assignment);
    }

    fn number(self: *Self) void {
        const val = std.fmt.parseFloat(f64, self.previous.lexeme) catch unreachable;
        self.emitConstant(Value{ .number = val });
    }

    fn grouping(self: *Self) void {
        self.expression();
        self.consume(.right_paren, "Expect ')' after expression.");
    }

    // prefix, infix, precedence

    fn binary(self: *Self) void {
        const ty = self.previous.ty;
        const prec = ty.infix_prec();
        if (prec == null) {
            self.errorHere("Invalid infix operator.");
            return;
        }

        // NOTE(chrde): everything is left-associative
        self.parsePrecedence(@intToEnum(Precedence, @enumToInt(prec.?) + 1));

        const ops: []const OpCode = switch (ty) {
            .plus => &.{.add},
            .minus => &.{.substract},
            .slash => &.{.divide},
            .star => &.{.multiply},
            .greater => &.{.greater},
            .greater_equal => &.{ .less, .not },
            .equal_equal => &.{.equal},
            .bang_equal => &.{ .equal, .not },
            .less => &.{.less},
            .less_equal => &.{ .greater, .not },
            else => unreachable,
        };
        self.emitOps(ops);
    }

    fn unary(self: *Self) void {
        const ty = self.previous.ty;
        self.parsePrecedence(.unary);

        switch (ty) {
            .minus => self.emitByte(@enumToInt(OpCode.negate)),
            .bang => self.emitByte(@enumToInt(OpCode.not)),
            else => unreachable,
        }
    }

    fn literal(self: *Self) void {
        switch (self.previous.ty) {
            .@"false" => self.emitByte(@enumToInt(OpCode.@"false")),
            .@"true" => self.emitByte(@enumToInt(OpCode.@"true")),
            .nil => self.emitByte(@enumToInt(OpCode.nil)),
            else => unreachable,
        }
    }

    fn parsePrecedence(self: *Self, prec: Precedence) void {
        self.advance();
        switch (self.previous.ty) {
            .left_paren => self.grouping(),
            .minus, .bang => self.unary(),
            .number => self.number(),
            .@"false", .@"true", .nil => self.literal(),
            else => return self.errorHere("Expected expression."),
        }

        while (self.current.ty.infix_prec()) |next_prec| {
            if (@enumToInt(prec) > @enumToInt(next_prec)) {
                break;
            }
            self.advance();
            switch (self.previous.ty) {
                .plus, .minus, .slash, .star, .equal_equal, .greater, .greater_equal, .less, .less_equal => self.binary(),
                else => return self.errorHere("Invalid infix operator."),
            }
        }
    }

    fn end(self: *Self) void {
        self.emitReturn();
        if (!self.had_error) {
            d.disassembleChunk(self.chunk.*, "code");
        }
    }

    fn emitReturn(self: *Self) void {
        self.emitByte(@enumToInt(OpCode.@"return"));
    }

    fn emitConstant(self: *Self, value: Value) void {
        if (self.makeConstant(value)) |constant| {
            self.emitBytes(&[2]usize{ @enumToInt(OpCode.constant), constant });
        }
    }

    fn makeConstant(self: *Self, value: Value) ?usize {
        const constant = self.chunk.addConstant(value) catch |err| {
            self.errorHere("Too many constants in one chunk");
            return null;
        };
        return constant;
    }

    fn emitByte(self: *Self, byte: usize) void {
        self.chunk.write(byte, self.previous.line);
    }

    fn emitOps(self: *Self, ops: []const OpCode) void {
        for (ops) |op| {
            self.emitByte(@enumToInt(op));
        }
    }

    fn emitBytes(self: *Self, bytes: []usize) void {
        for (bytes) |byte| {
            self.chunk.write(byte, self.previous.line);
        }
    }
};

pub const Precedence = enum {
    none,
    assignment,
    @"or",
    @"and",
    equality,
    comparison,
    term,
    factor,
    unary,
    call,
    primary,
};
