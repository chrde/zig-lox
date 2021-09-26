const std = @import("std");
const debug = std.debug;
const Scanner = @import("scanner.zig").Scanner;
const Vm = @import("vm.zig").Vm;
const Token = @import("scanner.zig").Token;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const Obj = @import("object.zig").Obj;
const d = @import("debug.zig");

pub const Compiler = struct {
    const Self = @This();
    vm: *Vm,
    scanner: Scanner,
    chunk: *Chunk,
    current: Token = undefined,
    previous: Token = undefined,
    had_error: bool = false,
    panic_mode: bool = false,

    pub fn init(vm: *Vm, source: []const u8, chunk: *Chunk) Self {
        return Self{
            .vm = vm,
            .scanner = Scanner.init(source),
            .chunk = chunk,
        };
    }

    fn match(self: *Self, ty: Token.Type) bool {
        if (!self.check(ty)) return false;
        self.advance();
        return true;
    }

    fn check(self: Self, ty: Token.Type) bool {
        return self.current.ty == ty;
    }

    pub fn compile(self: *Self) error{Compile}!void {
        self.advance();
        while (!self.match(Token.Type.eof)) {
            self.declaration();
        }
        if (self.had_error) {
            return error.Compile;
        }
        return self.end();
    }

    fn printStatement(self: *Self) void {
        self.expression();
        self.consume(.semicolon, "Expect ';' after value.");
        self.emitOp(OpCode.print);
    }

    fn declaration(self: *Self) void {
        if (self.match(.@"var")) {
            self.varDeclaration();
        } else {
            self.statement();
        }

        if (self.panic_mode) {
            self.synchronize();
        }
    }

    fn varDeclaration(self: *Self) void {
        const global = self.parseVariable("Expect variable name.") catch unreachable;
        if (self.match(.equal)) {
            self.expression();
        } else {
            self.emitOp(.nil);
        }
        self.consume(.semicolon, "Expect ';' after variable declaration.");
        self.defineVariable(global);
    }

    fn statement(self: *Self) void {
        if (self.match(Token.Type.print)) {
            self.printStatement();
        } else {
            self.expressionStatement();
        }
    }

    fn expressionStatement(self: *Self) void {
        self.expression();
        self.consume(.semicolon, "Expect ';' after expression.");
        self.emitOp(OpCode.pop);
    }

    fn advance(self: *Self) void {
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

    fn consume(self: *Self, ty: Token.Type, message: []const u8) void {
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

    fn synchronize(self: *Self) void {
        self.panic_mode = false;
        while (self.current.ty != .eof) {
            if (self.previous.ty == .semicolon) return;
            switch (self.current.ty) {
                .class, .fun, .@"var", .@"for", .@"if", .@"while", .print, .@"return" => break,
                else => self.advance(),
            }
        }
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
            .minus => self.emitOp(OpCode.negate),
            .bang => self.emitOp(OpCode.not),
            else => unreachable,
        }
    }

    fn literal(self: *Self) void {
        switch (self.previous.ty) {
            .@"false" => self.emitOp(OpCode.@"false"),
            .@"true" => self.emitOp(OpCode.@"true"),
            .nil => self.emitOp(OpCode.nil),
            else => unreachable,
        }
    }

    fn string(self: *Self) void {
        const lexeme = self.previous.lexeme;
        const new_string = Obj.String.copy(self.vm, lexeme) catch unreachable;
        const obj = Value{ .obj = &new_string.obj };
        self.emitConstant(obj);
    }

    fn variable(self: *Self, can_assign: bool) void {
        self.namedVariable(self.previous, can_assign);
    }

    fn namedVariable(self: *Self, token: Token, can_assign: bool) void {
        const global = self.identifierConstant(token) catch unreachable;
        if (can_assign and self.match(.equal)) {
            self.expression();
            self.emitBytes(&[2]usize{ @enumToInt(OpCode.set_global), global });
        } else {
            self.emitBytes(&[2]usize{ @enumToInt(OpCode.get_global), global });
        }
    }

    // prefix, infix, precedence
    fn parsePrecedence(self: *Self, prec: Precedence) void {
        self.advance();
        const can_assign = @enumToInt(prec) <= @enumToInt(Precedence.assignment);
        switch (self.previous.ty) {
            .left_paren => self.grouping(),
            .minus, .bang => self.unary(),
            .number => self.number(),
            .string => self.string(),
            .identifier => self.variable(can_assign),
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

        if (can_assign and self.match(.equal)) {
            self.errorHere("Invalid assignment target.");
        }
    }

    fn parseVariable(self: *Self, message: []const u8) !usize {
        self.consume(.identifier, message);
        return self.identifierConstant(self.previous);
    }

    fn defineVariable(self: *Self, global: usize) void {
        self.emitBytes(&[2]usize{ @enumToInt(OpCode.define_global), global });
    }

    fn identifierConstant(self: *Self, token: Token) !usize {
        const str = Obj.String.copy(self.vm, token.lexeme) catch unreachable;
        return try self.makeConstant(Value{ .obj = &str.obj });
    }

    fn end(self: *Self) void {
        self.emitReturn();
        if (!self.had_error) {
            d.disassembleChunk(self.chunk.*, "code");
        }
    }

    fn emitReturn(self: *Self) void {
        self.emitOp(OpCode.@"return");
    }

    fn emitConstant(self: *Self, value: Value) void {
        const val = self.makeConstant(value) catch unreachable;
        self.emitBytes(&[2]usize{ @enumToInt(OpCode.constant), val });
    }

    fn makeConstant(self: *Self, value: Value) !usize {
        const constant = self.chunk.addConstant(value) catch |err| {
            self.errorHere("Too many constants in one chunk");
            return err;
        };
        return constant;
    }

    fn emitByte(self: *Self, byte: usize) void {
        self.chunk.write(byte, self.previous.line);
    }

    fn emitOp(self: *Self, op: OpCode) void {
        self.emitByte(@enumToInt(op));
    }

    fn emitOps(self: *Self, ops: []const OpCode) void {
        for (ops) |op| {
            self.emitOp(op);
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
