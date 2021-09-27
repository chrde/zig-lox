const std = @import("std");
const debug = std.debug;
const Vm = @import("vm.zig").Vm;
const Token = @import("scanner.zig").Token;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const Obj = @import("object.zig").Obj;
const Parser = @import("parser.zig").Parser;
const d = @import("debug.zig");

pub const Compiler = struct {
    const Self = @This();
    vm: *Vm,
    chunk: *Chunk,
    parser: Parser,

    pub fn init(vm: *Vm, source: []const u8, chunk: *Chunk) Self {
        return Self{
            .vm = vm,
            .parser = Parser.init(source),
            .chunk = chunk,
        };
    }

    pub fn compile(self: *Self) error{Compile}!void {
        self.parser.advance();
        while (!self.parser.match(Token.Type.eof)) {
            self.declaration();
        }
        if (self.parser.had_error) {
            return error.Compile;
        }
        return self.end();
    }

    fn printStatement(self: *Self) void {
        self.expression();
        self.parser.consume(.semicolon, "Expect ';' after value.");
        self.emitOp(OpCode.print);
    }

    fn declaration(self: *Self) void {
        if (self.parser.match(.@"var")) {
            self.varDeclaration();
        } else {
            self.statement();
        }

        if (self.parser.panic_mode) {
            self.parser.synchronize();
        }
    }

    fn varDeclaration(self: *Self) void {
        const global = self.parseVariable("Expect variable name.") catch unreachable;
        if (self.parser.match(.equal)) {
            self.expression();
        } else {
            self.emitOp(.nil);
        }
        self.parser.consume(.semicolon, "Expect ';' after variable declaration.");
        self.defineVariable(global);
    }

    fn statement(self: *Self) void {
        if (self.parser.match(Token.Type.print)) {
            self.printStatement();
        } else {
            self.expressionStatement();
        }
    }

    fn expressionStatement(self: *Self) void {
        self.expression();
        self.parser.consume(.semicolon, "Expect ';' after expression.");
        self.emitOp(OpCode.pop);
    }

    fn expression(self: *Self) void {
        self.parsePrecedence(.assignment);
    }

    fn number(self: *Self) void {
        const val = std.fmt.parseFloat(f64, self.parser.previous.lexeme) catch unreachable;
        self.emitConstant(Value{ .number = val });
    }

    fn grouping(self: *Self) void {
        self.expression();
        self.parser.consume(.right_paren, "Expect ')' after expression.");
    }

    fn binary(self: *Self) void {
        const ty = self.parser.previous.ty;
        const prec = ty.infix_prec();
        if (prec == null) {
            self.parser.errorHere("Invalid infix operator.");
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
        const ty = self.parser.previous.ty;
        self.parsePrecedence(.unary);

        switch (ty) {
            .minus => self.emitOp(OpCode.negate),
            .bang => self.emitOp(OpCode.not),
            else => unreachable,
        }
    }

    fn literal(self: *Self) void {
        switch (self.parser.previous.ty) {
            .@"false" => self.emitOp(OpCode.@"false"),
            .@"true" => self.emitOp(OpCode.@"true"),
            .nil => self.emitOp(OpCode.nil),
            else => unreachable,
        }
    }

    fn string(self: *Self) void {
        const lexeme = self.parser.previous.lexeme;
        const new_string = Obj.String.copy(self.vm, lexeme) catch unreachable;
        const obj = Value{ .obj = &new_string.obj };
        self.emitConstant(obj);
    }

    fn variable(self: *Self, can_assign: bool) void {
        self.namedVariable(self.parser.previous, can_assign);
    }

    fn namedVariable(self: *Self, token: Token, can_assign: bool) void {
        const global = self.identifierConstant(token) catch unreachable;
        if (can_assign and self.parser.match(.equal)) {
            self.expression();
            self.emitBytes(&[2]usize{ @enumToInt(OpCode.set_global), global });
        } else {
            self.emitBytes(&[2]usize{ @enumToInt(OpCode.get_global), global });
        }
    }

    // prefix, infix, precedence
    fn parsePrecedence(self: *Self, prec: Precedence) void {
        self.parser.advance();
        const can_assign = @enumToInt(prec) <= @enumToInt(Precedence.assignment);
        switch (self.parser.previous.ty) {
            .left_paren => self.grouping(),
            .minus, .bang => self.unary(),
            .number => self.number(),
            .string => self.string(),
            .identifier => self.variable(can_assign),
            .@"false", .@"true", .nil => self.literal(),
            else => return self.parser.errorHere("Expected expression."),
        }

        while (self.parser.current.ty.infix_prec()) |next_prec| {
            if (@enumToInt(prec) > @enumToInt(next_prec)) {
                break;
            }
            self.parser.advance();
            switch (self.parser.previous.ty) {
                .plus, .minus, .slash, .star, .equal_equal, .greater, .greater_equal, .less, .less_equal => self.binary(),
                else => return self.parser.errorHere("Invalid infix operator."),
            }
        }

        if (can_assign and self.parser.match(.equal)) {
            self.parser.errorHere("Invalid assignment target.");
        }
    }

    fn parseVariable(self: *Self, message: []const u8) !usize {
        self.parser.consume(.identifier, message);
        return self.identifierConstant(self.parser.previous);
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
        if (!self.parser.had_error) {
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
            self.parser.errorHere("Too many constants in one chunk");
            return err;
        };
        return constant;
    }

    fn emitOps(self: *Self, ops: []const OpCode) void {
        for (ops) |op| {
            self.emitOp(op);
        }
    }

    fn emitOp(self: *Self, op: OpCode) void {
        self.emitByte(@enumToInt(op));
    }

    fn emitByte(self: *Self, byte: usize) void {
        self.chunk.write(byte, self.parser.previous.line);
    }

    fn emitBytes(self: *Self, bytes: []usize) void {
        for (bytes) |byte| {
            self.emitByte(byte);
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
