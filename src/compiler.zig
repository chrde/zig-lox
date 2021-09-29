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

const stack_max = 255;
const uninitialized = -1;
const jump_bytes = 2;

const Local = struct {
    name: Token,
    depth: i32 = uninitialized,
};

pub const Compiler = struct {
    const Self = @This();
    vm: *Vm,
    chunk: *Chunk,
    locals: std.ArrayList(Local),
    scope_depth: i32 = 0,
    parser: Parser,

    pub fn init(vm: *Vm, source: []const u8, chunk: *Chunk) !Self {
        return Self{
            .vm = vm,
            .parser = Parser.init(source),
            .locals = try std.ArrayList(Local).initCapacity(vm.allocator, stack_max),
            .chunk = chunk,
        };
    }

    pub fn deinit(self: *Self) void {
        self.locals.deinit();
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
        if (self.parser.match(.print)) {
            self.printStatement();
        } else if (self.parser.match(.@"if")) {
            self.ifStatement();
        } else if (self.parser.match(.@"while")) {
            self.whileStatement();
        } else if (self.parser.match(.left_brace)) {
            self.beginScope();
            self.block();
            self.endScope();
        } else {
            self.expressionStatement();
        }
    }

    fn whileStatement(self: *Self) void {
        const loop_start = self.chunk.code.items.len;
        self.parser.consume(.left_paren, "Expect '(' after 'while'.");
        self.expression();
        self.parser.consume(.right_paren, "Expect ')' after condition.");

        const exit_jump = self.emitJump(.jump_if_false);
        self.emitOp(.pop);
        self.statement();
        self.emitLoop(loop_start);

        self.patchJump(exit_jump);
        self.emitOp(.pop);
    }

    fn emitLoop(self: *Self, start: usize) void {
        self.emitOp(.loop);
        const len = self.chunk.code.items.len;
        const offset = len - start + jump_bytes;
        if (offset > std.math.maxInt(u16)) {
            self.parser.errorHere("Loop body too large.");
        } else {
            self.emitBytes(std.mem.asBytes(&@intCast(u16, offset)));
        }
    }

    fn ifStatement(self: *Self) void {
        self.parser.consume(.left_paren, "Expect '(' after 'if'.");
        self.expression();
        self.parser.consume(.right_paren, "Expect ')' after condition.");

        const then_jump = self.emitJump(.jump_if_false);
        self.emitOp(.pop);
        self.statement();
        const else_jump = self.emitJump(.jump);

        self.patchJump(then_jump);
        self.emitOp(.pop);

        if (self.parser.match(.@"else")) {
            self.statement();
        }
        self.patchJump(else_jump);
    }

    fn block(self: *Self) void {
        while (!self.parser.check(.right_brace) and !self.parser.check(.eof)) {
            self.declaration();
        }
        self.parser.consume(.right_brace, "Expect '}' after block.");
    }

    fn beginScope(self: *Self) void {
        self.scope_depth += 1;
    }

    fn endScope(self: *Self) void {
        self.scope_depth -= 1;

        while (self.locals.items.len > 0 and self.locals.items[self.locals.items.len - 1].depth > self.scope_depth) {
            self.emitOp(.pop);
            _ = self.locals.pop();
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

    fn namedVariable(self: *Self, name: Token, can_assign: bool) void {
        var arg: u8 = undefined;
        var set_op: OpCode = undefined;
        var get_op: OpCode = undefined;
        if (try self.resolveLocal(name)) |local| {
            arg = local;
            set_op = .set_local;
            get_op = .get_local;
        } else {
            arg = self.identifierConstant(name) catch unreachable;
            set_op = .set_global;
            get_op = .get_global;
        }
        if (can_assign and self.parser.match(.equal)) {
            self.expression();
            self.emitBytes(&[2]u8{ @enumToInt(set_op), arg });
        } else {
            self.emitBytes(&[2]u8{ @enumToInt(get_op), arg });
        }
    }

    fn resolveLocal(self: *Self, name: Token) !?u8 {
        var l: usize = self.locals.items.len;
        while (l > 0) : (l -= 1) {
            const local = self.locals.items[l - 1];
            if (std.mem.eql(u8, local.name.lexeme, name.lexeme)) {
                if (local.depth == uninitialized) {
                    self.parser.errorHere("Can't read local variable in its own initializer");
                    return null;
                } else {
                    return @intCast(u8, l - 1);
                }
            }
        }
        return null;
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
                .@"and" => self.and_(),
                .@"or" => self.or_(),
                else => return self.parser.errorHere("Invalid infix operator."),
            }
        }

        if (can_assign and self.parser.match(.equal)) {
            self.parser.errorHere("Invalid assignment target.");
        }
    }

    fn or_(self: *Self) void {
        const jump = self.emitJump(.jump_if_true);
        self.emitOp(.pop);
        self.parsePrecedence(.@"or");
        self.patchJump(jump);
    }

    fn and_(self: *Self) void {
        const jump = self.emitJump(.jump_if_false);
        self.emitOp(.pop);
        self.parsePrecedence(.@"and");
        self.patchJump(jump);
    }

    fn parseVariable(self: *Self, message: []const u8) !?u8 {
        self.parser.consume(.identifier, message);

        self.declareVariable();
        if (self.scope_depth > 0) return null;
        return try self.identifierConstant(self.parser.previous);
    }

    fn declareVariable(self: *Self) void {
        if (self.scope_depth == 0) return;
        const name = self.parser.previous;
        var l: usize = self.locals.items.len;
        while (l > 0) : (l -= 1) {
            const local = self.locals.items[l - 1];
            if (local.depth < self.scope_depth) {
                break;
            }

            if (std.mem.eql(u8, name.lexeme, local.name.lexeme)) {
                self.parser.errorHere("Already a variable with this name in this scope.");
            }
        }
        self.addLocal(name);
    }

    fn addLocal(self: *Self, name: Token) void {
        const local = Local{
            .name = name,
        };
        self.locals.appendAssumeCapacity(local);
    }

    fn defineVariable(self: *Self, global: ?u8) void {
        if (global) |g| {
            self.emitBytes(&[2]u8{ @enumToInt(OpCode.define_global), g });
        } else {
            self.markInitialized();
        }
    }

    fn markInitialized(self: *Self) void {
        self.locals.items[self.locals.items.len - 1].depth = self.scope_depth;
    }

    fn identifierConstant(self: *Self, token: Token) !u8 {
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
        self.emitBytes(&[2]u8{ @enumToInt(OpCode.constant), val });
    }

    fn makeConstant(self: *Self, value: Value) !u8 {
        const constant = self.chunk.addConstant(value) catch |err| {
            self.parser.errorHere("Too many constants in one chunk");
            return err;
        };
        return constant;
    }

    fn patchJump(self: *Self, offset: usize) void {
        const jump = self.chunk.code.items.len - offset - jump_bytes;
        if (jump > std.math.maxInt(u16)) {
            self.parser.errorHere("Too much code to jump over.");
        } else {
            std.mem.writeIntSlice(u16, self.chunk.code.items[offset .. offset + 2], @intCast(u16, jump), .Little);
        }
    }

    fn emitJump(self: *Self, op: OpCode) usize {
        self.emitOp(op);
        self.emitByte(0xFF);
        self.emitByte(0xFF);
        return self.chunk.code.items.len - jump_bytes;
    }

    fn emitOps(self: *Self, ops: []const OpCode) void {
        for (ops) |op| {
            self.emitOp(op);
        }
    }

    fn emitOp(self: *Self, op: OpCode) void {
        self.emitByte(@enumToInt(op));
    }

    fn emitByte(self: *Self, byte: u8) void {
        self.chunk.write(byte, self.parser.previous.line);
    }

    fn emitBytes(self: *Self, bytes: []u8) void {
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
