package haxeparser;

import haxe.ds.ImmutableList;
import haxe.ds.Option;

import ocaml.List;
import ocaml.Ref;
// import byte.ByteData;

// eval
// enum Small_type {
// 	TNull;
// 	TBool(b:Bool);
// 	TFloat(f:Float);
// 	TString(s:String);
// }

class HaxeCondParser extends hxparse.Parser<hxparse.LexerTokenSource<syntax.Lexer.Token>, syntax.Lexer.Token> implements hxparse.ParserBuilder {
	public function new(stream){
		super(stream);
	}

	public function parseMacroCond(allowOp:Bool):{tk:Option<syntax.Lexer.Token>, expr:core.Ast.Expr} {
		return switch stream {
			case [{td:Const(CIdent(t)), pos:p}]:
				parseMacroIdent(allowOp, t, p);
			case [{td:Const(CString(s)), pos:p}]:
				{tk:None, expr:{expr:EConst(CString(s)), pos:p}};
			case [{td:Const(CInt(i)), pos:p}]:
				{tk:None, expr:{expr:EConst(CInt(i)), pos:p}};
			case [{td:Const(CFloat(f)), pos:p}]:
				{tk:None, expr:{expr:EConst(CFloat(f)), pos:p}};
			case [{td:Kwd(k), pos:p}]:
				parseMacroIdent(allowOp, core.Ast.s_keyword(k), p);
			case [{td:POpen, pos:p1}, o = parseMacroCond(true), {td:PClose, pos:p2}]:
				var e:core.Ast.Expr = {expr:EParenthesis(o.expr), pos:core.Ast.punion(p1, p2)};
				if (allowOp) parseMacroOp(e) else { tk:None, expr:e };
			case [{td:Unop(op), pos:p}, o = parseMacroCond(allowOp)]:
				{tk:o.tk, expr:HaxeParser.makeUnop(op, o.expr, p)};
		}
	}

	function parseMacroIdent(allowOp:Bool, t:String, p:core.Globals.Pos):{tk:Option<syntax.Lexer.Token>, expr:core.Ast.Expr} {
		if (t == "display") {
			syntax.Parser.special_identifier_files.set(core.Path.unique_full_path(p.pfile),t);
		}
		var e:core.Ast.Expr = {expr:EConst(CIdent(t)), pos:p};
		return if (!allowOp) { tk:None, expr:e } else parseMacroOp(e);
	}

	function parseMacroOp(e:core.Ast.Expr):{tk:Option<syntax.Lexer.Token>, expr:core.Ast.Expr} {
		return switch peek(0) {
			case {td:Binop(op)}:
				junk();
				op = switch peek(0) {
					case {td:Binop(OpAssign)} if (op == OpGt):
						junk();
						OpGte;
					case _: op;
				}
				try {
					var o = parseMacroCond(true);
					{tk:o.tk, expr:HaxeParser.makeBinop(op, e, o.expr)};
				}
				catch (_:hxparse.NoMatch<Dynamic>) {
					syntax.Parser.serror();
				}
			case tk:
				{tk:Some(tk), expr:e};
		}
	}
}

class HaxeTokenSource {
	var lexer:syntax.Lexer;

	var ctx:core.Define;

	@:allow(haxeparser.HaxeParser)
	var old:syntax.Lexer.LexerFile;

	@:allow(haxeparser.HaxeParser)
	var restore_cache:Void->Void;

	@:allow(haxeparser.HaxeParser)
	// var mstack:Array<core.Globals.Pos>;
	var mstack:Ref<ImmutableList<core.Globals.Pos>>;

	var rawSource:hxparse.LexerTokenSource<syntax.Lexer.Token>;
	var sraw:HaxeCondParser;

	// public function new(lexer,defines, ctx){
	public function new(lexer, ctx){
		this.lexer = lexer;
		this.ctx = ctx;

		this.old = syntax.Lexer.save();
		this.restore_cache = syntax.parser.TokenCache.clear();
		this.mstack = new Ref(Tl);

		syntax.Parser.last_doc = None;
		syntax.Parser.in_macro = core.Define.defined(ctx, Macro);
		// lexer.token(syntax.Lexer.skip_header);

		this.rawSource = new hxparse.LexerTokenSource(lexer,syntax.Lexer.tok);
		this.sraw = new HaxeCondParser(this.rawSource);
	}

	function lexerToken() : syntax.Lexer.Token {
		return lexer.token(syntax.Lexer.tok);
	}

	function next_token () : syntax.Lexer.Token {
		return process_token(lexerToken());
	}

	function process_token (tk:syntax.Lexer.Token) : syntax.Lexer.Token {
		return switch (tk.td) {
			case Comment(s):
				var tk = next_token();
				if (syntax.Parser.use_doc) {
					var l = s.length;
					if (l > 0 && s.charAt(0) == "*") {
						var docLength = l - ((l > 1 && s.charAt(l-1) == "*") ? 2 : 1);
						syntax.Parser.last_doc = Some({doc:s.substr(1, docLength), pos:tk.pos.pmin});
					}
				}
				tk;
			case CommentLine(_):
				next_token();
			case Sharp("end"):
				switch (mstack.get()) {
					case []: tk;
					case _ :: l:
						mstack.set(l);
						next_token();
				}
			case Sharp("else") | Sharp("elseif"):
				switch (mstack.get()) {
					case []: tk;
					case _ :: l:
						mstack.set(l);
						process_token(skip_tokens(tk.pos, false));
				}
			case Sharp("if"):
				process_token(enter_macro(tk.pos));
			case Sharp("error"):
				switch (lexerToken()) {
					case {td:Const(CString(s)), pos:p}: syntax.Parser.error(Custom(s), p);
					case _: syntax.Parser.error(Unimplemented, tk.pos);
				}
			case Sharp("line"):
				var ntk = next_token();
				switch (ntk.td) {
					case Const(CInt(s)):
						var line = Std.parseInt(s);
						if (line == null) {
							syntax.Parser.error(Custom("Could not parse ridiculous line number "+s), ntk.pos);
						}
						syntax.Lexer.cur.lline = line - 1;
						next_token();
					case _: syntax.Parser.error(Unexpected(ntk.td), ntk.pos);
				}
			case _:
				tk;
		}
	}

	function enter_macro (p:core.Globals.Pos) : syntax.Lexer.Token {
		var o = sraw.parseMacroCond(false);
		var tk = switch (o.tk) {
			case None: lexerToken();
			case Some(s): s;
		}

		var cond = switch (o.expr.expr) {
			case EConst(CIdent("macro")) if (core.Path.unique_full_path(p.pfile) == syntax.Parser.resume_display.get().pfile): true;
			case _: false;
		}
		if (isTrue(eval(ctx, o.expr)) || cond) {
			mstack.set(p :: mstack.get());
			return tk;
		}
		else {
			return skip_tokens_loop(p, true, tk);
		}
	}

	function skip_tokens_loop (p:core.Globals.Pos, test:Bool, tk:syntax.Lexer.Token) : syntax.Lexer.Token {
		return switch (tk.td) {
			case Sharp("end"):
				lexerToken();
			case Sharp("elseif"), Sharp("else") if (!test):
				skip_tokens(p, test);
			case Sharp("else"):
				mstack.set(tk.pos :: mstack.get());
				lexerToken();
			case Sharp("elseif"):
				enter_macro(tk.pos);
			case Sharp("if"):
				skip_tokens_loop(p, test, skip_tokens(p, false));
			case Eof:
				if (syntax.Parser.do_resume()) {
					tk;
				}
				else {
					syntax.Parser.error(Unclosed_macro, p);
				}
			case _:
				skip_tokens(p, test);
		}
	}

	function skip_tokens (p:core.Globals.Pos, test:Bool) : syntax.Lexer.Token {
		return skip_tokens_loop(p, test, lexerToken());
	}

	@:access(haxeparser.HaxeCondParser)
	public function token():syntax.Lexer.Token{
		var t = next_token();
		syntax.parser.TokenCache.add(t);
		return t;
	}

	public static function isTrue(a:syntax.ParserEntry.Small_type) {
		return switch a {
			case TBool(false), TNull, TFloat(0.0), TString(""): false;
			case _: true;
		}
	}

	public static function cmp(a:syntax.ParserEntry.Small_type, b:syntax.ParserEntry.Small_type) {
		return switch [a, b] {
			case [TNull, TNull]: 0;
			case [TFloat(a), TFloat(b)]: Reflect.compare(a, b);
			case [TString(a), TString(b)]: Reflect.compare(a, b);
			case [TBool(a), TBool(b)]: Reflect.compare(a, b);
			case [TString(a), TFloat(b)]: Reflect.compare(Std.parseFloat(a), b);
			case [TFloat(a), TString(b)]: Reflect.compare(a, Std.parseFloat(b));
			case _: throw ocaml.Exit.instance; // alway false
		}
	}

	public static function eval(ctx:core.Define, e:core.Ast.Expr) : syntax.ParserEntry.Small_type {
		return switch (e.expr) {
			case EConst(CIdent(i)):
				try {
					TString(core.Define.raw_defined_value(ctx, i));
				}
				catch (_:ocaml.Not_found) {
					TNull;
				}
			case EConst(CString(s)): TString(s);
			case EConst(CInt(n)), EConst(CFloat(n)): TFloat(Std.parseFloat(n));
			case EBinop(OpBoolAnd, e1, e2): TBool(isTrue(eval(ctx, e1)) && isTrue(eval(ctx, e2)));
			case EBinop(OpBoolOr, e1, e2): TBool(isTrue(eval(ctx, e1)) || isTrue(eval(ctx, e2)));
			case EUnop(OpNot, _, e): TBool(!isTrue(eval(ctx, e)));
			case EParenthesis(e): eval(ctx, e);
			case EBinop(op, e1, e2):
				var v1 = eval(ctx, e1);
				var v2 = eval(ctx, e2);
				function compare (op:Int->Int->Bool) : syntax.ParserEntry.Small_type {
					return try {
						return TBool(op(cmp(v1, v2), 0));
					}
					catch (_:Dynamic) {
						return TBool(false);
					}
				}
				switch (op) {
					case OpEq: compare(function (a, b) { return a == b;});
					case OpNotEq: compare(function (a, b) { return a != b;});
					case OpGt: compare(function (a, b) { return a > b;});
					case OpGte: compare(function (a, b) { return a >= b;});
					case OpLt: compare(function (a, b) { return a < b;});
					case OpLte: compare(function (a, b) { return a <= b;});
					case _: syntax.Parser.error(Custom("Unsupported operation"), e.pos);
				}
			case _: syntax.Parser.error(Custom("Invalid condition expression"), e.pos);
		}
	}

	public function curPos():hxparse.Position{
		return lexer.curPos();
	}
}

class HaxeParser extends hxparse.Parser<HaxeTokenSource, syntax.Lexer.Token> implements hxparse.ParserBuilder {

	public function new(input:byte.ByteData, sourceName:String, ctx:core.Define) {
		var lexer = new syntax.Lexer(input, sourceName);
		var ts = new HaxeTokenSource(lexer, ctx); // stream
		super(ts);
	}

	public function npeek(n:Int) : Array<syntax.Lexer.Token> {
		return [for (i in 0...n) peek(i) ];
	}

	public function parse() : {pack:ImmutableList<String>, decls:ImmutableList<core.Ast.TypeDecl>} {
		function catched () : Dynamic {
			var l = peek(0);
			if (l == null) {
				l = syntax.Parser.last_token(this);
			}
			syntax.Lexer.restore(stream.old);
			stream.restore_cache();
			return syntax.Parser.error(Unexpected(l.td), l.pos);
		}

		return try {
			var l = parseFile();
			switch (stream.mstack.get()) {
				case p::_ if (!syntax.Parser.do_resume()):
					syntax.Parser.error(Unclosed_macro, p);
				case _:
			}
			stream.restore_cache();
			syntax.Lexer.restore(stream.old);
			l;
		}
		catch (_:ocaml.stream.Error) { catched(); }
		catch (_:ocaml.stream.Failure) { catched(); }
		catch (_:hxparse.ParserError) { catched(); }
		catch (e:Any) {
			syntax.Lexer.restore(stream.old);
			stream.restore_cache();
			throw e;
		}
	}

	@:allow(haxeparser.HaxeCondParser)
	static function punion(p1:core.Globals.Pos, p2:core.Globals.Pos) {
		return core.Ast.punion(p1, p2);
	}

	public static function isLowerIdent(s:String) {
		function loop(p) {
			var c = s.charCodeAt(p);
			return if (c >= 'a'.code && c <= 'z'.code)
				true
			else if (c == '_'.code) {
				if (p + 1 < s.length)
					loop(p + 1);
				else
					true;
			} else
				false;
		}
		return loop(0);
	}

	static function isPostfix(e:core.Ast.Expr, u:core.Ast.Unop) {
		return switch (u) {
			case OpIncrement | OpDecrement:
				switch(e.expr) {
					case EConst(_) | EField(_) | EArray(_):
						true;
					case _:
						false;
				}
			case OpNot | OpNeg | OpNegBits: false;
		}
	}

	static function isPrefix(u:core.Ast.Unop) {
		return switch(u) {
			case OpIncrement | OpDecrement: true;
			case OpNot | OpNeg | OpNegBits: true;
		}
	}

	static function precedence(op:core.Ast.Binop) {
		var left = true;
		var right = false;
		return switch(op) {
			case OpIn : {p: 0, left: right};
			case OpMod : {p: 1, left: left};
			case OpMult | OpDiv : {p: 2, left: left};
			case OpAdd | OpSub : {p: 3, left: left};
			case OpShl | OpShr | OpUShr : {p: 4, left: left};
			case OpOr | OpAnd | OpXor : {p: 5, left: left};
			case OpEq | OpNotEq | OpGt | OpLt | OpGte | OpLte : {p: 6, left: left};
			case OpInterval : {p: 7, left: left};
			case OpBoolAnd : {p: 8, left: left};
			case OpBoolOr : {p: 9, left: left};
			case OpArrow : {p: 10, left: left};
			case OpAssign | OpAssignOp(_) : {p:11, left:right};
		}
	}

	static function isNotAssign(op:core.Ast.Binop) {
		return switch(op) {
			case OpAssign | OpAssignOp(_): false;
			case _: true;
		}
	}

	static function isDollarIdent(e:core.Ast.Expr) {
		return switch (e.expr) {
			case EConst(CIdent(n)) if (n.charCodeAt(0) == "$".code): true;
			case _: false;
		}
	}

	static function swap(op1:core.Ast.Binop, op2:core.Ast.Binop) {
		var i1 = precedence(op1);
		var i2 = precedence(op2);
		return i1.left && i1.p <= i2.p;
	}

	@:allow(haxeparser.HaxeCondParser)
	static function makeBinop(op:core.Ast.Binop, e:core.Ast.Expr, e2:core.Ast.Expr) : core.Ast.Expr {
		return switch (e2.expr) {
			case EBinop(_op,_e,_e2) if (swap(op,_op)):
				var _e = makeBinop(op,e,_e);
				{expr: EBinop(_op,_e,_e2), pos:punion(_e.pos,_e2.pos)};
			case ETernary(e1,e2,e3) if (isNotAssign(op)):
				var e = makeBinop(op,e,e1);
				{expr:ETernary(e,e2,e3), pos:punion(e.pos, e3.pos)};
			case _:
				{ expr: EBinop(op,e,e2), pos:punion(e.pos, e2.pos)};
		}
	}

	@:allow(haxeparser.HaxeCondParser)
	static function makeUnop(op:core.Ast.Unop, e:core.Ast.Expr, p1:core.Globals.Pos) : core.Ast.Expr {
		return switch(e.expr) {
			case EBinop(bop,e,e2):
				{ expr: EBinop(bop, makeUnop(op,e,p1), e2), pos: punion(p1,e.pos)};
			case ETernary(e1,e2,e3):
				{ expr: ETernary(makeUnop(op,e1,p1), e2, e3), pos:punion(p1,e.pos)};
			case _:
				{ expr: EUnop(op,Prefix,e), pos:punion(p1,e.pos)};
		}
	}

	static function makeMeta(name:core.Meta.StrictMeta, params:Array<core.Ast.Expr>, e:core.Ast.Expr, p1:core.Globals.Pos) : core.Ast.Expr {
		return switch(e.expr) {
			case EBinop(bop,e,e2):
				{ expr: EBinop(bop, makeMeta(name,params,e,p1), e2), pos: punion(p1,e.pos)};
			case ETernary(e1,e2,e3):
				{ expr: ETernary(makeMeta(name,params,e1,p1), e2, e3), pos:punion(p1,e.pos)};
			case _:
				{ expr: EMeta({name:name, params:params, pos:p1}, e), pos: punion(p1, e.pos) };
		}
	}

	static function apush<T>(a:Array<T>, t:T) {
		a.push(t);
		return a;
	}

	static function aunshift<T>(a:Array<T>, t:T) {
		a.unshift(t);
		return a;
	}

	function anyEnumIdent() : core.Ast.PlacedName {
		return switch stream {
			case [i = ident()]: i;
			case [{td:Kwd(k), pos:p}]: {pack:core.Ast.s_keyword(k).toLowerCase(), pos:p};
		}
	}

	function getDoc() : Option<String> {
		return syntax.Parser.get_doc(this);
	}

	function popt<T>(f:Void->T) : Option<T> {
		return
		try {
			switch stream {
				case [v = f()]: Some(v);
				case _: None;
			}
		}
		catch (_:ocaml.stream.Failure) {
			None;
		}
	}

	function plist<T>(f:Void->T) : Array<T> {
		var _tmp = try {
			switch stream {
				case [v = f()]: Some(v);
				case _: None;
			}
		}
		catch (_:ocaml.stream.Failure) {
			None;
		}
		return
		switch (_tmp) {
			case Some(v):
				try {
					var l = plist(f);
					aunshift(l, v);
				}
				catch (_:ocaml.stream.Failure) {
					throw new ocaml.stream.Error("");
				}
			case None: [];
		}
	}

	function psep<T>(sep:core.Ast.Token, f:Void->T):Array<T> {
		var _tmp = try {
			switch stream {
				case [v = f()]: Some(v);
				case _: None;
			}
		}
		catch (_:ocaml.stream.Failure) {
			None;
		}
		return switch (_tmp) {
			case Some(v):
				function loop() : Array<T> {
					return
					try {
						switch stream {
							case [{td:sep2} && sep2 == sep, v = f(), l = loop()]: [v].concat(l);
							case _: [];
						}
					}
					catch (_:ocaml.stream.Failure) {
						throw new ocaml.stream.Error("");
					}
				}
				return [v].concat(loop());
			case _: [];
		}
	}

	function ident() : core.Ast.PlacedName {
		return switch stream {
			case [{td:Const(CIdent(i)),pos:p}]: { pack: i, pos: p};
		}
	}

	function dollarIdent() : core.Ast.PlacedName {
		return switch stream {
			case [{td:Const(CIdent(i)),pos:p}]: { pack: i, pos: p};
			case [{td:Dollar(i), pos:p}]: { pack: "$" + i, pos: p};
		}
	}

	function dollarIdentMacro(pack:Array<String>) : core.Ast.PlacedName {
		return switch stream {
			case [{td:Const(CIdent(i)),pos:p}]: { pack: i, pos: p};
			case [{td:Dollar(i), pos:p}]: { pack: "$" + i, pos: p};
			case [{td:Kwd(Macro), pos: p} && pack.length > 0]: { pack: "macro", pos: p };
			case [{td:Kwd(Extern), pos: p} && pack.length > 0]: { pack: "extern", pos: p };
		}
	}

	function lowerIdentOrMacro() {
		return switch stream {
			case [{td:Const(CIdent(i))} && isLowerIdent(i)]: i;
			case [{td:Kwd(Macro)}]: "macro";
			case [{td:Kwd(Extern)}]: "extern";
		}
	}

	function propertyIdent() : core.Ast.PlacedName {
		return switch stream {
			case [i = ident()]: i;
			case [{td:Kwd(Dynamic), pos:p}]: {pack:"dynamic", pos:p};
			case [{td:Kwd(Default), pos:p}]: {pack:"default", pos:p};
			case [{td:Kwd(Null), pos:p}]: {pack:"null", pos:p};
		}
	}

	function comma() : Void {
		return switch stream {
			case [{td:Comma}]:
		}
	}

	function semicolon() : core.Globals.Pos {
		return if (syntax.Parser.last_token(this).td == BrClose) {
			switch stream {
				case [{td: Semicolon, pos:p}]: p;
				case _: syntax.Parser.last_token(this).pos;
			}
		} else switch stream {
			case [{td: Semicolon, pos:p}]: p;
			case _:
				var pos = syntax.Parser.last_token(this).pos;
				if (syntax.Parser.do_resume())
					pos
				else
					throw new syntax.parser.Error(Missing_semicolon, pos);
			}
	}

	function parseFile() : {pack:ImmutableList<String>, decls:ImmutableList<core.Ast.TypeDecl>} {
		syntax.Parser.last_doc = None;
		return switch stream {
			case [{td:Kwd(Package)}, pack = parsePackage()]:
				switch stream {
					case [{td:Const(CIdent(_)), pos:p} && pack.length == 0]:
						syntax.Parser.error(Custom("Package name must start with a lowercase character"), p);
					case [_ = semicolon(), l = parseTypeDecls(pack,[]), {td:Eof}]:
						{ pack: pack, decls: l };
				}
			case [l = parseTypeDecls([],[]), {td:Eof}]:
				{ pack: [], decls: l };
		}
	}

	function parseTypeDecls(pack:ImmutableList<String>, acc:ImmutableList<core.Ast.TypeDecl>) : ImmutableList<core.Ast.TypeDecl> {
		try {
			return switch stream {
				case [ v = parseTypeDecl(), l = parseTypeDecls(pack,v::acc) ]:
					l;
				case _: ocaml.List.rev(acc);
			}
		}
		catch (tp:syntax.parser.TypePath) {
			// resolve imports
			var b = tp.is_import;
			var name:String = null;
			if (tp.p != Tl) {
				throw tp;
			}
			switch (tp.c) {
				case Some({c:n, cur_package:false}):
					name = n;
				case _: throw tp;
			}
			// resolve imports
			List.iter(function (d:core.Ast.TypeDecl) {
				switch (d.decl) {
					case EImport({pns:t, mode:_}):
						switch (List.rev(t)) {
							case {pack:n} :: path if (n == name && List.for_all(function (p) { return isLowerIdent(p.pack); }, {var _path:ImmutableList<core.Ast.PlacedName> = path; path;})):
								throw new syntax.parser.TypePath(
									List.map(function (p) {return p.pack;}, List.rev(path)), Some({c:name, cur_package:false}), b
								);
							case _:
						}
					case _:
				}
			}, acc);
			throw new syntax.parser.TypePath(pack, Some({c:name, cur_package:true}), b);
		}
	}

	function parseTypeDecl() : core.Ast.TypeDecl {
		return switch stream {
			case [{td:Kwd(Import), pos:p1}]:
				parseImport(p1);
			case [{td:Kwd(Using), pos: p1}]:
				parseUsing(p1);
			case [doc = getDoc(), meta = parseMeta(), c = parseCommonFlags()]:
				switch stream {
					case [flags = parseEnumFlags(), name = typeName(), tl = parseConstraintParams(), {td:BrOpen}, l = plist(parseEnum), {td:BrClose, pos: p2}]:
						{decl: EEnum({
							d_name: name,
							d_doc: doc,
							d_meta: meta,
							d_params: tl,
							d_flags: c.map(syntax.Parser.decl_flag_to_enum_flag).concat(flags.flags),
							d_data: l
						}), pos: punion(flags.pos,p2)};
					case [flags = parseClassFlags(), name = typeName(), tl = parseConstraintParams(), hl = plist(parseClassHerit), {td:BrOpen}, fl = parseClassFields(false,flags.pos)]:
						{decl: EClass({
							d_name: name,
							d_doc: doc,
							d_meta: meta,
							d_params: tl,
							d_flags: c.map(syntax.Parser.decl_flag_to_class_flag).concat(flags.flags).concat(hl),
							d_data: fl.fields
						}), pos: punion(flags.pos,fl.pos)};
					case [{td: Kwd(Typedef), pos: p1}, name = typeName(), tl = parseConstraintParams(), {td:Binop(OpAssign), pos: p2}, t = parseComplexType()]:
						switch stream {
							case [{td:Semicolon}]:
							case _:
						}
						{ decl: ETypedef({
							d_name: name,
							d_doc: doc,
							d_meta: meta,
							d_params: tl,
							d_flags: c.map(syntax.Parser.decl_flag_to_enum_flag),
							d_data: t
						}), pos: punion(p1,t.pos)};
					case [{td:Kwd(Abstract), pos:p1}, name = typeName(), tl = parseConstraintParams(), st = parseAbstractSubtype(), sl = plist(parseAbstractRelations), {td:BrOpen}, fl = parseClassFields(false, p1)]:
						// var flags = c.map(function(flag):core.Ast.AbstractFlag return switch(flag.e) { case EPrivate: APrivAbstract; case EExtern: AExtern; });
						var flags = c.map(syntax.Parser.decl_flag_to_abstract_flag);
						switch (st) {
							case None:
							case Some(t): flags.unshift(AIsType(t));
						}
						{ decl: EAbstract({
							d_name: name,
							d_doc: doc,
							d_meta: meta,
							d_params: tl,
							d_flags: flags.concat(sl),
							d_data: fl.fields
						}), pos: punion(p1, fl.pos)};
				}
		}
	}

	function parseClass(doc:Option<String>, meta:core.Ast.Metadata, cflags:Array<{fst: core.Ast.ClassFlag, snd:String}>, needName:Bool) : core.Ast.TypeDecl {
		var optName = if (needName) typeName else function() {
			var t = popt(typeName);
			return switch(t) {
				case None: {pack:"", pos:core.Globals.null_pos};
				case Some(n): n;
			};
		}
		return switch stream {
			// case [flags = parseClassFlags(), name = optName(), tl = parseConstraintParams(), hl = psep(Comma,parseClassHerit), {td: BrOpen}, fl = parseClassFields(false,flags.pos)]:
			case [flags = parseClassFlags(), name = optName(), tl = parseConstraintParams(), hl = plist(parseClassHerit), {td: BrOpen}, fl = parseClassFields(!needName, flags.pos)]:
				{ decl: EClass({
					d_name: name,
					d_doc: doc,
					d_meta: meta,
					d_params: tl,
					d_flags: cflags.map(function(i) return i.fst).concat(flags.flags).concat(hl),
					d_data: fl.fields
				}), pos: punion(flags.pos,fl.pos)};
		}
	}

	function parseImport(p1:core.Globals.Pos) : core.Ast.TypeDecl {
		function loop (acc:Array<core.Ast.PlacedName>) : {pos:core.Globals.Pos, acc:Array<core.Ast.PlacedName>, mode:core.Ast.ImportMode} {
			return switch stream {
				case [{td:Dot, pos:p}]:
					function resume () {
						var r = [];
						for (a in acc) {
							r.unshift(a.pack);
						}
						return syntax.Parser.type_path(r, true);
					}
					if (syntax.Parser.is_resuming(p)) { resume(); }
					switch stream {
						case [{td:Const(CIdent(k)), pos:p}]:
							loop(aunshift(acc, {pack:k, pos:p}));
						case [{td:Kwd(Macro), pos:p}]:
							loop(aunshift(acc, {pack:"macro", pos:p}));
						case [{td:Kwd(Extern), pos:p}]:
							loop(aunshift(acc, {pack:"extern", pos:p}));
						case [{td:Binop(OpMult)}, {td:Semicolon, pos:p2 }]:
							acc.reverse();
							{pos:p2, acc:acc, mode:IAll};
						case [{td:Binop(OpOr)} && syntax.Parser.do_resume()]:
							syntax.Parser.set_resume(p);
							resume();
						case _:
							syntax.Parser.serror();
					}
				case [{td:Semicolon, pos:p2}]:
					acc.reverse();
					{pos:p2, acc:acc, mode:INormal};
				case [{td:Kwd(In)}, {td:Const(CIdent(name))}, {td:Semicolon, pos:p2}]:
					acc.reverse();
					{pos:p2, acc:acc, mode:IAsName(name)};
				case [{td:Const(CIdent("as"))}, {td:Const(CIdent(name))}, {td:Semicolon, pos:p2}]:
					acc.reverse();
					{pos:p2, acc:acc, mode:IAsName(name)};
				case _:
					syntax.Parser.serror();
			}
		}

		var path : {pos:core.Globals.Pos, acc:Array<core.Ast.PlacedName>, mode:core.Ast.ImportMode} = switch stream {
			case [{td:Const(CIdent(name)), pos:p}]: loop([{pack:name, pos:p}]);
			case _:
				if (syntax.Parser.would_skip_resume(p1, this)) {
					{pos:p1, acc:[], mode:INormal};
				}
				else {
					syntax.Parser.serror();
				}
		};

		return {decl:EImport({pns:path.acc, mode:path.mode}), pos:punion(p1, path.pos)};
	}

	function parseUsing(p1:core.Globals.Pos) : core.Ast.TypeDecl {
		function loop (acc) : {pos:core.Globals.Pos, acc:Array<core.Ast.PlacedName>} {
			return switch stream {
				case [{td:Dot, pos:p}]:
					switch stream {
						case [{td:Const(CIdent(k)), pos:p}]:
							loop(aunshift(acc, {pack:k, pos:p}));
						case [{td:Kwd(Macro), pos:p}]:
							loop(aunshift(acc, {pack:"macro", pos:p}));
						case [{td:Kwd(Extern), pos:p}]:
							loop(aunshift(acc, {pack:"extern", pos:p}));
						case _:
							if (syntax.Parser.is_resuming(p)) {
								var res = [];
								for (a in acc) {
									res.unshift(a.pack);
								}
								syntax.Parser.type_path(res, false);
							}
							syntax.Parser.serror();
					}
				case [{td:Semicolon, pos:p2}]:
					acc.reverse();
					{pos:p2, acc:acc};
			}
		}

		var path = switch stream {
			case [{td:Const(CIdent(name)), pos:p}]: loop([{pack:name, pos:p}]);
			case _:
				if (syntax.Parser.would_skip_resume(p1, this)) {
					{pos:p1, acc:[]};
				}
				else {
					syntax.Parser.serror();
				}
		};
		return {decl:EUsing(path.acc), pos:punion(p1, path.pos)};
	}

	function parseAbstractRelations() : core.Ast.AbstractFlag {
		return switch stream {
			case [{td:Const(CIdent("to"))}, t = parseComplexType()]: AToType(t);
			case [{td:Const(CIdent("from"))}, t = parseComplexType()]: AFromType(t);
		}
	}

	function parseAbstractSubtype() : Option<core.Ast.TypeHint> {
		return switch stream {
			case [{td:POpen}, t = parseComplexType(), {td:PClose}]: Some(t);
			case _: None;
		}
	}

	function parsePackage() {
		return psep(Dot, lowerIdentOrMacro);
	}

	function parseClassFields(tdecl:Bool, p1:core.Globals.Pos):{fields:Array<core.Ast.ClassField>, pos:core.Globals.Pos} {
		var l = parseClassFieldResume(tdecl);
		var p2 = switch stream {
			case [{td: BrClose, pos: p2}]:
				p2;
			case _:
				if (syntax.Parser.do_resume()) {
					syntax.Parser.last_token(this).pos;
				}
				else {
					syntax.Parser.serror();
				}
		}
		return {
			fields: l,
			pos: p2
		}
	}

	function parseClassFieldResume(tdecl:Bool):Array<core.Ast.ClassField> {
		function catchFunction() : Array<core.Ast.ClassField> {
			// look for next variable/function or next type declaration
			function junk(k:Int) : Void {
				if (k>0) {
					this.junk();
					junk(k-1);
				}
			}
			// walk back tokens which are prefixing a type/field declaration
			function junk_tokens(k:Int) : Void {
				if (k!=0) {
					var acc = [];
					for (e in npeek(k)) {
						acc.unshift(e.td);
					}
					if (acc.length > 0) {
						switch (acc[0]) {
							case Kwd(Private): junk_tokens(k-1);
							case Const(CIdent(_)), Kwd(_):
								if (acc.length >= 3 && acc[1] == DblDot && acc[2] == At) {
									junk_tokens(acc.length - 3);
								}
								else if (acc.length >= 2 && acc[1] == At) {
									junk_tokens(acc.length - 2);
								}
							case PClose:
								// count matching parenthesises for metadata call
								function loop(n:Int, l:Array<core.Ast.Token>) : Array<core.Ast.Token>{
									if (l.length == 0) {
										return [];
									}
									return switch (l[0]) {
										case POpen: (n == 0) ? l.slice(1) : loop(n-1, l.slice(1));
										case PClose: loop(n+1, l.slice(1));
										case _: loop(n, l.slice(1));
									}
								}
								var looped = loop(0, acc.slice(1));
								if (looped.length == 0) { junk(k); }
								switch (looped[0]) {
									case Const(CIdent(_)), Kwd(_):
										if (looped.length >= 3 && looped[1] == DblDot && looped[2] == At) {
											junk_tokens(looped.length - 3);
										}
										else if (looped.length >= 2 && looped[1] == At) {
											junk_tokens(looped.length - 2);
										}
									case _: junk(k);
								}
							default: junk(k);
						}
					}
					else {
						junk(k);
					}
				}
			}

			function loop(k:Int) : Array<core.Ast.ClassField>{
				var acc = [];
				for (e in npeek(k)) {
					acc.unshift(e.td);
				}
				var l =acc.length;
				var l =acc.length;
				if (l == 0) { return [];}
				switch (acc[0]) {
					case Kwd(e):
						// metadata
						if ((l > 1 && acc[1] == At) || (l > 2 && acc[1] == DblDot && acc[2] == At)) {
							return loop(k+1);
						}
						// field declaration (1)
						else if (e == New && l > 1 && std.Type.enumEq(acc[1], Kwd(Function))) {
							junk_tokens(k - 2);
							return parseClassFieldResume(tdecl);
						}
						else if (e == Macro || e == Public || e == Static || e == Var || e == Final || e == Override || e == Dynamic || e == Inline) {
							junk_tokens(k-1);
							return parseClassFieldResume(tdecl);
						}
						// type declaraion (1)
						else if (e == Import || e == Using || e == Extern || e == Class || e == Interface || e == Enum || e == Typedef || e == Abstract ) {
							junk_tokens(k-1);
							return [];
						}
					//field declaration (2)
					case Const(_):
						if (l > 1 && std.Type.enumEq(acc[1], Kwd(Function))) {
							junk_tokens(k-2);
							return parseClassFieldResume(tdecl);
						}
					case BrClose:
						if (tdecl) {
							junk_tokens(k-1);
							return [];
						}
					// type declaration (2)
					case Eof:
						junk_tokens(k-1);
						return [];
					case _:

				}
				return loop(k+1);
			}
			return loop(1);
		}

		if (!syntax.Parser.do_resume()) {
			return plist(parseClassField);
		}
		return try {
			var c = parseClassField();
			aunshift(parseClassFieldResume(tdecl), c);
		}
		catch (_:ocaml.stream.Error) { catchFunction();}
		catch (_:ocaml.stream.Failure) { catchFunction();}
		catch (_:hxparse.NoMatch<Dynamic>) { catchFunction();}
	}

	function parseCommonFlags():Array<syntax.Parser.DeclFlag> {
		return switch stream {
			case [{td:Kwd(Private)}, l = parseCommonFlags()]: apush(l, DPrivate);
			case [{td:Kwd(Extern)}, l = parseCommonFlags()]: apush(l, DExtern);
			case _: [];
		}
	}

	function parseMetaArgumentExpr () : core.Ast.Expr {
		return try {
			expr();
		}
		catch (err:syntax.parser.Display) {
			switch (err.expr.expr) {
				case EDisplay(e,_):
					try {
						syntax.Parser.type_path(core.Ast.string_list_of_expr_path_raise(e), false);
					}
					catch (e:ocaml.Exit) {
						err.expr;
					}
				case _: err.expr;
			}
		}
	}

	function parseMetaParams(pname:core.Globals.Pos) {
		return switch stream {
			case [{td: POpen, pos:p} && p.pmin == pname.pmax, params = psep(Comma, parseMetaArgumentExpr), {td: PClose}]: params;
			case _: [];
		}
	}

	function parseMetaEntry() : core.Ast.MetadataEntry {
		return switch stream {
			case [{td:At, pos:p1}]:
				switch stream {
					case [name = metaName(p1), params = parseMetaParams(name.pos)]:
						{name: name.name, params: params, pos: name.pos};
					case _:
						if (syntax.Parser.is_resuming(p1)) {
							{name:Last, params:[],pos:p1};
						}
						else {
							syntax.Parser.serror();
						}
				}
		}
	}

	function parseMeta() : core.Ast.Metadata {
		return switch stream {
			case [entry = parseMetaEntry()]: apush(parseMeta(), entry);
			case _: [];
		}
	}

	function metaName(p1:core.Globals.Pos) : {name:core.Meta.StrictMeta, pos:core.Globals.Pos} {
		return switch stream {
			case [{td:Const(CIdent(i)), pos:p} && p.pmin == p1.pmax]: {name: Custom(i), pos: p};
			case [{td:Kwd(k), pos:p} && p.pmin == p1.pmax]: {name: Custom(core.Ast.s_keyword(k)), pos:p};
			case [{td:DblDot, pos:p} && p.pmin == p1.pmax]:
				switch stream {
					case [{td:Const(CIdent(i)), pos:p1} && p1.pmin == p.pmax]: {name: core.Meta.parse(i), pos: punion(p,p1)};
					case [{td:Kwd(k), pos:p1} && p1.pmin == p.pmax]: {name: core.Meta.parse(core.Ast.s_keyword(k)), pos:punion(p,p1)};
					case _:
						if (syntax.Parser.is_resuming(p)) {
							{name:Last, pos:p};
						}
						else {
							throw ocaml.stream.Failure.instance;
						}
				}
		}
	}

	function parseEnumFlags() : {flags:Array<core.Ast.EnumFlag>, pos:core.Globals.Pos} {
		return switch stream {
			case [{td:Kwd(Enum), pos:p}]: {flags: [], pos: p};
		}
	}

	function parseClassFlags() : {flags:Array<core.Ast.ClassFlag>, pos:core.Globals.Pos} {
		return switch stream {
			case [{td:Kwd(Class), pos:p}]: {flags: [], pos: p};
			case [{td:Kwd(Interface), pos:p}]: {flags: [core.Ast.ClassFlag.HInterface], pos: p};
		}
	}

	function parseComplexTypeAt(p:core.Globals.Pos) : core.Ast.TypeHint {
		return switch stream {
			case [t = parseComplexType()]: t;
			case _:
				if (syntax.Parser.is_resuming(p))
					{ct:CTPath({tpackage:[], tname:"", tparams:[], tsub:None}), pos:p};
				else
					syntax.Parser.serror();
		}
	}

	function parseTypeHint() : core.Ast.TypeHint {
		return switch stream {
			case [{td: DblDot, pos:p1}, t = parseComplexTypeAt(p1)]: t;
		}
	}

	function parseTypeOpt() : Option<core.Ast.TypeHint> {
		return switch stream {
			case [t = parseTypeHint()]: Some(t);
			case _: None;
		}
	}

	function parseComplexType() : core.Ast.TypeHint {
		return parseComplexTypeMaybeNamed(false);
	}

	function parseComplexTypeMaybeNamed(allowNamed:Bool) {
		return switch stream {
			case [{td:POpen, pos:p1}, tl = psep(Comma, parseComplexTypeMaybeNamed.bind(true)), {td:PClose, pos:p2}]:
				if (tl.length == 0) {
					//it was () or (a:T) - clearly a new function type syntax, proceed with parsing return type
					parseFunctionTypeNext(tl, p1);
				}
				else if (tl.length == 1) {
					switch(tl[0].ct) {
						case CTNamed(_,_):
							//it was () or (a:T) - clearly a new function type syntax, proceed with parsing return type
							parseFunctionTypeNext(tl, p1);
						case _:
							// it was some single unnamed type in parenthesis - use old function type syntax
							var t:core.Ast.TypeHint = {ct:CTParent(tl[0]), pos:punion(p1, p2)};
							parseComplexTypeNext(t);
					}
				}
				else {
					// it was multiple arguments - clearly a new function type syntax, proceed with parsing return type
					parseFunctionTypeNext(tl, p1);
				}
			case _:
				var t = parseComplexTypeInner(allowNamed);
				parseComplexTypeNext(t);
		}
	}

	function parseStructuralExtension() : core.Ast.PlacedTypePath {
		return switch stream {
			case [{td: Binop(OpGt)}, t = parseTypePath(), {td: Comma}]: t;
		}
	}

	function parseComplexTypeInner (allowNamed:Bool) : core.Ast.TypeHint {
		return switch stream {
			case [{td:POpen, pos:p1}, t = parseComplexType(), {td:PClose, pos:p2}]: {ct:CTParent(t), pos:punion(p1, p2)};
			case [{td:BrOpen, pos: p1}]:
				switch stream {
					case [l = parseTypeAnonymous(false)]: {ct:CTAnonymous(l.fst), pos:punion(p1,l.snd)};
					case [t = parseStructuralExtension()]:
						var tl = aunshift(plist(parseStructuralExtension), t);
						switch stream {
							case [l = parseTypeAnonymous(false)]: {ct:CTExtend(tl,l.fst), pos:punion(p1, l.snd)};
							case [fl = parseClassFields(true, p1)]: {ct:CTExtend(tl, fl.fields), pos:punion(p1,fl.pos)};
						}
					case [l = parseClassFields(true, p1)]: {ct:CTAnonymous(l.fields), pos:punion(p1, l.pos)};
					case _: syntax.Parser.serror();
				}
			case [{td:Question, pos:p1}, t = parseComplexTypeInner(allowNamed)]:
				{ct:CTOptional(t), pos:punion(p1,t.pos)};
			case [n = dollarIdent()]:
				switch stream {
					case [{td:DblDot} && allowNamed, t = parseComplexType()]:
						{ct:CTNamed(n,t), pos:punion(n.pos, t.pos)}
					case _:
						var t = parseTypePath2(None,[], n.pack, n.pos);
						{ct:CTPath(t.tp), pos:t.pos}
				}
			case [t = parseTypePath()]:
				{ct:CTPath(t.tp), pos:t.pos};
		}
	}

	function parseTypePath() : core.Ast.PlacedTypePath {
		return parseTypePath1(None, []);
	}

	function parseTypePath1(p0:Option<core.Globals.Pos>, pack:Array<String>) : core.Ast.PlacedTypePath {
		return switch stream {
			case [ident = dollarIdentMacro(pack)]:
				parseTypePath2(p0, pack, ident.pack, ident.pos);
			case [{td:Binop(OpOr)} && syntax.Parser.do_resume()]:
				var rev = pack.copy();
				rev.reverse();
				throw new syntax.parser.TypePath(rev, None, false);
		}
	}

	function parseTypePath2(p0:Option<core.Globals.Pos>, pack:Array<String>, name:String, p1:core.Globals.Pos) : core.Ast.PlacedTypePath {
		if (isLowerIdent(name)) {
			return switch stream {
				case [{td:Dot, pos:p}]:
					if (syntax.Parser.is_resuming(p)) {
						var rev = [name].concat(pack);
						rev.reverse();
						throw new syntax.parser.TypePath(rev, None, false);
					}
					else {
						var op = switch (p0) {
							case None:Some(p1);
							case Some(_): p0;
						}
						parseTypePath1(op, [name].concat(pack));
					}
				case [{td:Semicolon}]:
					syntax.Parser.error(Custom("Type name should start with an uppercase letter"), p1);
				case _: syntax.Parser.serror();
			}
		}
		else {
			var sub = switch stream {
				case [{td:Dot, pos:p}]:
					if (syntax.Parser.is_resuming(p)) {
						var rev = pack.copy();
						rev.reverse();
						throw new syntax.parser.TypePath(rev, Some({c:name, cur_package:false}), false);
					}
					else {
						switch stream {
							case [{td:Const(CIdent(name)), pos:p2} && !isLowerIdent(name)]: {fst:Some(name), snd:p2};
							case [{td:Binop(OpOr)} && syntax.Parser.do_resume()]:
								syntax.Parser.set_resume(p);
								var rev = pack.copy();
								rev.reverse();
								throw new syntax.parser.TypePath(rev, Some({c:name, cur_package:false}),false);
							case _: syntax.Parser.serror();
						}
					}
				case _: {fst:None, snd:p1};
			};
			var params = switch stream {
				case [{td:Binop(OpLt)}, l = psep(Comma, parseTypePathOrConst), {td:Binop(OpGt), pos:p2}]:
					{fst:l, snd:p2};
				case _: {fst:[], snd:sub.snd};
			};
			var rev = pack.copy();
			rev.reverse();
			var op = switch (p0) {
				case None:p1;
				case Some(p): p;
			};
			return {tp:{
					tpackage:rev,
					tname:name,
					tparams:params.fst,
					tsub: sub.fst
				}, pos:punion(op, params.snd)};
		}
	}

	function typeName() : core.Ast.PlacedName {
		return switch stream {
			case [{td: Const(CIdent(name)), pos:p}]:
				if (isLowerIdent(name)) throw new syntax.parser.Error(Custom("Type name should start with an uppercase letter"), p);
				else {pack:name, pos:p};
			case [{td: Dollar(name), pos:p}]:
				  {pack:"$"+name, pos:p};
		}
	}

	function parseTypePathOrConst() : core.Ast.TypeParamOrConst {
		// we can't allow (expr) here
		return switch stream {
			case [{td:BkOpen, pos: p1}, l = parseArrayDecl(), {td:BkClose, pos:p2}]: TPExpr({expr: EArrayDecl(l), pos:punion(p1,p2)});
			case [t = parseComplexType()]: TPType(t);
			case [{td:Const(c), pos:p}]: TPExpr({expr:EConst(c), pos:p});
			case [e = expr()]: TPExpr(e);
			case _: syntax.Parser.serror();
		}
	}

	function parseComplexTypeNext(t:core.Ast.TypeHint) : core.Ast.TypeHint {
		return switch stream {
			case [{td:Arrow}, t2 = parseComplexType()]:
				switch(t2.ct) {
					case CTFunction(args,r):
						{ct:CTFunction(aunshift(args,t),r), pos:punion(t.pos, t2.pos)};
					case _:
						{ct:CTFunction([t],t2), pos:punion(t.pos, t2.pos)};
				}
			case _: t;
		}
	}

	function parseFunctionTypeNext (tl:Array<core.Ast.TypeHint>, p1:core.Globals.Pos) : core.Ast.TypeHint {
		return switch stream {
			case [{td:Arrow}, tret = parseComplexTypeInner(false)]:
				{ct:CTFunction(tl, tret), pos:punion(p1, tret.pos)};
			case _: syntax.Parser.serror();
		}
	}

	function parseTypeAnonymous(opt:Bool) : {fst:Array<core.Ast.ClassField>, snd:core.Globals.Pos} {
		return switch stream {
			case [{td:Question} && !opt]: parseTypeAnonymous(true);
			case [id = ident(), t = parseTypeHint()]:
				var p2 = last.pos;
				function next(acc:ImmutableList<core.Ast.ClassField>) : ImmutableList<core.Ast.ClassField> {
					var cf:core.Ast.ClassField = {
						cff_name: id,
						cff_meta: opt ? [{name:core.Meta.StrictMeta.Optional,params:[], pos:core.Globals.null_pos}] : [],
						cff_access: [],
						cff_doc: None,
						cff_kind: core.Ast.ClassFieldKind.FVar(Some(t),None),
						cff_pos: punion(id.pos, p2)
					};
					return cf :: acc;
				}
				switch stream {
					case [{td:BrClose, pos:p2}]: {fst:next([]), snd:p2};
					case [{td:Comma, pos:p2}]:
						switch stream {
							case [{td:BrClose, pos:p2}]: {fst:next([]), snd:p2};
							case [l = parseTypeAnonymous(false)]: {fst:next(l.fst), snd:punion(id.pos, l.snd)};
							case _: syntax.Parser.serror();
						}
					case _: syntax.Parser.serror();
				}
		}
	}

	function parseEnum() : core.Ast.EnumConstructor {
		var doc = getDoc();
		var meta = parseMeta();
		return switch stream {
			case [name = ident(), params = parseConstraintParams()]:
				var args = switch stream {
					case [{td:POpen}, l = psep(Comma, parseEnumParam), {td:PClose}]: l;
					case _: [];
				}
				var t = popt(parseTypeHint);
				var p2 = switch stream {
					case [p = semicolon()]: p;
					case _: syntax.Parser.serror();
				}
				{
					ec_name: name,
					ec_doc: doc,
					ec_meta: meta,
					ec_args: args,
					ec_params: params,
					ec_type: t,
					ec_pos: punion(name.pos, p2)
				}
		}
	}

	function parseEnumParam() : {name:String, type:core.Ast.TypeHint, opt:Bool} {
		return switch stream {
			case [{td:Question}, name = ident(), t = parseTypeHint()]: { name: name.pack, opt: true, type: t};
			case [name = ident(), t = parseTypeHint()]: { name: name.pack, opt: false, type: t };
		}
	}

	function parseFunctionField(doc:Option<String>, meta:core.Ast.Metadata, al:Array<core.Ast.Access>) : {name:core.Ast.PlacedName, kind:core.Ast.ClassFieldKind, pos:core.Globals.Pos, access:Array<core.Ast.Access>}{
		return switch stream {
			case [{td:Kwd(Function), pos:p1}, name = parseFunName(), pl = parseConstraintParams(), {td:POpen}, args = psep(Comma, parseFunParam), {td:PClose}, t = popt(parseTypeHint)]:
				var e = switch stream {
					case [e = toplevelExpr()]:
						try {
							semicolon();
						}
						catch (er:syntax.parser.Error) {
							switch (er.error_msg) {
								case Missing_semicolon:
									syntax.Parser.display_error(Missing_semicolon, er.pos);
								case _: throw er;
							}
						}
						{ expr: Some(e), pos: e.pos };
					case [{td: Semicolon,pos:p}]:
						{ expr: None, pos: p}
					case _: syntax.Parser.serror();
				}
				var f:core.Ast.Func = {
					f_params: pl,
					f_args: args,
					f_type: t,
					f_expr: e.expr
				};
				{
					name: name,
					kind: core.Ast.ClassFieldKind.FFun(f),
					pos: punion(p1, e.pos),
					access: al
				};
		};
	}

	function parseVarFieldAssignment () : {expr:Option<core.Ast.Expr>, pos:core.Globals.Pos} {
		return switch stream {
			case [{td:Binop(OpAssign)}, e = toplevelExpr(), p2 = semicolon()]: { expr: Some(e), pos: p2 };
			case [{td:Semicolon, pos:p2}]: { expr: None, pos: p2 };
			case _: syntax.Parser.serror();
		}
	}

	function parseClassField():core.Ast.ClassField {
		var doc = getDoc();
		return switch stream {
			case [meta = parseMeta(), al = parseCfRights(true,[])]:
				var data = switch stream {
					case [{td:Kwd(Var), pos:p1}, name = dollarIdent()]:
						switch stream {
							case [{td:POpen}, i1 = propertyIdent(), {td:Comma}, i2 = propertyIdent(), {td:PClose}]:
								var t = popt(parseTypeHint);
								var e = parseVarFieldAssignment();
								{
									name: name,
									pos: punion(p1,e.pos),
									kind: core.Ast.ClassFieldKind.FProp(i1, i2, t, e.expr),
									access:al
								}
							case [t = popt(parseTypeHint)]:
								var e = parseVarFieldAssignment();
								{
									name: name,
									pos: punion(p1,e.pos),
									kind: core.Ast.ClassFieldKind.FVar(t ,e.expr),
									access:al
								}
						}
					case [{td:Kwd(Final), pos:p1}]:
						switch stream {
							case [name = dollarIdent(), t = popt(parseTypeHint), e = parseVarFieldAssignment()]:
								al.unshift(AFinal);
								{
									name:name,
									pos: punion(p1, e.pos),
									kind: core.Ast.ClassFieldKind.FVar(t, e.expr),
									access:al
								};
							case [al=parseCfRights(al.indexOf(AStatic) == -1, [core.Ast.Access.AFinal].concat(al)), f = parseFunctionField(doc, meta, al)]:
								f;
							case _:
								syntax.Parser.serror();
						}
					case [f = parseFunctionField(doc, meta, al)]:
						f;
					case _:
						if (al.length == 0) {
							throw ocaml.stream.Failure.instance;
						}
						else {
							syntax.Parser.serror();
						}
				};
				{
					cff_name: data.name,
					cff_doc: doc,
					cff_meta: meta,
					cff_access: data.access,
					cff_pos: data.pos,
					cff_kind: data.kind
				};
		}
	}

	function parseCfRights(allowStatic:Bool, l:Array<core.Ast.Access>) : Array<core.Ast.Access> {
		return switch stream {
			case [{td:Kwd(Static)} && allowStatic, l = parseCfRights(false, aunshift(l, AStatic))]: l;
			case [{td:Kwd(Macro)} && !(l.indexOf(AMacro) != -1), l = parseCfRights(allowStatic, aunshift(l, AMacro))]: l;
			case [{td:Kwd(Public)} && !(l.indexOf(APublic) != -1|| l.indexOf(APrivate) != -1), l = parseCfRights(allowStatic, aunshift(l, APublic))]: l;
			case [{td:Kwd(Private)} && !(l.indexOf(APublic) != -1|| l.indexOf(APrivate) != -1), l = parseCfRights(allowStatic, aunshift(l, APrivate))]: l;
			case [{td:Kwd(Override)} && !(l.indexOf(AOverride) != -1), l = parseCfRights(false, aunshift(l, AOverride))]: l;
			case [{td:Kwd(Dynamic)} && !(l.indexOf(ADynamic) != -1), l = parseCfRights(allowStatic, aunshift(l, ADynamic))]: l;
			case [{td:Kwd(Inline)}, l = parseCfRights(allowStatic, aunshift(l, AInline))]: l;
			case _: l;
		}
	}

	function parseFunName() : core.Ast.PlacedName {
		return switch stream {
			case [id = dollarIdent()]: id;
			case [{td:Kwd(New), pos:p}]: {pack:"new", pos:p};
		}
	}

	function parseFunParam() : core.Ast.FunArg {
		var meta = parseMeta();
		return switch stream {
			case [{td:Question}, id = dollarIdent(), t = popt(parseTypeHint), c = parseFunParamValue()]: { name: id, opt: true, type: t, value: c, meta: meta };
			case [id = dollarIdent(), t = popt(parseTypeHint), c = parseFunParamValue()]: { name: id, opt: false, type: t, value: c, meta: meta };

		}
	}

	function parseFunParamValue() : Option<core.Ast.Expr> {
		return switch stream {
			case [{td:Binop(OpAssign)}, e = toplevelExpr()]: Some(e);
			case _: None;
		}
	}

	// Never called
	function parseFunParamType() {
		// Show if called somehow
		trace("haxeparser.HaxeParser.parsFunParamType has been called");
		return switch stream {
			case [{td:Question}, id = ident(), t = parseTypeHint()]: { name: id, opt: true, type: t};
			case [ id = ident(), t = parseTypeHint()]: { name: id, opt: false, type: t};
		}
	}

	function parseConstraintParams() : Array<core.Ast.TypeParam> {
		return switch stream {
			case [{td:Binop(OpLt)}, l = psep(Comma, parseConstraintParam), {td:Binop((OpGt))}]: l;
			case _: [];
		}
	}

	function parseConstraintParam() : core.Ast.TypeParam {
		return switch stream {
			case [meta = parseMeta(), name = typeName()]:
				var params = [];
				var ctl = switch stream {
					case [{td:DblDot}]:
						switch stream {
							case [{td:POpen}, l = psep(Comma, parseComplexType), {td:PClose}]: l;
							case [t = parseComplexType()]: [t];
							case _: syntax.Parser.serror();
						}
					case _: [];
				}
				{
					tp_name: name,
					tp_params: params,
					tp_constraints: ctl,
					tp_meta: meta
				}
		}
	}

	function parseTypePathOrResume(p1:core.Globals.Pos) : core.Ast.PlacedTypePath{
		return switch stream {
			case [t = parseTypePath()] : t;
			case _:
				if (syntax.Parser.would_skip_resume(p1, this)) {
					{tp:{tpackage:[], tname:"", tparams:[], tsub:None}, pos:core.Globals.null_pos}
				}
				else {
					throw ocaml.stream.Failure.instance;
				}
		}
	}

	function parseClassHerit() : core.Ast.ClassFlag {
		return switch stream {
			case [{td:Kwd(Extends), pos:p1}, t = parseTypePathOrResume(p1)]: HExtends(t);
			case [{td:Kwd(Implements), pos:p1}, t = parseTypePathOrResume(p1)]: HImplements(t);
		}
	}

	function block1() : core.Ast.ExprDef {
		return switch stream {
			case [{td:Const(CIdent(name)), pos:p}]: block2(name, NoQuotes, CIdent(name), p);
			case [{td:Const(CString(name)), pos:p}]: block2(name, DoubleQuotes, CString(name), p);
			case [b = block([])]: EBlock(b);
		}
	}

	function block2(name:String, quotes:core.Ast.QuoteStatus, ident:core.Ast.Constant, p:core.Globals.Pos) : core.Ast.ExprDef {
		return switch stream {
			case [{td:DblDot}, e = expr(), l = parseObjDecl()]:
				l.unshift({name:name, expr:e, quotes:quotes, pos:p});
				EObjectDecl(l);
			case _:
				var e = exprNext({expr:EConst(ident), pos: p});
				try {
					semicolon();
					var b = block([e]);
					EBlock(b);
				}
				catch (err:syntax.parser.Error) {
					syntax.Parser.display_error(err.error_msg, err.pos);
					EBlock(block([e]));
				}
		}
	}

	function block(acc:Array<core.Ast.Expr>) : Array<core.Ast.Expr> {
		return blockWithPos(acc, core.Globals.null_pos).acc;
	}

	function blockWithPos(acc:Array<core.Ast.Expr>, p:core.Globals.Pos) : {acc:Array<core.Ast.Expr>, pos:core.Globals.Pos} {
		return try {
			// because of inner recursion, we can't put Display handling in errors below
			var e = try {
				parseBlockElt();
			}
			catch (e:syntax.parser.Display) {
				syntax.Parser.display({expr:EBlock(apush(acc,e.expr)), pos:e.expr.pos});
			}
			blockWithPos(apush(acc,e), e.pos);
		}
		catch (_:hxparse.NoMatch<Dynamic>) { // Stream.Failure ?
			return {acc:acc, pos:p};
		}
		catch (_:ocaml.stream.Failure) {
			return {acc:acc, pos:p};
		}
		catch (_:ocaml.stream.Error) {
			var tk = nextToken();
			syntax.Parser.display_error(Unexpected(tk.td), tk.pos);
			return blockWithPos(acc, tk.pos);
		}
		catch (e:syntax.parser.Error) {
			syntax.Parser.display_error(e.error_msg, e.pos);
			return blockWithPos(acc, p);
		}

	}

	function parseBlockElt() : core.Ast.Expr {
		return switch stream {
			case [{td:Kwd(Var), pos:p1}, vl = parseVarDecls(p1), p2 = semicolon()]:
				{ expr: EVars(vl), pos:punion(p1,p2)};
			case [{td:Kwd(Inline), pos:p1}, {td:Kwd(Function)}, e = parseFunction(p1, true), _ = semicolon()]: e;
			case [e = expr(), _ = semicolon()]: e;
		}
	}

	function parseObjDecl() : Array<core.Ast.ObjectField> {
		var acc:Array<core.Ast.ObjectField> = [];
		while(true) {
			switch stream {
				case [{td:Comma}]:
					switch stream {
						case [id = ident(), {td:DblDot}, e = expr()]:
							acc.push({name:id.pack, expr: e, quotes: NoQuotes, pos:id.pos});
						case [{td:Const(CString(name)), pos:p}, {td:DblDot}, e = expr()]:
							acc.push({name:name, expr: e, quotes: DoubleQuotes, pos:p});
						case _:
							break;
					}
				case _:
					break;
			}
		}
		return acc;
	}

	function parseArrayDecl() : Array<core.Ast.Expr> {
		var acc = [];
		var br = false;
		while(true) {
			switch stream {
				case [e = expr()]:
					acc.push(e);
					switch stream {
						case [{td: Comma}]:
						case _: br = true;
					}
				case _: br = true;
			}
			if (br) break;
		}
		return acc;
	}

	function parseVarDeclHead () : {name:String, t:Option<core.Ast.TypeHint>, p:core.Globals.Pos} {
		return switch stream {
			case [id=dollarIdent(), t = popt(parseTypeHint)]: {name:id.pack, t:t, p:id.pos};
		};
	}

	function parseVarAssignement () : Option<core.Ast.Expr> {
		return switch stream {
			case [{td:Binop(OpAssign), pos:p1}]:
				switch stream {
					case [e = expr()]: Some(e);
					case _: syntax.Parser.error(Custom("expression expected after ="), p1);
				};
			case _: None;
		};
	}

	function parseVarAssignementResume (vl:Array<core.Ast.Var>, name:String, pn:core.Globals.Pos, t:Option<core.Ast.TypeHint>) : core.Ast.Var {
			try {
				var eo = parseVarAssignement();
				return {name:{pack:name, pos:pn},type:t, expr:eo};
			}
			catch (err:syntax.parser.Display) {
				var v = {name:{pack:name, pos:pn},type:t, expr:Some(err.expr)};
				var e:core.Ast.Expr = {expr:EVars(apush(vl, v)), pos:punion(pn, err.expr.pos)};
				syntax.Parser.display(e);
			}
	}

	function parseVarDeclsNext (vl:Array<core.Ast.Var>) : Array<core.Ast.Var> {
		return switch stream {
			case [{td:Comma, pos:p1}, head=parseVarDeclHead()]:
				var decl = parseVarAssignementResume(vl, head.name, head.p, head.t);
				parseVarDeclsNext(apush(vl, decl));
			case _: vl;
		}
	}

	function parseVarDecls (p1:core.Globals.Pos) : Array<core.Ast.Var> {
		return switch stream {
			case [head = parseVarDeclHead()]:
				var decl = parseVarAssignementResume([], head.name, head.p, head.t);
				parseVarDeclsNext([decl]);
			case _:
				syntax.Parser.error(Custom("Missing variable identifier"),p1);
		}
	}

	function parseVarDecl () : core.Ast.Var {
		return switch stream {
			case [head = parseVarDeclHead(), decl = parseVarAssignementResume([], head.name, head.p, head.t)]: decl;
		};
	}

	function inlineFunction() : {isInline:Bool, pos:core.Globals.Pos} {
		return switch stream {
			case [{td:Kwd(Inline)}, {td:Kwd(Function), pos:p1}]: { isInline: true, pos: p1};
			case [{td:Kwd(Function), pos: p1}]: { isInline: false, pos: p1};
		}
	}

	function parseMacroExpr(p:core.Globals.Pos) : core.Ast.Expr {
		return switch stream {
			case [{td:DblDot}, t = parseComplexType()]:
				var toType = syntax.Reification.reify(syntax.Parser.in_macro).toType;
				var t = toType(t,p);
				{ expr: ECheckType(t, {ct:CTPath({tpackage:["haxe","macro"], tname:"Expr", tsub:Some("ComplexType"), tparams: []}), pos:core.Globals.null_pos}), pos: p};
			case [{td:Kwd(Var), pos:p1}, vl = psep(Comma, parseVarDecl)]:
				syntax.Reification.reifyExpr({expr:EVars(vl), pos:p1}, syntax.Parser.in_macro);
			case [d = parseClass(None, [],[],false)]:
				var toType = syntax.Reification.reify(syntax.Parser.in_macro).toTypeDef;
				{ expr: ECheckType(toType(d), {ct:CTPath({tpackage:["haxe","macro"], tname:"Expr", tsub:Some("TypeDefinition"), tparams: []}), pos:core.Globals.null_pos}), pos: p};
			case [e = secureExpr()]:
				syntax.Reification.reifyExpr(e, syntax.Parser.in_macro);
		}
	}

	function parseFunction(p1:core.Globals.Pos, inl:Bool) : core.Ast.Expr {
		return switch stream {
			case [name = popt(dollarIdent), pl = parseConstraintParams(), {td:POpen}, al=psep(Comma, parseFunParam), {td:PClose}, t = popt(parseTypeHint)]:
				function make(e:core.Ast.Expr) : core.Ast.Expr {
					var f:core.Ast.Func = {
						f_params : pl,
						f_type : t,
						f_args : al,
						f_expr : Some(e)
					};

					var n = switch (name) {
						case None: None;
						case Some(v): Some((inl) ? "inline_"+v.pack : v.pack);
					};
					return {
						expr: EFunction(n,f),
						pos: punion(p1, e.pos)
					};
				};
				try {
					exprNext(make(secureExpr()));
				}
				catch (e:syntax.parser.Display) {
					syntax.Parser.display(e.expr);
				}
		}
	}

	function arrowExpr() : {fst:core.Ast.Expr, snd:Bool} {
		return switch stream {
			case [{td:Arrow}]:
				try {
					var e = expr();
					{fst:e, snd:false};
				}
				catch (err:syntax.parser.Display) {
					{fst:err.expr, snd:true};
				}
			case _: syntax.Parser.serror();
		}
	}

	function arrowFunction (p1:core.Globals.Pos, al:Array<core.Ast.FunArg>, er:{fst:core.Ast.Expr, snd:Bool}) : core.Ast.Expr {
		function make (e:core.Ast.Expr) : core.Ast.Expr {
			return {expr:EFunction(None, {
				f_params:[],
				f_type:None,
				f_args:al,
				f_expr:Some({expr:EReturn(Some(e)), pos:e.pos})
				}),
			pos:punion(p1,e.pos)};
		}
		return (er.snd) ? syntax.Parser.display(make(er.fst)) : make(er.fst);
	}

	function arrowIdentChecktype (e:core.Ast.Expr) : {fst:core.Ast.PlacedName, snd:Option<core.Ast.TypeHint>} {
		return switch (e) {
			case {expr:EConst(CIdent(n)),pos:p} : {fst:{pack:n, pos:p}, snd:None};
			case {expr:ECheckType({expr:EConst(CIdent(n)), pos:p},t)} : {fst:{pack:n,pos:p}, snd:Some(t)};
			case _: syntax.Parser.serror();
		}
	}

	function arrowFirstParam (e:core.Ast.Expr) : core.Ast.FunArg {
		return switch (e.expr) {
			case EConst(CIdent(n)):
				{
					name:{pack:n, pos:e.pos},
					opt:false,
					meta:[],
					type:None,
					value:None
				};
			case EBinop(OpAssign,e1,e2), EParenthesis({expr:EBinop(OpAssign,e1,e2)}):
				var ct = arrowIdentChecktype(e1);
				{
					name:ct.fst,
					opt:true,
					meta:[],
					type:ct.snd,
					value:Some(e2)
				};
			case EParenthesis(e):
				var ct = arrowIdentChecktype(e);
				{
					name:ct.fst,
					opt:false,
					meta:[],
					type:ct.snd,
					value:None
				};
			case _ :
				syntax.Parser.serror();
		}
	}

	public function expr() : core.Ast.Expr {
		return switch stream {
			case [meta = parseMetaEntry()]:
				try {
					makeMeta(meta.name, meta.params, secureExpr(), meta.pos);
				}
				catch (error:syntax.parser.Display) {
					syntax.Parser.display(makeMeta(meta.name, meta.params, error.expr, meta.pos));
				}
				catch (error:hxparse.ParserError) {
					if (core.Path.unique_full_path(meta.pos.pfile) == syntax.Parser.resume_display.get().pfile) {
						var e:core.Ast.Expr = {expr:EConst(CIdent("null")), pos:meta.pos};
						syntax.Parser.display(makeMeta(meta.name, meta.params, e, meta.pos));
					}
					throw error;
				}
			case [{td:BrOpen, pos:p1}]:
				if (syntax.Parser.is_resuming(p1)) {
					syntax.Parser.display({expr:EDisplay({expr:EObjectDecl([]), pos:p1}, false), pos:p1});
				}
				switch stream {
					case [{td:Binop(OpOr), pos:p2} && syntax.Parser.do_resume()]:
						syntax.Parser.set_resume(p1);
						syntax.Parser.display({expr:EDisplay({expr:EObjectDecl([]), pos:p1}, false), pos:p1});
					case [b = block1()]:
						var p2 = switch stream {
							case [{td:BrClose, pos:p2}]: p2;
							case _:
								// Ignore missing } if we are resuming and "guess" the last position.
								if (syntax.Parser.do_resume()) {
									nextToken().pos;
								}
								else {
									syntax.Parser.serror();
								}
						}
						var e = { expr: b, pos: punion(p1, p2)};
						switch(b) {
							case EObjectDecl(_): exprNext(e);
							case _: e;
						}
				}
			case [{td:Kwd(Macro), pos:p}]:
				parseMacroExpr(p);
			case [{td:Kwd(Var), pos: p1}, v = parseVarDecl()]: { expr: EVars([v]), pos: p1};
			case [{td:Const(c), pos:p}]: exprNext({expr:EConst(c), pos:p});
			case [{td:Kwd(This), pos:p}]: exprNext({expr: EConst(CIdent("this")), pos:p});
			case [{td:Kwd(True), pos:p}]: exprNext({expr: EConst(CIdent("true")), pos:p});
			case [{td:Kwd(False), pos:p}]: exprNext({expr: EConst(CIdent("false")), pos:p});
			case [{td:Kwd(Null), pos:p}]: exprNext({expr: EConst(CIdent("null")), pos:p});
			case [{td:Kwd(Cast), pos:p1}]:
				switch stream {
					case [{td:POpen, pos:pp}, e = expr()]:
						switch stream {
							case [{td:Comma}, t = parseComplexType(), {td:PClose, pos:p2}]: exprNext({expr:ECast(e,Some(t)), pos: punion(p1,p2)});
							case [t = parseTypeHint(), {td:PClose, pos:p2}]:
								var pu = punion(p1, p2);
								var ep:core.Ast.Expr = {expr: EParenthesis({expr: ECheckType(e, t), pos: pu}), pos: pu};
								exprNext({expr: ECast(ep, None), pos: punion(p1, pu)});
							case [{td:Const(CIdent("is")), pos:p_is}, t=parseTypePath(), {td:PClose, pos:p2} ]:
								var e_is = makeIs(e, t, punion(p1, p2), p_is);
								exprNext({expr:ECast(e_is, None), pos:punion(p1, e_is.pos)});
							case [{td:PClose, pos:p2}]:
								var ep = exprNext({expr: EParenthesis(e), pos: punion(pp, p2)});
								exprNext({expr:ECast(ep,None),pos:punion(p1,ep.pos)});
							case _: syntax.Parser.serror();
						}
					case [e = secureExpr()]: exprNext({expr:ECast(e,None), pos:punion(p1, e.pos)});
				}
			case [{td:Kwd(Throw), pos:p}, e = expr()]: { expr: EThrow(e), pos: p};
			case [{td:Kwd(New), pos:p1}, t = parseTypePath()]:
				switch stream {
					case [{td:POpen, pos:po}, e = parseCallParams(function(el, p2) {return {expr:ENew(t, el), pos:punion(p1,p2)}}, po)]:
						exprNext(e);
					case _:
						if (syntax.Parser.do_resume()) {
							{expr:ENew(t, []), pos:punion(p1, t.pos)};
						}
						else { syntax.Parser.serror(); }
				}
			case [{td:POpen, pos: p1}]:
				switch stream {
					case [{td:PClose, pos:p2}, er = arrowExpr()]:
						arrowFunction(p1, [], er);
					case [{td:Question, pos:p2}, al = psep(Comma, parseFunParam), {td:PClose}, er = arrowExpr()]:
						if (al.length == 0) { throw false; }
						al[0].opt = true;
						al[0].meta = [];
						arrowFunction(p1, al, er);
					case [e = expr()]:
						switch stream {
							case [{td:PClose, pos:p2}]: exprNext({expr:EParenthesis(e), pos:punion(p1,p2)});
							case [{td:Comma, pos:pc}, al = psep(Comma, parseFunParam), {td:PClose}, er = arrowExpr()]:
								arrowFunction(p1, apush(al, arrowFirstParam(e)), er);
							case [t = parseTypeHint()]:
								switch stream {
									case [{td:PClose, pos:p2}]:
										exprNext({expr:EParenthesis({expr:ECheckType(e, t), pos:punion(p1, p2)}), pos:punion(p1, p2)});
									case [{td:Comma, pos:pc}, al = psep(Comma, parseFunParam), {td:PClose}, er = arrowExpr()]:
										var ct = arrowIdentChecktype(e);
										arrowFunction(p1, apush(al,{name:ct.fst, opt:false, meta:[], type:Some(t), value:None}), er);
									case [{td:Binop(OpAssign), pos:p2}, ea1 = expr()]:
										function with_args(al:Array<core.Ast.FunArg>, er) {
											return switch (e.expr) {
												case EConst(CIdent(n)):
													al.unshift({
														name:{pack:n, pos:e.pos},
														opt:true,
														meta:[],
														type:Some(t),
														value:Some(ea1)
													});
													arrowFunction(p1,
														al,
														er);
												case _: syntax.Parser.serror();
											}
										}
										switch stream {
											case [{td:PClose, pos:p2}, er = arrowExpr()]:
												with_args([], er);
											case [{td:Comma, pos:pc}, al = psep(Comma, parseFunParam), {td:PClose}, er = arrowExpr()]:
												with_args(al, er);
											case _: syntax.Parser.serror();
										}
									case _: syntax.Parser.serror();
								}
							case [{td:Const(CIdent("is")), pos:p_is}, t = parseTypePath(), {td:PClose, pos:p2}]:
								exprNext(makeIs(e, t, punion(p1, p2), p_is));
							case _: syntax.Parser.serror();
						}
				}
			case [{td:BkOpen, pos:p1}, l = parseArrayDecl(), {td:BkClose, pos:p2}]: exprNext({expr: EArrayDecl(l), pos:punion(p1,p2)});
			case [{td:Kwd(Function), pos:p1}, e = parseFunction(p1, false)]: e;
			case [{td:Unop(op), pos:p1} && isPrefix(op), e = expr()]: makeUnop(op,e,p1);
			case [{td:Binop(OpSub), pos:p1}, e = expr()]:
				// makeUnop(OpNeg, e, p1);
				function neg(s:String) {
					return s.charCodeAt(0) == '-'.code
						? s.substr(1)
						: "-" + s;
				}
				switch (makeUnop(OpNeg,e,p1)) {
					case {expr:EUnop(OpNeg,Prefix,{expr:EConst(CInt(i))}), pos:p}:
						{expr:EConst(CInt(neg(i))), pos:p};
					case {expr:EUnop(OpNeg,Prefix,{expr:EConst(CFloat(j))}), pos:p}:
						{expr:EConst(CFloat(neg(j))), pos:p};
					case e: e;
				}
			case [{td:Kwd(For), pos:p}, {td:POpen}, it = expr(), {td:PClose}]:
				try {
					var e = secureExpr();
					{ expr: EFor(it,e), pos:punion(p, e.pos)};
				}
				catch (e:syntax.parser.Display) {
					syntax.Parser.display({ expr: EFor(it,e.expr), pos:punion(p, e.expr.pos)});
				}
			case [{td:Kwd(If), pos:p}, {td:POpen}, cond = expr(), {td:PClose}, e1 = expr()]:
				var e2 = switch stream {
					case [{td:Kwd(Else)}, e2 = expr()]: Some(e2);
					case _:
						switch npeek(2) {
							case [{td:Semicolon}, {td:Kwd(Else)}]:
								junk();
								junk();
								Some(secureExpr());
							case _: None;
						}
				}
				var p2 = switch (e2) {
					case None: e1.pos;
					case Some(e): e.pos;
				}
				{ expr: EIf(cond,e1,e2), pos:punion(p, p2)};
			case [{td:Kwd(Return), pos:p}, e = popt(expr)]:
				var p1 = switch (e) {
					case None: p;
					case Some(v): punion(p, v.pos);
				}
				{ expr: EReturn(e), pos: p1};
			case [{td:Kwd(Break), pos:p}]: { expr: EBreak, pos: p };
			case [{td:Kwd(Continue), pos:p}]: { expr: EContinue, pos: p};
			case [{td:Kwd(While), pos:p1}, {td:POpen}, cond = expr(), {td:PClose}]:
				try {
					var e = secureExpr();
					{ expr: EWhile(cond, e, NormalWhile), pos: punion(p1, e.pos)};
				}
				catch (e:syntax.parser.Display) {
					syntax.Parser.display({ expr: EWhile(cond, e.expr, NormalWhile), pos: punion(p1, e.expr.pos)});
				}
			case [{td:Kwd(Do), pos:p1}, e = expr(), {td:Kwd(While)}, {td:POpen}, cond = expr(), {td:PClose}]: { expr: EWhile(cond,e,DoWhile), pos:punion(p1, e.pos)};
			case [{td:Kwd(Switch), pos:p1}, e = expr(), {td:BrOpen}, cases = parseSwitchCases(e), {td:BrClose, pos:p2}]:
				{ expr: ESwitch(e,cases.cases,cases.def), pos:punion(p1,p2)};
			case [{td:Kwd(Try), pos:p1}, e = expr(), cl = parseCatches(e, [], e.pos)]:
				{ expr: ETry(e,cl.catches), pos:punion(p1, cl.pos)};
			case [{td:IntInterval(i), pos:p1}, e2 = expr()]: makeBinop(OpInterval,{expr:EConst(CInt(i)), pos:p1}, e2);
			case [{td:Kwd(Untyped), pos:p1}, e = expr()]: { expr: EUntyped(e), pos:punion(p1,e.pos)};
			case [{td:Dollar(v), pos:p}]: exprNext({expr:EConst(CIdent("$" + v)), pos:p});
		}
	}

	function exprNext(e1:core.Ast.Expr):core.Ast.Expr {
		return switch stream {
			case [{td:BrOpen, pos:p1} && isDollarIdent(e1), eparam = expr(), {td:BrClose,pos:p2}]:
				switch (e1.expr) {
					case EConst(CIdent(n)):
						exprNext({expr: EMeta({name:core.Meta.string_to_meta(n), params:[], pos:e1.pos},eparam), pos:punion(p1,p2)});
					case _: throw false;
				}
			case [{td:Dot, pos:p}]:
				if (syntax.Parser.is_resuming(p)) {
					syntax.Parser.display({expr:EDisplay(e1, false), pos:p});
				}
				switch stream {
					case [{td:Kwd(Macro), pos:p2} && p.pmax == p2.pmin]:
						exprNext({expr:EField(e1,"macro"), pos:punion(e1.pos,p2)});
					case [{td:Kwd(Extern), pos:p2} && p.pmax == p2.pmin]:
						exprNext({expr:EField(e1,"extern"), pos:punion(e1.pos,p2)});
					case [{td:Kwd(New), pos:p2} && p.pmax == p2.pmin]:
						exprNext({expr:EField(e1,"new"), pos:punion(e1.pos,p2)});
					case [{td:Const(CIdent(f)), pos:p2} && p.pmax == p2.pmin]:
						exprNext({expr:EField(e1,f), pos:punion(e1.pos,p2)});
					case [{td:Dollar(v), pos:p2}]:
						exprNext({expr:EField(e1, "$" + v), pos:punion(e1.pos, p2)});
					case [{td:Binop(OpOr), pos:p2} && syntax.Parser.do_resume()]:
						syntax.Parser.set_resume(p);
						syntax.Parser.display({expr:EDisplay(e1, false), pos:p}); // help for debug display mode
					case _:
						// turn an integer followed by a dot into a float
						switch(e1) {
							case {expr: EConst(CInt(v)), pos:p2} if (p2.pmax == p.pmin):
								exprNext({expr:EConst(CFloat(v + ".")), pos:punion(p,p2)});
							case _: syntax.Parser.serror();
						}
				}
			case [{td:POpen, pos:p1}, e = parseCallParams(function(el, p2) {return {expr:ECall(e1, el), pos:punion(e1.pos, p2)};}, p1)]:
				exprNext(e);
			case [{td:BkOpen}, e2 = expr(), {td:BkClose, pos:p2}]:
				exprNext({expr:EArray(e1,e2), pos:punion(e1.pos,p2)});
			case [{td:Arrow, pos:pa}]:
				var er = try {
					var e = expr();
					{fst:e, snd:false};
				}
				catch (err:syntax.parser.Display) {
					{fst:err.expr, snd:true};
				}
				arrowFunction(e1.pos, [arrowFirstParam(e1)], er);
			case [{td:Binop(OpGt), pos:p1}]:
				switch stream {
					case [{td:Binop(OpGt), pos:p2} && p1.pmax == p2.pmin]:
						switch stream {
							case [{td:Binop(OpGt), pos:p3} && p2.pmax == p3.pmin]:
								switch stream {
									case [{td:Binop(OpAssign), pos:p4} && p3.pmax == p4.pmin, e2 = expr()]:
										makeBinop(OpAssignOp(OpUShr),e1,e2);
									case [e2 = secureExpr()]:
										makeBinop(OpUShr,e1,e2);
								}
							case [{td:Binop(OpAssign), pos:p3} && p2.pmax == p3.pmin, e2 = expr()]:
								makeBinop(OpAssignOp(OpShr),e1,e2);
							case [e2 = secureExpr()]:
								makeBinop(OpShr,e1,e2);
						}
					case [{td:Binop(OpAssign), pos:p2} && p1.pmax == p2.pmin]:
						makeBinop(OpGte,e1,secureExpr());
					case [e2 = secureExpr()]:
						makeBinop(OpGt,e1,e2);
				}
			case [{td:Binop(op)}]:
				try {
					switch stream {
						case [e2 = expr()]: makeBinop(op,e1,e2);
						case _: syntax.Parser.serror();
					}
				}
				catch (err:syntax.parser.Display) {
					throw new syntax.parser.Display(makeBinop(op, e1, err.expr));
				}
			case [{td:Unop(op), pos:p} && isPostfix(e1,op)]:
				exprNext({expr:EUnop(op,Postfix,e1), pos:punion(e1.pos, p)});
			case [{td:Question}, e2 = expr(), {td:DblDot}, e3 = expr()]:
				{ expr: ETernary(e1,e2,e3), pos: punion(e1.pos, e3.pos)};
			case [{td:Kwd(In)}, e2 = expr()]:
				makeBinop(OpIn, e1, e2);
			case _: e1;
		}
	}

	function parseGuard() : core.Ast.Expr {
		return switch stream {
			case [{td:Kwd(If)}, {td:POpen}, e = expr(), {td:PClose}]:
				e;
		}
	}

	function parseExprOrVar() :core.Ast.Expr {
		return switch stream {
			case [{td:Kwd(Var),pos:p1}, name = dollarIdent()]:
				{ expr: EVars([{name: name, type:None, expr:None}]), pos: punion(p1, name.pos) };
			case [e = expr()]: e;
		}
	}

	function parseSwitchCases(eswitch:core.Ast.Expr) : {cases:Array<core.Ast.Case>, def:Option<{e:Option<core.Ast.Expr>, pos:core.Globals.Pos}>} {
		var cases = [];
		var def = None;
		function caseBlock(b:Array<core.Ast.Expr>, p1:core.Globals.Pos, p2:core.Globals.Pos) : {e:Option<core.Ast.Expr>, pos:core.Globals.Pos} {
			return if (b.length == 0) {
				{e:None, pos:p1};
			} else {
				var p = punion(p1, p2);
				switch(b) {
					case [e = macro $b{el}]: {e:Some(e), pos:p};
					case _: {e:Some({ expr: EBlock(b), pos: p}), pos:p};
				}
			}
		}
		while(true) {
			switch stream {
				case [{td:Kwd(Default), pos:p1}, {td:DblDot}]:
					var bp = try {
						blockWithPos([], p1);
					}
					catch (err:syntax.parser.Display) {
						syntax.Parser.display({expr:ESwitch(eswitch, cases, Some({e:Some(err.expr), pos:punion(p1,err.expr.pos)})), pos:punion(eswitch.pos, err.expr.pos)});
					}
					var e = caseBlock(bp.acc, p1, bp.pos);
					if (def != None) {
						syntax.Parser.error(Duplicate_default, p1);
					}
					def = Some(e);
				case [{td:Kwd(Case), pos:p1}, el = psep(Comma,parseExprOrVar), eg = popt(parseGuard), {td:DblDot}]:
					if (el.length == 0) {
						syntax.Parser.error(Custom("case without a pattern is not allowed"), p1);
					}
					var bp = try {
						blockWithPos([], p1);
					}
					catch (err:syntax.parser.Display) {
						syntax.Parser.display({expr:ESwitch(eswitch, apush(cases, {values:el,guard:eg,expr:Some(err.expr), pos:punion(p1,err.expr.pos)}), None), pos:punion(eswitch.pos, err.expr.pos)});
					}
					var p2 = switch (eg) {
						case Some(e): e.pos;
						case None:
							if (el.length > 0) {
								el[0].pos;
							}
							else {
								p1;
							}
					}
					var e = caseBlock(bp.acc, p1, p2);
					cases.push({values:el,guard:eg,expr:e.e, pos:e.pos});
				case _:
					break;
			}
		}
		return {
			cases: cases,
			def: def
		}
	}

	function parseCatch(etry:core.Ast.Expr) : {fst:core.Ast.Catch, snd:core.Globals.Pos} {
		return switch stream {
			case [{td:Kwd(Catch), pos:p}, {td:POpen}, id = dollarIdent(), ]:
				switch stream {
					case [t = parseTypeHint(), {td:PClose}]:
						try {
							var e = secureExpr();
							{fst:{
								name:id,
								type:t,
								expr:e,
								pos:punion(p, e.pos)
							}, snd:e.pos};
						}
						catch (err:syntax.parser.Display) {
							syntax.Parser.display({
								expr:ETry(etry, [{name:id, type:t, expr:err.expr, pos:err.expr.pos}]),
								pos:punion(etry.pos, err.expr.pos)
							});
						}
					case [{pos:p}]:
						syntax.Parser.error(Missing_type,p);
				}
		}
	}

	function parseCatches(etry:core.Ast.Expr, catches:Array<core.Ast.Catch>, pmax:core.Globals.Pos) : {catches:Array<core.Ast.Catch>, pos:core.Globals.Pos} {
		return switch stream {
			case [c = parseCatch(etry)]:
				parseCatches(etry, aunshift(catches, c.fst), c.snd);
			case _:
				catches.reverse();
				{catches:catches, pos:pmax};
		}
	}

	function parseCallParams(f:Array<core.Ast.Expr>->core.Globals.Pos->core.Ast.Expr, p1:core.Globals.Pos) {
		function makeDisplayCall(el:Array<core.Ast.Expr>, p2:core.Globals.Pos) : Dynamic {
			var e = f(el, p2);
			syntax.Parser.display({expr:EDisplay(e, true), pos:e.pos});
		}
		if (syntax.Parser.is_resuming(p1)) {
			makeDisplayCall([], p1);
		}
		function parseNextParam(acc:Array<core.Ast.Expr>, p1:core.Globals.Pos) {
			function catched(exc:Dynamic) : Dynamic {
				var p2 = nextToken().pos;
				if (syntax.Parser.encloses_resume(punion(p1, p2))) {
					makeDisplayCall(acc, p2);
				}
				throw exc;
			}
			var e = try {
				expr();
			}
			catch (exc:hxparse.ParserError) {
				catched(exc);
			}
			catch (exc:ocaml.Error) {
				catched(exc);
			}
			catch (exc:ocaml.stream.Failure) {
				catched(exc);
			}
			catch (e:syntax.parser.Display) {
				syntax.Parser.display(f(apush(acc, e.expr), e.expr.pos));
			}
			return switch stream {
				case [{td:PClose, pos:p2}]: f(apush(acc,e), p2);
				case [{td:Comma, pos:p2}]: parseNextParam(apush(acc, e), p2);
				case [{td:Semicolon, pos:p2}]:
					if (syntax.Parser.encloses_resume(punion(p1, p2))) {
						makeDisplayCall(acc,p2);
					}
					else {
						syntax.Parser.serror();
					}
				case _:
					var p2 = nextToken().pos;
					if (syntax.Parser.encloses_resume(punion(p1, p2))) {
						makeDisplayCall(apush(acc, e),p2);
					}
					else {
						syntax.Parser.serror();
					}
			}
		}

		return switch stream {
			case [{td:PClose, pos:p2}]: f([], p2);
			case [{td:Binop(OpOr), pos:p2} && syntax.Parser.do_resume()]:
				syntax.Parser.set_resume(p1);
				makeDisplayCall([], p2);
			case _: parseNextParam([], p1);
		}
	}

	function toplevelExpr():core.Ast.Expr {
		return try {
			expr();
		}
		catch (err:syntax.parser.Display) {
			err.expr;
		}
	}

	function secureExpr() : core.Ast.Expr {
		return switch stream {
			case [e = expr()] : e;
			case _: syntax.Parser.serror();
		}
	}

	function makeIs (e:core.Ast.Expr, pt:core.Ast.PlacedTypePath, p:core.Globals.Pos, p_is:core.Globals.Pos) : core.Ast.Expr {
		var e_is:core.Ast.Expr = {expr:EField({expr:EConst(CIdent("Std")), pos:core.Globals.null_pos},"is"), pos:p_is};
		var e2 = core.Ast.expr_of_type_path(pt.tp, pt.pos);
		return {expr:ECall(e_is, [e,e2]),pos:p};
	}

	function nextToken(): syntax.Lexer.Token {
		var tk = peek(0);
		return (tk == null) ? syntax.Parser.last_token(this) : tk;
	}
}