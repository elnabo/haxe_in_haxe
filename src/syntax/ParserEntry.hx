package syntax;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
import ocaml.Hashtbl;
using equals.Equal;

// eval
enum Small_type {
	TNull;
	TBool(b:Bool);
	TFloat(f:Float);
	TString(s:String);
}

class ParserEntry {

	public static inline function is_true(a:Small_type) {
		return haxeparser.HaxeParser.HaxeTokenSource.isTrue(a);
	}

	public static inline function eval(ctx:core.Define, e:core.Ast.Expr) : Small_type {
		return haxeparser.HaxeParser.HaxeTokenSource.eval(ctx, e);
	}

	public static function parse (ctx:core.Define, code:{fst:byte.ByteData, snd:String})  : {pack:ImmutableList<String>, decls:ImmutableList<core.Ast.TypeDecl>} {
		return new haxeparser.HaxeParser(code.fst, code.snd, ctx).parse();
	}

	public static function parse_string (com:core.Define, s:String, p:core.Globals.Pos, error:(String, core.Globals.Pos)->Dynamic, inlined:Bool) : {pack:ImmutableList<String>, decls:ImmutableList<core.Ast.TypeDecl>} {
		var old = syntax.Lexer.save();
		var old_file = try { Some(Hashtbl.find(syntax.Lexer.all_files, p.pfile)); }
			catch (_:ocaml.Not_found) { None; }
		var old_display = syntax.Parser.resume_display.get();
		var old_de = syntax.Parser.display_error;
		function restore () {
			switch (old_file) {
				case None:
				case Some(f): Hashtbl.replace(syntax.Lexer.all_files, p.pfile, f);
			}
			if (!inlined) { syntax.Parser.resume_display.set(old_display); }
			syntax.Lexer.restore(old);
			syntax.Parser.display_error = old_de;
		}
		syntax.Lexer.init(p.pfile, true);
		syntax.Parser.display_error = function (e:syntax.parser.ErrorMsg, p:core.Globals.Pos) { throw new syntax.parser.Error(e, p); }
		if (!inlined) { syntax.Parser.resume_display.set(core.Globals.null_pos); }
		var _tmp = try {
			parse(com, {fst:byte.ByteData.ofString(s), snd:p.pfile});
		}
		catch (err:syntax.parser.Error) {
			var e = err.error_msg; var pe = err.pos;
			restore();
			core.Error.error(syntax.Parser.error_msg(e), (inlined) ? pe : p);
		}
		catch (err:syntax.lexer.Error) {
			var e = err.error_msg; var pe = err.pos;
			restore();
			core.Error.error(syntax.Lexer.error_msg(e), (inlined) ? pe : p);
		}
		restore();
		return _tmp;
	}

	public static function parse_expr_string(com:core.Define, s:String, p:core.Globals.Pos, error:(String, core.Globals.Pos)->Dynamic, inl:Bool) : core.Ast.Expr {
		var head = "class X{static function main() ";
		var head = (p.pmin > head.length) ? head + StringTools.lpad("", " ", p.pmin + head.length) : head;
		function loop (e:core.Ast.Expr) : core.Ast.Expr {
			var e = core.Ast.map_expr(loop, e);
			return {expr:e.expr, pos:p};
		}
		return
		switch (parse_string(com, head+s+";}", p, error, inl)) {
			case {decls:[{decl:EClass({d_data:[{cff_name:{pack:"main", pos:null_pos}, cff_kind:FFun({f_expr:Some(e)})}]})}]}: // capturing in null_pos why ?
				(inl) ? e : loop(e);
			case _:
				throw ocaml.Exit.instance;
		}
	}

	// public static function cmp(a:Small_type, b:Small_type) {
	// 	return switch [a, b] {
	// 		case [TNull, TNull]: 0;
	// 		case [TFloat(a), TFloat(b)]: Reflect.compare(a, b);
	// 		case [TString(a), TString(b)]: Reflect.compare(a, b);
	// 		case [TBool(a), TBool(b)]: Reflect.compare(a, b);
	// 		case [TString(a), TFloat(b)]: Reflect.compare(Std.parseFloat(a), b);
	// 		case [TFloat(a), TString(b)]: Reflect.compare(a, Std.parseFloat(b));
	// 		case _: throw ocaml.Exit.instance; // alway false
	// 	}
	// }

	// public static function eval(ctx:core.Define, e:core.Ast.Expr) : Small_type {
	// 	return switch (e.expr) {
	// 		case EConst(CIdent(i)):
	// 			try {
	// 				TString(core.Define.raw_defined_value(ctx, i));
	// 			}
	// 			catch (_:ocaml.Not_found) {
	// 				TNull;
	// 			}
	// 		case EConst(CString(s)): TString(s);
	// 		case EConst(CInt(n)), EConst(CFloat(n)): TFloat(Std.parseFloat(n));
	// 		case EBinop(OpBoolAnd, e1, e2): TBool(isTrue(eval(ctx, e1)) && isTrue(eval(ctx, e2)));
	// 		case EBinop(OpBoolOr, e1, e2): TBool(isTrue(eval(ctx, e1)) || isTrue(eval(ctx, e2)));
	// 		case EUnop(OpNot, _, e): TBool(!isTrue(eval(ctx, e)));
	// 		case EParenthesis(e): eval(ctx, e);
	// 		case EBinop(op, e1, e2):
	// 			var v1 = eval(ctx, e1);
	// 			var v2 = eval(ctx, e2);
	// 			function compare (op:Int->Int->Bool) :Small_type {
	// 				return try {
	// 					return TBool(op(cmp(v1, v2), 0));
	// 				}
	// 				catch (_:Dynamic) {
	// 					return TBool(false);
	// 				}
	// 			}
	// 			switch (op) {
	// 				case OpEq: compare(function (a, b) { return a == b;});
	// 				case OpNotEq: compare(function (a, b) { return a != b;});
	// 				case OpGt: compare(function (a, b) { return a > b;});
	// 				case OpGte: compare(function (a, b) { return a >= b;});
	// 				case OpLt: compare(function (a, b) { return a < b;});
	// 				case OpLte: compare(function (a, b) { return a <= b;});
	// 				case _: syntax.Parser.error(Custom("Unsupported operation"), e.pos);
	// 			}
	// 		case _: syntax.Parser.error(Custom("Invalid condition expression"), e.pos);
	// 	}
	// }

	// parse main
	// public static function parse (ctx:core.Define, code:haxeparser.HaxeLexer) {
	// 	trace("TODO: syntax.ParserEntry.parse");
	// 	var old = Lexer.save();
	// 	var restore_cache = syntax.parser.TokenCache.clear();
	// 	var mstack = [];
	// 	syntax.Parser.last_doc = None;
	// 	syntax.Parser.in_macro = core.Define.defined(ctx, Macro);
	// 	try {
	// 		code.token(syntax.Lexer.skip_header);
	// 	}catch (e:hxparse.ParserError) {
	// 	}

	// 	var sraw = function (_:Int) { return Some(code.token(syntax.Lexer.token)); };
	// 	var process_token:syntax.Lexer.Token -> syntax.Lexer.Token;
	// 	var skip_tokens:{file:String, min:Int, max:Int}->Bool->syntax.Lexer.Token;
	// 	var enter_macro:{file:String, min:Int, max:Int}->syntax.Lexer.Token;

	// 	function next_token () {
	// 		return process_token(code.token(syntax.Lexer.token));
	// 	}

	// 	process_token = function (tk:syntax.Lexer.Token) {
	// 		return switch (tk.td) {
	// 			case Comment(s):
	// 				var ntk = next_token();
	// 				if (syntax.Parser.use_doc) {
	// 					var l = s.length;
	// 					if (l > 0 && s.charAt(0) == "*") {
	// 						syntax.Parser.last_doc = Some({
	// 							a:s.substr(1, (l - ((l > 1 && s.charAt(l-1) == "*") ? 2 : 1))),
	// 							b:tk.pos.min
	// 						});
	// 					}
	// 				}
	// 				ntk;
	// 			case CommentLine(s): next_token();
	// 			case Sharp("end"):
	// 				if (mstack.length == 0) {
	// 					tk;
	// 				}
	// 				else {
	// 					mstack.shift();
	// 					next_token();
	// 				}
	// 			case Sharp("else"), Sharp("elseif"):
	// 				if (mstack.length == 0) {
	// 					tk;
	// 				}
	// 				else {
	// 					mstack.shift();
	// 					process_token(skip_tokens(tk.pos, false));
	// 				}
	// 			case Sharp("if"):
	// 				process_token(enter_macro(tk.pos));
	// 			case Sharp("error"):
	// 				var ntk = code.token(syntax.Lexer.token);
	// 				switch (ntk.td) {
	// 					case Const(CInt(s)): throw syntax.parser.Error.of(Custom(s), ntk.pos);
	// 					default: throw syntax.parser.Error.of(Unimplemented,tk.pos);
	// 				}
	// 			case Sharp("line"):
	// 				var ntk = next_token();
	// 				var line:Int = switch (ntk.td) {
	// 					case Const(CInt(s)):
	// 						var i = Std.parseInt(s);
	// 						if (i == null) {
	// 							throw syntax.parser.Error.of(Custom("Could not parse ridiculous line number "+s), ntk.pos);
	// 						}
	// 						else {
	// 							i;
	// 						}
	// 					default: throw syntax.parser.Error.of(Unexpected(ntk.td), ntk.pos);
	// 				}
	// 				syntax.Lexer.cur.lline = line - 1;
	// 				next_token();
	// 			default:
	// 				tk;
	// 		}
	// 	};

	// 	enter_macro = function (p) {
	// 		return null;
	// 	};
	// 	return null;
	// }
}