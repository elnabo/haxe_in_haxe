package syntax;

import haxe.ds.Option;

import core.Ast.ExprDef;
import core.Globals.Pos;
import syntax.Lexer.Token;

// typedef Position = {file:String, min:Int, max:Int};

// inspired by https://github.com/Simn/haxeparser/blob/master/src/haxeparser/HaxeParser.hx
// class CondParser extends hxparse.Parser<hxparse.LexerTokenSource<Token>, Token> implements hxparse.ParserBuilder {
// 	public function new(stream){
// 		super(stream);
// 	}

// 	public function parseMacroCond(allowOp:Bool):{tk:Option<Token>, expr:core.Ast.Expr}
// 	{
// 		return switch stream {
// 			case [{td:Const(CIdent(t)), pos:p}]:
// 				parseMacroIdent(allowOp, t, p);
// 			case [{td:Const(CString(s)), pos:p}]:
// 				{td:None, expr:{expr:EConst(CString(s)), pos:Pos.of(p)}};
// 			case [{tok:Const(CInt(s)), pos:p}]:
// 				{td:None, expr:{expr:EConst(CInt(s)), pos:Pos.of(p)}};
// 			case [{tok:Const(CFloat(s)), pos:p}]:
// 				{td:None, expr:{expr:EConst(CFloat(s)), pos:Pos.of(p)}};
// 			case [{tok:Kwd(k), pos:p}]:
// 				parseMacroIdent(allowOp, HaxeParser.keywordString(k), p);
// 			case [{td:POpen, pos:p1}, o = parseMacroCond(true), {tok:PClose, pos:p2}]:
// 				var e = {expr:EParenthesis(o.expr), pos:HaxeParser.punion(p1, p2)};
// 				if (allowOp) parseMacroOp(e) else { tk:None, expr:e };
// 			case [{td:Unop(op), pos:p}, o = parseMacroCond(allowOp)]:
// 				{tk:o.tk, expr:HaxeParser.makeUnop(op, o.expr, p)};
// 		}
// 	}

// 	function parseMacroIdent(allowOp:Bool, t:String, p:Position):{tk:Option<Token>, expr:core.Ast.Expr}
// 	{
// 		var e = {expr:EConst(CIdent(t)), pos:Pos.of(p)};
// 		return if (!allowOp) { tk:None, expr:e } else parseMacroOp(e);
// 	}

// 	function parseMacroOp(e:core.Ast.Expr):{tk:Option<Token>, expr:core.Ast.Expr}
// 	{
// 		return switch peek(0) {
// 			case {td:Binop(op)}:
// 				junk();
// 				op = switch peek(0) {
// 					case {td:Binop(OpAssign)} if (op == OpGt):
// 						junk();
// 						OpGte;
// 					case _: op;
// 				}
// 				var o = parseMacroCond(true);
// 				{tk:o.tk, expr:HaxeParser.makeBinop(op, e, o.expr)};
// 			case tk:
// 				{tk:Some(tk), expr:e};
// 		}
// 	}
// }

// class Grammar {
// }