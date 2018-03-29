package neko;

import haxe.ds.ImmutableList;
import haxe.ds.Option;

typedef Pos = {
	psource:String,
	pline: Int
}

enum Constant {
	True;
	False;
	Null;
	This;
	Int(i:Int);
	Float(s:String);
	String(s:String);
	Builtin(s:String);
	Ident(s:String);
	Int32(i:haxe.Int32);
}

enum WhileFlag {
	NormalWhile;
	DoWhile;
}

typedef EObjectElement = {s:String, e:Expr};

enum ExprDecl {
	EConst(c:Constant);
	EBlock(l:ImmutableList<Expr>);
	EParenthesis(e:Expr);
	EField(e:Expr, s:String);
	ECall(e:Expr, el:ImmutableList<Expr>);
	EArray(e1:Expr, e2:Expr);
	EVars(vl:ImmutableList<{name:String, def:Option<Expr>}>);
	EWhile(e1:Expr, e2:Expr, flag:WhileFlag);
	EIf(cond:Expr, ethen:Expr, eelse:Option<Expr>);
	ETry(e1:Expr, s:String, e2:Expr);
	EFunction(params:ImmutableList<String>, body:Expr);
	EBinop(op:String, e1:Expr, e2:Expr);
	EReturn(eo:Option<Expr>);
	EBreak(eo:Option<Expr>);
	EContinue;
	ENext(e1:Expr, e2:Expr);
	EObject(ol:ImmutableList<EObjectElement>);
	ELabel(s:String);
	ESwitch(e:Expr, cases:ImmutableList<{e1:Expr, e2:Expr}>, guard:Option<Expr>);
	ENeko(s:String);
}

typedef Expr = {
	decl:ExprDecl,
	pos:Pos
}

class Nast {
	public static inline function pos (e:Expr) : Pos { return e.pos; }
	public static final null_pos:Pos = {pline:0, psource:"<null pos>"};

}