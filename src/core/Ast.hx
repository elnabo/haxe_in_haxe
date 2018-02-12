package core;

import core.Globals.Pos;
import core.Meta.StrictMeta;
import haxe.ds.ImmutableList;
import haxe.ds.Option;
import ocaml.List;

using equals.Equal;

enum Keyword {
	Function;
	Class;
	Var;
	If;
	Else;
	While;
	Do;
	For;
	Break;
	Continue;
	Return;
	Extends;
	Implements;
	Import;
	Switch;
	Case;
	Default;
	Static;
	Public;
	Private;
	Try;
	Catch;
	New;
	This;
	Throw;
	Extern;
	Enum;
	In;
	Interface;
	Untyped;
	Cast;
	Override;
	Typedef;
	Dynamic;
	Package;
	Inline;
	Using;
	Null;
	True;
	False;
	Abstract;
	Macro;
	Final;
}

enum Binop {
	OpAdd;
	OpMult;
	OpDiv;
	OpSub;
	OpAssign;
	OpEq;
	OpNotEq;
	OpGt;
	OpGte;
	OpLt;
	OpLte;
	OpAnd;
	OpOr;
	OpXor;
	OpBoolAnd;
	OpBoolOr;
	OpShl;
	OpShr;
	OpUShr;
	OpMod;
	OpAssignOp (b:Binop);
	OpInterval;
	OpArrow;
	OpIn;
}
enum Unop {
	OpIncrement;
	OpDecrement;
	OpNot;
	OpNeg;
	OpNegBits;
}

enum Constant {
	CInt (s:String);
	CFloat (s:String);
	CString (s:String);
	CIdent (s:String);
	CRegexp (s1:String, s2:String);
}

enum Token {
	Eof;
	Const (c:Constant);
	Kwd (k:Keyword);
	Comment (s:String);
	CommentLine (s:String);
	Binop (b:Binop);
	Unop (u:Unop);
	Semicolon;
	Comma;
	BrOpen;
	BrClose;
	BkOpen;
	BkClose;
	POpen;
	PClose;
	Dot;
	DblDot;
	Arrow;
	IntInterval (s:String);
	Sharp (s:String);
	Question;
	At;
	Dollar (s:String);
}

enum UnopFlag {
	Prefix;
	Postfix;
}

enum WhileFlag {
	NormalWhile;
	DoWhile;
}

enum QuoteStatus {
	NoQuotes;
	DoubleQuotes;
}

typedef TypePath = {
	tpackage : ImmutableList<String>,
	tname : String,
	tparams : ImmutableList<TypeParamOrConst>,
	tsub : Option<String>
}

typedef PlacedTypePath = {
	tp : TypePath,
	pos: Pos
} 

enum TypeParamOrConst {
	TPType (hint:TypeHint);
	TPExpr (e:Expr);
}

enum ComplexType {
	CTPath (tp:TypePath);
	CTFunction (l:ImmutableList<TypeHint>, th:TypeHint);
	CTAnonymous (cf:ImmutableList<ClassField>);
	CTParent (th:TypeHint);
	CTExtend (l:ImmutableList<PlacedTypePath>, cf:ImmutableList<ClassField>);
	CTOptional (th:TypeHint);
	CTNamed (pn:PlacedName, th:TypeHint);
}

typedef TypeHint = {
	 ct:ComplexType,
	 pos:Pos
}

typedef FunArg = {
	name:PlacedName,
	opt:Bool,
	meta:Metadata,
	type:Option<TypeHint>,
	value:Option<Expr>
};
typedef Func = {
	f_params : ImmutableList<TypeParam>,
	f_args : ImmutableList<FunArg>,
	f_type : Option<TypeHint>,
	f_expr : Option<Expr>
}

typedef PlacedName = {
	pack:String,
	pos:Pos
}

typedef ObjectField = {
	name:String,
	pos:Globals.Pos,
	quotes:Ast.QuoteStatus,
	expr:Expr
}

typedef Var = {
	name:PlacedName,
	type:Option<TypeHint>,
	expr:Option<Expr>
};
typedef Case = {values:ImmutableList<Expr>, guard:Option<Expr>, expr:Option<Expr>, pos:Pos};
typedef Catch = {name:PlacedName, type:TypeHint, expr:Expr, pos:Pos};
enum ExprDef {
	EConst (c:Constant);
	EArray (e1:Expr, e2:Expr);
	EBinop (op:Binop, e1:Expr, e2:Expr);
	EField (e:Expr, s:String);
	EParenthesis (e:Expr);
	EObjectDecl (fields:ImmutableList<ObjectField>);
	EArrayDecl (values:ImmutableList<Expr>);
	ECall (e:Expr, params:ImmutableList<Expr>);
	ENew (ptp:PlacedTypePath, exprs:ImmutableList<Expr>);
	EUnop (op:Unop, flag:UnopFlag, e:Expr);
	// EVars of (placed_name * type_hint option * expr option) list
	EVars (vars:ImmutableList<Var>);
	EFunction (s:Option<String>, f:Func);
	EBlock (exprs:ImmutableList<Expr>);
	EFor (e1:Expr, e2:Expr);
	EIf (econd:Expr, eif:Expr, eelse:Option<Expr>);
	EWhile (econd:Expr, e:Expr, flag:WhileFlag);
	//ocaml type: ESwitch of expr * (expr list * expr option * expr option * pos) list * (expr option * pos) option
	ESwitch (e:Expr, cases:ImmutableList<Case>, edef:Option<{e:Option<Expr>, pos:Pos}>);
	// ETry of expr * (placed_name * type_hint * expr * pos) list
	ETry (e:Expr, catches:ImmutableList<Catch>);
	EReturn (e:Option<Expr>);
	EBreak;
	EContinue;
	EUntyped (e:Expr);
	EThrow (e:Expr);
	ECast (e:Expr, th:Option<TypeHint>);
	EDisplay (e:Expr, b:Bool);
	EDisplayNew (ptp:PlacedTypePath);
	ETernary (e1:Expr, e2:Expr, e3:Expr);
	ECheckType (e:Expr, th:TypeHint);
	EMeta (me:MetadataEntry, e:Expr);
}

typedef Expr = {
	expr : ExprDef,
	pos : core.Globals.Pos
}

typedef TypeParam = {
	tp_name : PlacedName,
	tp_params :	ImmutableList<TypeParam>,
	tp_constraints : ImmutableList<TypeHint>,
	tp_meta : Metadata
}

typedef Documentation = Option<String>;

typedef Metadata = ImmutableList<MetadataEntry>;
typedef MetadataEntry = {
	name : StrictMeta,
	params : ImmutableList<Expr>,
	pos : Pos
};

enum Access {
	APublic;
	APrivate;
	AStatic;
	AOverride;
	ADynamic;
	AInline;
	AMacro;
	AFinal;
}

enum ClassFieldKind {
	FVar (th:Option<TypeHint>, e:Option<Expr>);
	FFun (func:Func);
	FProp (pn1:PlacedName, pn2:PlacedName, th:Option<TypeHint>, e:Option<Expr>);
}

typedef ClassField = {
	cff_name : PlacedName,
	cff_doc : Documentation,
	cff_pos : Pos,
	cff_meta : Metadata,
	cff_access : ImmutableList<Access>,
	cff_kind : ClassFieldKind
}

enum EnumFlag {
	EPrivate;
	EExtern;
}

enum ClassFlag {
	HInterface;
	HExtern;
	HPrivate;
	HExtends (ptp:PlacedTypePath);
	HImplements (ptp:PlacedTypePath);
}

enum AbstractFlag {
	APrivAbstract;
	AFromType (th:TypeHint);
	AToType (th:TypeHint);
	AIsType (th:TypeHint);
	AExtern;
}

typedef EnumConstructor = {
	ec_name : PlacedName,
	ec_doc : Documentation,
	ec_meta : Metadata,
	ec_args : ImmutableList<{name:String, opt:Bool, type:TypeHint}>,
	ec_pos : Globals.Pos,
	ec_params : ImmutableList<TypeParam>,
	ec_type : Option<TypeHint>
}

typedef Definition<A,B> = {
	d_name : PlacedName,
	d_doc : Documentation,
	d_params : ImmutableList<TypeParam>,
	d_meta : Metadata,
	d_flags : ImmutableList<A>,
	d_data : B,
}

enum ImportMode {
	INormal;
	IAsName (s:String);
	IAll;
}

typedef Import = {
	pns:ImmutableList<PlacedName>,
	mode:ImportMode
}

enum TypeDef {
	EClass (d:Definition<ClassFlag, ImmutableList<ClassField>>);// of (class_flag, class_field list) definition
	EEnum (d:Definition<EnumFlag, ImmutableList<EnumConstructor>>); //of (enum_flag, enum_constructor list) definition
	ETypedef (d:Definition<EnumFlag, TypeHint>);//of (enum_flag, type_hint) definition
	EAbstract (d:Definition<AbstractFlag, ImmutableList<ClassField>>);
	EImport (i:Import);
	EUsing (pns:ImmutableList<PlacedName>);
}

typedef TypeDecl = {
	decl:TypeDef,
	pos:Globals.Pos
}

typedef Package = {
	pack:ImmutableList<String>,
	decls:ImmutableList<TypeDecl>
}

class Invalid_escape_sequence {
	public var char:Int;
	public var i:Int;
	public function new(char:Int, i:Int) {
		this.char = char;
		this.i = i;
	}
}

class Ast {

	public static function is_lower_ident(i:String) : Bool {
		return haxeparser.HaxeParser.isLowerIdent(i);
	}

	public static inline function punion(p1:core.Globals.Pos, p2:core.Globals.Pos) {
		return new core.Globals.Pos(p1.pfile, 
			(p1.pmin < p2.pmin) ? p1.pmin : p2.pmin,
			(p1.pmax > p2.pmax) ? p1.pmax : p2.pmax
			);
	}

	public static function string_list_of_expr_path_raise (e:core.Ast.Expr) : ImmutableList<String> {
		return switch (e.expr) {
			case EConst(CIdent(i)) : [i];
			case EField(e, f): f :: string_list_of_expr_path_raise(e);
			case _: throw ocaml.Exit.instance;
		}
	}

	public static function expr_of_type_path(tp:core.Ast.TypePath, p:core.Globals.Pos) : core.Ast.Expr {
		var sl = tp.tpackage; var s = tp.tname;
		return switch (sl) {
			case []: {expr:EConst(CIdent(s)), pos:p};
			case s1::sl:
				var sl:ImmutableList<String> = sl;
				var e1 = {expr:EConst(CIdent(s1)), pos:p};
				var e = List.fold_left(function (e, s) {
					return {expr:EField(e, s), pos:p};
				}, e1, sl);
				{expr:EField(e,s), pos:p};
		}
	}

	public static function safe_for_all2<A,B> (f:A->B->Bool, a:ImmutableList<A>, b:ImmutableList<B>) : Bool {
		return try {
			List.for_all2(f, a, b);
		}
		catch (err:Bool) { throw err; }
		catch (_:Any) { false; }
	}

	public static function unescape (s:String) {
		var b = new StringBuf();
		var i = 0;
		var esc = false;
		while (true) {
			if (s.length == i) {
				break;
			}
			var c = s.charCodeAt(i);
			if (esc) {
				var iNext = i + 1;
				switch (c) {
					case 'n'.code: b.add("\n");
					case 'r'.code: b.add("\r");
					case 't'.code: b.add("\t");
					case '"'.code | '\''.code | '\\'.code: b.addChar(c);
					case _ >= '0'.code && _ <= '3'.code => true:
						iNext += 2;
					case 'x'.code:
						var chars = s.substr(i + 1, 2);
						if (!(~/^[0-9a-fA-F]{2}$/.match(chars))) throw new Invalid_escape_sequence(c,i);
						var c = Std.parseInt("0x" + chars);
						b.addChar(c);
						iNext += 2;
					case 'u'.code:
						var c:Int;
						if (s.charAt(i + 1) == "{") {
							var endIndex = s.indexOf("}", i + 3);
							if (endIndex == -1) throw new Invalid_escape_sequence(s.charCodeAt(i),i);
							var l = endIndex - (i + 2);
							var chars = s.substr(i + 2, l);
							if (!(~/^[0-9a-fA-F]+$/.match(chars))) throw new Invalid_escape_sequence(s.charCodeAt(i),i);
							c = Std.parseInt("0x" + chars);
							if (c > 0x10FFFF) throw new Invalid_escape_sequence(s.charCodeAt(i),i);
							iNext += 2 + l;
						} else {
							var chars = s.substr(i + 1, 4);
							if (!(~/^[0-9a-fA-F]{4}$/.match(chars))) throw new Invalid_escape_sequence(s.charCodeAt(i),i);
							c = Std.parseInt("0x" + chars);
							iNext += 4;
						}
						b.addChar(c);
					case c:
						throw new Invalid_escape_sequence(c,i);
				}
				esc = false;
				i = iNext;
			} else switch (c) {
				case '\\'.code:
					++i;
					esc = true;
				case _:
					b.addChar(c);
					++i;
			}

		}
		return b.toString();
	}

	public static function map_expr (loop:core.Ast.Expr->core.Ast.Expr, expr:core.Ast.Expr) : core.Ast.Expr {
		trace("TODO: core.Ast.map_expr");
		return null;
	}

	public static function s_expr (e:core.Ast.Expr) : String {
		trace("TODO: core.Ast.s_expr");
		return null;
	}

	public static function s_escape (?hex:Bool=true, s:String) : String {
		var b = new StringBuf();
		for (i in 0...s.length) {
			var c = s.charAt(i);
			switch (c) {
				case '\n': b.add("\\n");
				case '\t': b.add("\\t");
				case '\r': b.add("\\r");
				case '"': b.add("\\\"");
				case '\\': b.add("\\\\");
				default:
					var ci = c.charCodeAt(0);
					b.add((ci < 32 && hex) ? "\\x"+StringTools.hex(ci) : c);
			}
		}
		return b.toString();
	}

	public static function s_constant(c:core.Ast.Constant) : String {
		return switch (c) {
			case CInt(s): s;
			case CFloat(s): s;
			case CString(s): "\"" + s_escape(s) + "\"";
			case CIdent(s): s;
			case CRegexp(r,o): "~/" + r + "/";
		};
	}

	public static function s_access (a:core.Ast.Access) : String {
		return switch (a) {
			case APublic: "public";
			case APrivate: "private";
			case AStatic: "static";
			case AOverride: "override";
			case ADynamic: "dynamic";
			case AInline: "inline";
			case AMacro: "macro";
			case AFinal: "final";
		};
	}

	public static function s_keyword (kw:core.Ast.Keyword) : String {
		return switch (kw) {
			case Function: "function";
			case Class: "class";
			case Static: "static";
			case Var: "var";
			case If: "if";
			case Else: "else";
			case While: "while";
			case Do: "do";
			case For: "for";
			case Break: "break";
			case Return: "return";
			case Continue: "continue";
			case Extends: "extends";
			case Implements: "implements";
			case Import: "import";
			case Switch: "switch";
			case Case: "case";
			case Default: "default";
			case Private: "private";
			case Public: "public";
			case Try: "try";
			case Catch: "catch";
			case New: "new";
			case This: "this";
			case Throw: "throw";
			case Extern: "extern";
			case Enum: "enum";
			case In: "in";
			case Interface: "interface";
			case Untyped: "untyped";
			case Cast: "cast";
			case Override: "override";
			case Typedef: "typedef";
			case Dynamic: "dynamic";
			case Package: "package";
			case Inline: "inline";
			case Using: "using";
			case Null: "null";
			case True: "true";
			case False: "false";
			case Abstract: "abstract";
			case Macro: "macro";
			case Final: "final";
		};
	}

	public static function keyword_s (s:String) : Keyword {
		return switch (s) {
			case "function": Function;
			case "class": Class;
			case "static": Static;
			case "var": Var;
			case "if": If;
			case "else": Else;
			case "while": While;
			case "do": Do;
			case "for": For;
			case "break": Break;
			case "return": Return;
			case "continue": Continue;
			case "extends": Extends;
			case "implements": Implements;
			case "import": Import;
			case "switch": Switch;
			case "case": Case;
			case "default": Default;
			case "private": Private;
			case "public": Public;
			case "try": Try;
			case "catch": Catch;
			case "new": New;
			case "this": This;
			case "throw": Throw;
			case "extern": Extern;
			case "enum": Enum;
			case "in": In;
			case "interface": Interface;
			case "untyped": Untyped;
			case "cast": Cast;
			case "override": Override;
			case "typedef": Typedef;
			case "dynamic": Dynamic;
			case "package": Package;
			case "inline": Inline;
			case "using": Using;
			case "null": Null;
			case "true": True;
			case  "false": False;
			case "abstract": Abstract;
			case "macro": Macro;
			case "final": Final;
			default: null;
		};
	}
	
	public static function s_binop (op:core.Ast.Binop) : String {
		return switch (op) {
			case OpAdd: "+";
			case OpMult: "*";
			case OpDiv: "/";
			case OpSub: "-";
			case OpAssign: "=";
			case OpEq: "==";
			case OpNotEq: "!=";
			case OpGte: ">=";
			case OpLte: "<=";
			case OpGt: ">";
			case OpLt: "<";
			case OpAnd: "&";
			case OpOr: "|";
			case OpXor: "^";
			case OpBoolAnd: "&&";
			case OpBoolOr: "||";
			case OpShr: ">>";
			case OpUShr: ">>>";
			case OpShl: "<<";
			case OpMod: "%";
			case OpAssignOp(op): s_binop(op) + "=";
			case OpInterval: "...";
			case OpArrow: "=>";
			case OpIn: " in ";
		};
	}

	public static function s_unop (op:core.Ast.Unop) : String {
		return switch (op) {
			case OpIncrement: "++";
			case OpDecrement: "--";
			case OpNot: "!";
			case OpNeg: "-";
			case OpNegBits: "~";
		};
	}

	public static function s_token (token:core.Ast.Token) : String {
		return switch (token) {
			case Eof: "<end of file>";
			case Const(c): s_constant(c);
			case Kwd(k): s_keyword(k);
			case Comment(s): "/*"+s+"*/";
			case CommentLine(s): "//"+s;
			case Binop(o): s_binop(o);
			case Unop(o): s_unop(o);
			case Semicolon: ";";
			case Comma: ",";
			case BkOpen: "[";
			case BkClose: "]";
			case BrOpen: "{";
			case BrClose: "}";
			case POpen: "(";
			case PClose: ")";
			case Dot: ".";
			case DblDot: ":";
			case Arrow: "->";
			case IntInterval(s): s + "...";
			case Sharp(s): "#" + s;
			case Question: "?";
			case At: "@";
			case Dollar(v): "$" + v;
		};
	}

	public static function match_path (recursive:Bool, sl:ImmutableList<String>, sl_pattern:ImmutableList<String>) : Bool {
		function loop (sl1:ImmutableList<String>, sl2:ImmutableList<String>) {
			return switch ({f:sl1, s:sl2}) {
				case {f:[], s:[]}: true;
				// always recurse into types of package paths
				case {f:(s1::(s11::_)), s:[s2]} if (is_lower_ident(s2) && !(is_lower_ident(s11))):
					s1 == s2;
				case {f:[_], s:[""]}: true;
				case {s:([]|[""])}: recursive;
				case {f:[]}: false;
				case {f:s1::sl1, s:s2::sl2}:
					var sl1:ImmutableList<String> = sl1; var sl2:ImmutableList<String> = sl2;
					s1 == s2 && loop(sl1, sl2);
			}
		}
		return loop(sl, sl_pattern);
	}

	public static function full_dot_path(mpath:core.Path, tpath:core.Path) : ImmutableList<String> {
		if (mpath.equals(tpath)) {
			return List.concat(tpath.a, [tpath.b]);
		}
		else {
			return List.concat(tpath.a, [mpath.b, tpath.b]);
		}
	}
}