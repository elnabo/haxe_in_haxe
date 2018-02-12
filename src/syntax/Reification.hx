package syntax;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
import ocaml.List;

class Reification {
	var curPos:Option<core.Ast.Expr>;
	var inMacro:Bool;

	function new(inMacro:Bool){
		this.curPos = None;
		this.inMacro = inMacro;
	}

	public static function reify (inMacro:Bool) : { toExpr:core.Ast.Expr->core.Ast.Expr, toType:core.Ast.TypeHint->core.Globals.Pos->core.Ast.Expr, toTypeDef:core.Ast.TypeDecl->core.Ast.Expr } {
		var reificator = new Reification(inMacro);
		return {
			toExpr: function(e:core.Ast.Expr):core.Ast.Expr{
				return reificator.toExpr(e,e.pos);
			},
			toType: reificator.toCType,
			toTypeDef: reificator.toTypeDef
		};
	}

	public static function reifyExpr (e:core.Ast.Expr, inMacro:Bool) : core.Ast.Expr {
		var toExpr = reify(inMacro).toExpr;
		var e = toExpr(e);
		return { expr: ECheckType(e, 
			{
				ct:CTPath({
					tpackage:["haxe","macro"],
					tname:"Expr",
					tsub:None,
					tparams: []}), 
				pos:core.Globals.null_pos
			}),
			pos: e.pos
		};
	}

	function mkEnum(ename:String, name:String, vl:ImmutableList<core.Ast.Expr>, p:core.Globals.Pos) : core.Ast.Expr{
		// We don't want the position of the call expression to span the entire call (#6396).
		var pmin = new core.Globals.Pos(p.pfile, p.pmin, p.pmin);
		var constr:core.Ast.Expr = {expr:EConst(CIdent(name)), pos:pmin};
		switch (vl){
			case []: return constr;
			case _ : return {expr:ECall(constr, vl),pos:pmin};
		}
	}

	function toConst(c:core.Ast.Constant, p:core.Globals.Pos):core.Ast.Expr{
		function cst(n:String, v:String):core.Ast.Expr{
			return mkEnum("Constant", n, [{expr:EConst(CString(v)),pos:p}], p);
		}

		switch(c){
			case CInt(i): return cst("CInt", i);
			case CString(s): return cst("CString", s);
			case CFloat(s): return cst("CFloat", s);
			case CIdent(s): return cst("CIdent", s);
			case CRegexp(r,o): return mkEnum("Constant", "CRegexp", [{expr:EConst(CString(r)),pos:p},{expr:EConst(CString(o)),pos:p}], p);
		}
	}

	function toBinop(o:core.Ast.Binop,p:core.Globals.Pos) : core.Ast.Expr{
		function op(n:String):core.Ast.Expr{
			return mkEnum("Binop", n, [], p);
		}

		switch(o){
			case OpAdd: return op("OpAdd");
			case OpMult: return op("OpMult");
			case OpDiv: return op("OpDiv");
			case OpSub: return op("OpSub");
			case OpAssign: return op("OpAssign");
			case OpEq: return op("OpEq");
			case OpNotEq: return op("OpNotEq");
			case OpGt: return op("OpGt");
			case OpGte: return op("OpGte");
			case OpLt: return op("OpLt");
			case OpLte: return op("OpLte");
			case OpAnd: return op("OpAnd");
			case OpOr: return op("OpOr");
			case OpXor: return op("OpXor");
			case OpBoolAnd: return op("OpBoolAnd");
			case OpBoolOr: return op("OpBoolOr");
			case OpShl: return op("OpShl");
			case OpShr: return op("OpShr");
			case OpUShr: return op("OpUShr");
			case OpMod: return op("OpMod");
			case OpAssignOp(o): return mkEnum("Binop", "OpAssignOp", [toBinop(o, p)], p);
			case OpInterval: return op("OpInterval");
			case OpArrow: return op("OpArrow");
			#if (haxe_ver >= 4)
			case OpIn: return op("OpIn");
			#end
		}
	}

	function toString(s:String, p:core.Globals.Pos):core.Ast.Expr{
		var len = s.length;
		if (len>1 && s.charAt(0) == '$') return {expr:EConst(CIdent(s.substr(1))),pos:p};
		else return {expr:EConst(CString(s)),pos:p};
	}

	function toPlacedName(s:core.Ast.PlacedName) {
		return toString(s.pack, s.pos);
	}

	function toArray<T>(f:T->core.Globals.Pos->core.Ast.Expr, a:ImmutableList<T>, p:core.Globals.Pos):core.Ast.Expr{
		return {
			expr:EArrayDecl(List.map(function (s) { return f(s, p);}, a)),
			pos:p
		};
	}

	function toNull(p:core.Globals.Pos):core.Ast.Expr{
		return {expr:EConst(CIdent("null")),pos:p};
	}

	function toOpt<T>(f:T->core.Globals.Pos->core.Ast.Expr, v:Option<T>, p:core.Globals.Pos):core.Ast.Expr{
		return switch (v) {
			case None: toNull(p);
			case Some(s): f(s, p);
		}
	}

	function toBool(o:Bool, p:core.Globals.Pos):core.Ast.Expr{
		var s:String = o?"true":"false";
		return {expr:EConst(CIdent(s)),pos:p};
	}

	function toObj(fields:ImmutableList<{field:String, expr:core.Ast.Expr}>, p:core.Globals.Pos):core.Ast.Expr{
		var map = List.map(function (s):core.Ast.ObjectField {
			return {
				name:s.field,
				pos:core.Globals.null_pos,
				quotes:NoQuotes,
				expr:s.expr
			};
		}, fields);
		return {expr:EObjectDecl(map),pos:p};
	}

	function toTParam(t:core.Ast.TypeParamOrConst, p:core.Globals.Pos):core.Ast.Expr{
		var n:String;
		var v:core.Ast.Expr;
		switch(t){
			case TPType(t):
				n = "TPType";
				v = toCType(t, p);
			case TPExpr(e):
				n = "TPExpr";
				v = toExpr(e, p);
		}

		return mkEnum("TypeParam", n, [v], p);
	}

	function toTPath(ptp:core.Ast.PlacedTypePath, p:core.Globals.Pos):core.Ast.Expr{
		var t = ptp.tp;
		var len = t.tname.length;
		if (t.tpackage == Tl && len > 1 && t.tname.charAt(0) == "$") {
			var name = t.tname.substr(1);
			var ei:core.Ast.Expr = {expr:EConst(CIdent(name)), pos:p};
			return switch (t.tparams) {
				case []: ei;
				case _:
					//macro : $TP<Int>` conveys the intent to use TP and overwrite the type parameters.
					var ea = toArray(toTParam, t.tparams, p);
					var fields:ImmutableList<{field:String, expr:core.Ast.Expr}> = [
						{field:"pack", expr:{expr:EField(ei, "pack"), pos:p}},
						{field:"name", expr:{expr:EField(ei, "name"), pos:p}},
						{field:"sub", expr:{expr:EField(ei, "sub"), pos:p}},
						{field:"params", expr:ea}
					];
					toObj(fields, p);
			}
		}
		else {
			var fields:ImmutableList<{field:String, expr:core.Ast.Expr}> = [
				{field:"pack", expr:toArray(toString, t.tpackage, p)},
				{field:"name", expr:toString(t.tname, p)},
				{field:"pack", expr:toArray(toTParam, t.tparams, p)}
			];
			switch (t.tsub) {
				case None:
				case Some(s):
					fields = {field:"sub", expr:toString(s, p)} :: fields;
			}
			return toObj(fields, p);
		}
	}

	public function toCType(t:core.Ast.TypeHint, p:core.Globals.Pos):core.Ast.Expr {
		function ct(n:String, vl:ImmutableList<core.Ast.Expr>):core.Ast.Expr {
			return mkEnum("ComplexType", n, vl, p);
		}

		return switch(t.ct){
			case CTPath({tpackage: [], tparams: [], tsub: None, tname: n }) if (n.charAt(0) == '$'):
				toString(n, p);
			case CTPath(t): ct("TPath", [toTPath({tp:t, pos:p}, p)]);
			case CTFunction(args, ret): ct("TFunction", [toArray(toTypeHint, args, p), toTypeHint(ret, p)]);
			case CTAnonymous(fields): ct("TAnonymous", [toArray(toCField, fields, p)]);
			case CTParent(t): ct("TParent", [toTypeHint(t, p)]);
			case CTExtend(tl, fields): ct("TExtend", [toArray(toTPath, tl, p), toArray(toCField, fields, p)]);
			case CTOptional(t): ct("TOptional", [toTypeHint(t, p)]);
			case CTNamed(s,t): ct("TNamed", [toPlacedName(s), toTypeHint(t,p)]);
		}
	}
	function toTypeHint(t:core.Ast.TypeHint, _:core.Globals.Pos) : core.Ast.Expr {
		// to_obj ["type",to_ctype t p;"pos",to_pos p] p
		return toCType(t, t.pos);
	}

	function toFun(f:core.Ast.Func, p:core.Globals.Pos):core.Ast.Expr{
		var p = new core.Globals.Pos(p.pfile, p.pmin, p.pmin);
		function farg(vv:core.Ast.FunArg,p:core.Globals.Pos):core.Ast.Expr{
			var n = vv.name;
			var o = vv.opt;
			var t = vv.type;
			var e = vv.value;
			var fields:ImmutableList<{field:String, expr:core.Ast.Expr}> = [
				{field:"name", expr:toString(n.pack, p)},
				{field:"opt", expr:toBool(o, p)},
				{field:"type", expr:toOpt(toTypeHint, t, p)}
			];
			switch (e) {
				case None:
				case Some(e):
					fields = {field:"value", expr:toExpr(e, p)} :: fields;
			}
			return toObj(fields, p);
		}

		function fparam(t:core.Ast.TypeParam,p:core.Globals.Pos):core.Ast.Expr{
			var fields:ImmutableList<{field:String, expr:core.Ast.Expr}> = [
				{field:"name", expr:toPlacedName(t.tp_name)},
				{field:"constraints", expr:toArray(toCType, t.tp_constraints, p)},
				{field:"params", expr:toArray(fparam, t.tp_params, p)}
			];
			return toObj(fields, p);
		}

		var fields:ImmutableList<{field:String, expr:core.Ast.Expr}> = [
			{field:"args",   expr:toArray(farg, f.f_args, p)},
			{field:"ret",    expr:toOpt(toTypeHint, f.f_type, p)},
			{field:"expr",   expr:toOpt(toExpr, f.f_expr, p)},
			{field:"params", expr:toArray(fparam, f.f_params, p)}
		];

		return toObj(fields, p);
	}

	function toCField(f:core.Ast.ClassField, p:core.Globals.Pos):core.Ast.Expr {
		var p = f.cff_pos;

		function toKind(k:core.Ast.ClassFieldKind):core.Ast.Expr {
			var n:String;
			var vl:ImmutableList<core.Ast.Expr>;
			switch(k){
				case FVar(ct, e): n = "FVar"; vl = [toOpt(toTypeHint, ct, p), toOpt(toExpr, e, p)];
				case FFun(f): n = "FFun"; vl = [toFun(f, p)];
				case FProp(get, set, t, e): n = "FProp"; vl = [toPlacedName(get), toPlacedName(set), toOpt(toTypeHint, t, p), toOpt(toExpr, e, p)];
			}
			return mkEnum("FieldType", n, vl, p);
		}

		var fields:Array<{field:String, expr:core.Ast.Expr}> = [];
		fields.push({field:"name", expr:toPlacedName(f.cff_name)});
		switch (f.cff_doc) {
			case None:
			case Some(s):
				fields.push({field:"doc", expr:toString(s, p)});
		}
		if (f.cff_access != Tl) {
			fields.push({field:"access", expr:toArray(toAccess, f.cff_access, p)});
		}
		fields.push({field:"kind", expr:toKind(f.cff_kind)});
		fields.push({field:"pos",  expr:toPos(f.cff_pos)});
		if (f.cff_meta != Tl) {
			fields.push({field:"meta", expr:toMeta(f.cff_meta, p)});
		}
		return toObj(List.rev(fields), p);
	}

	function toAccess(a:core.Ast.Access, p:core.Globals.Pos):core.Ast.Expr {
		var n = switch(a){
			case AFinal:     "AFinal";
			case APublic :   "APublic";
			case APrivate :  "APrivate";
			case AStatic :   "AStatic";
			case AOverride : "AOverride";
			case ADynamic :  "ADynamic";
			case AInline :   "AInline";
			case AMacro :    "AMacro";
		}
		return mkEnum("Access", n, [], p);
	}

	function toMeta(m:core.Ast.Metadata, p:core.Globals.Pos):core.Ast.Expr {
		return toArray(function(me:core.Ast.MetadataEntry, _:core.Globals.Pos):core.Ast.Expr {
			var fields:ImmutableList<{field:String, expr:core.Ast.Expr}> = [
				{field:"name",   expr:toString(core.Meta.to_string(me.name), me.pos)},
				{field:"params", expr:toExprArray(me.params, me.pos)},
				{field:"pos",    expr:toPos(me.pos)}
			];
			return toObj(fields, me.pos);
		}, m, p);
	}

	function toPos(p:core.Globals.Pos):core.Ast.Expr {
		switch (curPos) {
			case Some(posExpr): return posExpr;
			default:
		}

		var file:core.Ast.Expr = {expr:EConst(CString(p.pfile)), pos:p};
		var pmin:core.Ast.Expr = {expr:EConst(CInt(Std.string(p.pmin))), pos:p};
		var pmax:core.Ast.Expr = {expr:EConst(CInt(Std.string(p.pmax))), pos:p};
		if (inMacro)
			return {expr:EUntyped({expr:ECall({expr:EConst(CIdent("$__mk_pos__")), pos:p}, [file, pmin, pmax]), pos:p}), pos:p};
		else
			return toObj([{field:"file", expr:file}, {field:"min", expr:pmin}, {field:"max", expr:pmax}], p);
	}

	function toExprArray(a:ImmutableList<core.Ast.Expr>, p:core.Globals.Pos):core.Ast.Expr {
		return switch (a) {
			case [{expr:EMeta({name:Dollar("a"), params:[]},e1)}]:
				switch(e1.expr){
					case EArrayDecl(el): toExprArray(el, p);
					case _: e1;
				}
			case _: toArray(toExpr, a, p);
		}
	}

	public function toExpr(e:core.Ast.Expr, _:core.Globals.Pos):core.Ast.Expr {
		var p = e.pos;
		function expr(n:String, vl:ImmutableList<core.Ast.Expr>):core.Ast.Expr {
			var e = mkEnum("ExprDef", n, vl, p);
			return toObj([{field:"expr", expr:e}, {field:"pos", expr:toPos(p)}], p);
		}
		function loop(e:core.Ast.Expr):core.Ast.Expr {
			return toExpr(e, e.pos);
		}
		return switch(e.expr){
			case EConst(CIdent(n)) if (n.charAt(0) == '$' && n.length > 1 && n != "$__mk_pos__"):
				toString(n, p);
			case EConst(c):
				expr("EConst", [toConst(c, p)]);
			case EArray(e1, e2):
				expr("EArray", [loop(e1), loop(e2)]);
			case EBinop(op, e1, e2):
				expr("EBinop", [toBinop(op, p), loop(e1), loop(e2)]);
			case EField(e, s):
				var p = new core.Globals.Pos(p.pfile, p.pmax - s.length, p.pmin);
				expr("EField", [loop(e), toString(s, p)]);
			case EParenthesis(e):
				expr("EParenthesis", [loop(e)]);
			case EObjectDecl(fl):
				function quote_kind(kk:core.Ast.QuoteStatus, p:core.Globals.Pos) : core.Ast.Expr {
					var n = switch (kk) {
						case NoQuotes: "Unquoted";
						case DoubleQuotes: "Quoted";
					};
					return mkEnum("QuoteStatus", n, [], p);
				}
				return expr("EObjectDecl", [toArray(
					function(f:core.Ast.ObjectField, p2:core.Globals.Pos) {
						return toObj([
							{field:"field", expr:toString(f.name,p)},
							{field:"expr", expr:loop(f.expr)},
							{field:"quotes", expr:quote_kind(f.quotes, f.pos)}
						], p2);
					}, fl, p)
				]);
			case EArrayDecl(el):
				expr("EArrayDecl", [toExprArray(el, p)]);
			case ECall(e, el):
				expr("ECall", [loop(e), toExprArray(el, p)]);
			case ENew(t, el):
				expr("ENew", [toTPath(t, p), toExprArray(el, p)]);
			case EUnop(op, flag, e):
				var op2 = mkEnum("Unop",
					switch(op){
						case OpIncrement: "OpIncrement";
						case OpDecrement: "OpDecrement";
						case OpNot: "OpNot";
						case OpNeg: "OpNeg";
						case OpNegBits: "OpNegBits";
					}, [], p);
				expr("EUnop", [op2, toBool(flag == Postfix, p), loop(e)]);
			case EVars(vl):
				expr("EVars", [toArray(function(vv:core.Ast.Var, p:core.Globals.Pos):core.Ast.Expr {
					var name = vv.name; var type = vv.type; var expr = vv.expr;
					var fields:ImmutableList<{field:String, expr:core.Ast.Expr}> = [
						// "name", to_obj ["name",to_string n pn;"pos",to_pos pn] p;
						{field:"name", expr:toString(name.pack, p)},
						{field:"type", expr:toOpt(toTypeHint, type, p)},
						{field:"expr", expr:toOpt(toExpr, expr, p)}
					];
					return toObj(fields, p);
				}, vl, p)]);
			case EFunction(name, f):
				var name = switch (name) {
					case None: toNull(core.Globals.null_pos);
					case Some(n):
						if (StringTools.startsWith(n, "inline_$")) {
							var real_name = n.substr(7);
							var e_name = toString(real_name, p);
							var e_inline = toString("inline_", p);
							var e_add:core.Ast.Expr = {expr:EBinop(OpAdd, e_inline, e_name), pos:p};
							e_add;
						}
						else {
							toString(n, p);
						}
				}
				expr("EFunction", [name, toFun(f, p)]);
			case EBlock(el):
				expr("EBlock", [toExprArray(el, p)]);
			case EFor(e1, e2):
				expr("EFor", [loop(e1), loop(e2)]);
			case EIf(e1, e2, eelse):
				expr("EIf", [loop(e1), loop(e2), toOpt(toExpr, eelse, p)]);
			case EWhile(e1, e2, flag):
				expr("EWhile", [loop(e1), loop(e2), toBool(flag==NormalWhile, p)]);
			case ESwitch(e1, cases, def):
				function scase(swc:core.Ast.Case, p:core.Globals.Pos):core.Ast.Expr {
					var el = swc.values;
					var eg = swc.guard;
					var e = swc.expr;
					return toObj([
						{field:"values", expr:toExprArray(el, p)},
						{field:"guard", expr:toOpt(toExpr, eg, p)},
						{field:"expr", expr:toOpt(toExpr, e, p)}], p);
				}
				expr("ESwitch", [
					loop(e1),
					toArray(scase, cases,p),
					toOpt(function (e:{e:Option<core.Ast.Expr>,pos:core.Globals.Pos}, p:core.Globals.Pos) : core.Ast.Expr{
						return toOpt(toExpr, e.e, p);
					}, def, p)
				]);
			case ETry(e1, catches):
				function scatch(c:core.Ast.Catch, p:core.Globals.Pos):core.Ast.Expr {
					var n = c.name;
					var t = c.type;
					var e = c.expr;
					return toObj([
						{field:"name", expr:toString(n.pack, p)},
						{field:"type", expr:toCType(t, p)},
						{field:"expr", expr:loop(e)}], p);
				}
				expr("ETry", [loop(e1), toArray(scatch, catches, p)]);
			case EReturn(eo):
				expr("EReturn", [toOpt(toExpr, eo, p)]);
			case EBreak:
				expr("EBreak", []);
			case EContinue:
				expr("EContinue", []);
			case EUntyped(e):
				expr("EUntyped", [loop(e)]);
			case EThrow(e):
				expr("EThrow", [loop(e)]);
			case ECast(e, ct):
				expr("ECast", [loop(e), toOpt(toTypeHint, ct, p)]);
			case EDisplay(e, flag):
				expr("EDisplay", [loop(e), toBool(flag, p)]);
			case EDisplayNew(t):
				expr("EDisplayNew", [toTPath(t, p)]);
			case ETernary(e1, e2, e3):
				expr("ETernary", [loop(e1), loop(e2), loop(e3)]);
			case ECheckType(e1, ct):
				expr("ECheckType", [loop(e1), toTypeHint(ct, p)]);
			case EMeta(md, e1):
				switch(core.Meta.to_string(md.name)){
				case "$" | "$e":
					e1;
				case "$a":
					switch(e1.expr){
					case EArrayDecl(el): expr("EArrayDecl", [toExprArray(el, p)]);
					default: expr("EArrayDecl", [e1]);
					}
				case "$b":
					expr("EBlock", [e1]);
				case "$v":
					switch (e1.expr) {
						case EParenthesis({expr:ECheckType(e2,{ct:CTPath({tname:"String",tpackage:[]})})}):
							expr("EConst", [mkEnum("Constant", "CString", [e2], e2.pos)]);
						case EParenthesis({expr:ECheckType(e2,{ct:CTPath({tname:"Int",tpackage:[]})})}):
							expr("EConst", [mkEnum("Constant", "CInt", [e2], e2.pos)]);
						case EParenthesis({expr:ECheckType(e2,{ct:CTPath({tname:"Float",tpackage:[]})})}):
							expr("EConst", [mkEnum("Constant", "CFloat", [e2], e2.pos)]);
						default:
							{expr:ECall(
								{expr:EField(
									{expr:EField(
										{expr:EField(
											{expr:EConst(CIdent("haxe")),
											pos:p},
											"macro"
										),
										pos:p},
										"Context"
									),
									pos:p},
									"makeExpr"
								),
								pos:p},
								[e, toPos(e.pos)]
							),
							pos:p};
					}
				case "$i":
					expr("EConst", [mkEnum("Constant", "CIdent", [e1], e1.pos)]);
				case "$p":
					{expr:ECall(
						{expr:EField(
							{expr:EField(
								{expr:EField(
									{expr:EConst(CIdent("haxe")),
									pos:p},
									"macro"
								),
								pos:p},
								"MacroStringTools"
							),
							pos:p},
							"toFieldExpr"
						),
						pos:p},
						[e]
					),
					pos:p};
				case ":pos" if (List.length(md.params) == 1):
					var old = curPos;
					switch (md.params) {
						case Hd(v, Tl): curPos = Some(v);
						case _: throw false;
					}
					var e = loop(e1);
					curPos = old;
					e;
				case _: expr("EMeta", [
						toObj([
							{field:"name",expr:toString(core.Meta.to_string(md.name), p)},
							{field:"params",expr:toExprArray(md.params, p)},
							{field:"pos",expr:toPos(p)}
						], p),
						loop(e1)
					]);
				}
		}
	}

	function toTParamDecl(t:core.Ast.TypeParam, p:core.Globals.Pos):core.Ast.Expr{
		var params = List.map(function (tp) { 
			return toTParamDecl(tp, p);
		}, t.tp_params);
		var constraints = List.map(function (c) { 
			return toCType(c, p);
		}, t.tp_constraints);

		return toObj([
			{field:"name", expr:toPlacedName(t.tp_name)},
			{field:"params", expr:{expr:EArrayDecl(params),pos:p}},
			{field:"constraints", expr:{expr:EArrayDecl(constraints),pos:p}}
		],p);
	}

	public function toTypeDef(td:core.Ast.TypeDecl):core.Ast.Expr{
		var p = td.pos;

		switch(td.decl){
			case EClass(d):
				var ext = None;
				var impl:ImmutableList<core.Ast.Expr> = [];
				var interf = false;

				List.iter(function (f:core.Ast.ClassFlag) {
					switch(f){
						case HExtern | HPrivate:
						case HInterface: interf = true;
						case HExtends(t): 
							ext = switch (ext) {
								case None: Some(toTPath(t, p));
								case Some(_):
									impl = (toTPath(t, p)) :: impl;
									ext;
							};
						case HImplements(i): (toTPath(i, td.pos)) :: impl;
					}
				}, d.d_flags);

				var params = List.map(function (par) {
					return toTParamDecl(par, p);
				}, d.d_params);

				var isExtern = List.mem(core.Ast.ClassFlag.HExtern, d.d_flags);

				var kindParams:ImmutableList<core.Ast.Expr> = [
					switch(ext) { case None: {expr:EConst(CIdent("null")), pos:p}; case Some(t): t;},
					{expr:EArrayDecl(impl), pos:p},
					toBool(interf, p)
				];

				var fields = List.map(function (f) {
					return toCField(f, p);
				}, d.d_data);

				return toObj([
					{field:"pack", expr:{expr:EArrayDecl([]),pos:p}},
					{field:"name", expr:toString(d.d_name.pack, p)},
					{field:"pos", expr:(toPos(p))},
					{field:"meta", expr:toMeta(d.d_meta, p)},
					{field:"params", expr:{expr:EArrayDecl(params),pos:p}},
					{field:"isExtern", expr:toBool(isExtern, p)},
					{field:"kind", expr:mkEnum("TypeDefKind", "TDClass", kindParams, p)},
					{field:"fields", expr:{expr:EArrayDecl(fields), pos:p}}
				], td.pos);
			case _: throw false;
		}
	}
}