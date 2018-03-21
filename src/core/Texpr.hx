package core;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
import ocaml.Hashtbl;
import ocaml.List;
import ocaml.PMap;
import ocaml.Ref;

using equals.Equal;
using ocaml.Cloner;

import core.Type;

class Builder {
	public static function make_static_this (c:core.Type.TClass, p:core.Globals.Pos) : core.Type.TExpr {
		var ta:core.Type.T = TAnon({a_fields:c.cl_statics, a_status:new Ref<core.Type.AnonStatus>(Statics(c))});
		return core.Type.mk(TTypeExpr(TClassDecl(c)), ta, p);
	}

	public static function make_typeexpr (mt:core.Type.ModuleType, pos:core.Globals.Pos) : core.Type.TExpr {
		var t:core.Type.T = switch (mt) {
			case TClassDecl(c): TAnon({a_fields:c.cl_statics, a_status:new Ref(Statics(c))});
			case TEnumDecl(e): TAnon({a_fields:PMap.empty(), a_status:new Ref(EnumStatics(e))});
			case TAbstractDecl(a): TAnon({a_fields:PMap.empty(), a_status:new Ref(AbstractStatics(a))});
			case _: trace("Shall not be seen"); std.Sys.exit(255); throw false;
		}
		return core.Type.mk(TTypeExpr(mt), t, pos);
	}

	public static function make_static_field (c:core.Type.TClass, cf:core.Type.TClassField, p:core.Globals.Pos) : core.Type.TExpr {
		var e_this = make_static_this(c, p);
		return core.Type.mk(TField(e_this, FStatic(c, cf)), cf.cf_type, p);
	}

	public static function make_int (basic:core.Type.BasicTypes, i:haxe.Int32, p:core.Globals.Pos) : core.Type.TExpr{
		return core.Type.mk(TConst(TInt(i)), basic.tint, p);
	}

	public static function make_null (t:core.Type.T, p:core.Globals.Pos) : core.Type.TExpr{
		return core.Type.mk(TConst(TNull), t, p);
	}

	public static function make_local (v:core.Type.TVar, p:core.Globals.Pos) : core.Type.TExpr {
		return core.Type.mk(TLocal(v), v.v_type, p);
	}

	public static function make_const_texpr (basic:core.Type.BasicTypes, ct:core.Type.TConstant, p:core.Globals.Pos) : core.Type.TExpr{
		return switch (ct) {
			case TString(s): core.Type.mk(TConst(TString(s)), basic.tstring, p);
			case TInt(i): core.Type.mk(TConst(TInt(i)), basic.tint, p);
			case TFloat(f): core.Type.mk(TConst(TFloat(f)), basic.tfloat, p);
			case TBool(b): core.Type.mk(TConst(TBool(b)), basic.tbool, p);
			case TNull: core.Type.mk(TConst(TNull), basic.tnull(core.Type.mk_mono()), p);
			case _: core.Error.error("Unsupported constant", p);
		}
	}

	public static function mk_parent(e:core.Type.TExpr) : core.Type.TExpr {
		return core.Type.mk(TParenthesis(e), e.etype, e.epos);
	}

	public static function field (e:core.Type.TExpr, name:String, t:core.Type.T, p:core.Globals.Pos) : core.Type.TExpr {
		return core.Type.mk(TField(e, try { core.Type.quick_field(e.etype, name); } catch (_:ocaml.Not_found) { throw false; }), t, p);
	}

	public static function fcall (e:core.Type.TExpr, name:String, el:ImmutableList<core.Type.TExpr>, ret:core.Type.T, p:core.Globals.Pos) : core.Type.TExpr {
		var ft = core.Type.tfun(List.map(function (e:core.Type.TExpr) { return e.etype; }, el), ret);
		return core.Type.mk(TCall(field(e, name, ft, p), el), ret, p);
	}

	public static function binop (op:core.Ast.Binop, a:core.Type.TExpr, b:core.Type.TExpr, t:core.Type.T, p:core.Globals.Pos) : core.Type.TExpr {
		return core.Type.mk(TBinop(op, a, b), t, p);
	}
}

class Texpr {
	public static function equal_fa (fa1:core.Type.TFieldAccess, fa2:core.Type.TFieldAccess) : Bool {
		return switch {f:fa1,s:fa2} {
			// case {f:FStatic(c1,cf1), s:FStatic(c2,cf2)}: c1 == c2 && cf1 == cf2;
			case {f:FStatic(c1,cf1), s:FStatic(c2,cf2)}: c1.equals(c2) && cf1.equals(cf2);
			// case {f:FInstance(c1,tl1,cf1), s:FInstance(c2,tl2,cf2)}: c1 == c2 && safe_for_all2 type_iseq tl1 tl2 && cf1 == cf2;
			case {f:FInstance(c1,tl1,cf1), s:FInstance(c2,tl2,cf2)}: c1.equals(c2) && core.Ast.safe_for_all2(core.Type.type_iseq, tl1, tl2) && cf1.equals(cf2);
			// TODO: This is technically not correct but unfortunately the compiler makes a distinct tclass_field for each anon field access.
			case {f:FAnon(cf1), s:FAnon(cf2)}: cf1.cf_name == cf2.cf_name;
			case {f:FDynamic(s1), s:FDynamic(s2)}: s1.equals(s2);
			// case {f:FClosure(None,cf1),s:FClosure(None,cf2)}: cf1 == cf2;
			case {f:FClosure(None,cf1),s:FClosure(None,cf2)}: cf1.equals(cf2);
			// case {f:FClosure(Some(c1,tl1),cf1),s:FClosure(Some(c2,tl2),cf2)}: c1 == c2 && safe_for_all2 type_iseq tl1 tl2 && cf1 == cf2;
			case {f:FClosure(Some({c:c1,params:tl1}),cf1),s:FClosure(Some({c:c2,params:tl2}),cf2)}: c1.equals(c2) && core.Ast.safe_for_all2(core.Type.type_iseq, tl1, tl2) && cf1.equals(cf2);
			// case {f:FEnum(en1,ef1), s:FEnum(en2,ef2)}: en1 == en2 && ef1 == ef2;
			case {f:FEnum(en1,ef1), s:FEnum(en2,ef2)}: en1.equals(en2) && ef1.equals(ef2);
			case _: false;
		}
	}

	public static function equal (e1:core.Type.TExpr, e2:core.Type.TExpr) : Bool {
		return switch ({f:e1.eexpr, s:e2.eexpr}) {
			case {f:TConst(ct1), s:TConst(ct2)}: ct1.equals(ct2);
			// case {f:TLocal(v1) , s:TLocal(v2)}: v1 == v2;
			case {f:TLocal(v1) , s:TLocal(v2)}: v1.equals(v2);
			case {f:TArray(eb1,ei1), s:TArray(eb2,ei2)}: equal(eb1, eb2) && equal(ei1, ei2);
			case {f:TBinop(op1,lhs1,rhs1), s:TBinop(op2,lhs2,rhs2)}: op1.equals(op2) && equal(lhs1, lhs2) && equal(rhs1, rhs2);
			case {f:TField(e1,fa1), s:TField(e2,fa2)}: equal(e1, e2) && equal_fa(fa1, fa2);
			// case {f:TTypeExpr(mt1), s:TTypeExpr(mt2)}: mt1 == mt2;
			case {f:TTypeExpr(mt1), s:TTypeExpr(mt2)}: mt1.equals(mt2);
			case {f:TParenthesis(e1), s:TParenthesis(e2)}: equal(e1, e2);
			// case {f:TObjectDecl(fl1), s:TObjectDecl(fl2)}: safe_for_all2(function (s1,e1) (s2,e2)  s1 = s2 && equal e1 e2) fl1 fl2
			case {f:TObjectDecl(fl1), s:TObjectDecl(fl2)}: core.Ast.safe_for_all2(function (o1:core.Type.TObjectField, o2:core.Type.TObjectField) { return o1.name == o2.name && o1.pos.equals(o2.pos) && o1.quotes == o2.quotes && equal(o1.expr, o2.expr);}, fl1, fl2);
			case {f:TArrayDecl(el1), s:TArrayDecl(el2)}, {f:TBlock(el1), s:TBlock(el2)}: core.Ast.safe_for_all2(equal, el1, el2);
			case {f:TCall(e1,el1), s:TCall(e2,el2)}: equal(e1, e2) && core.Ast.safe_for_all2(equal, el1, el2);
			// case {f:TNew(c1,tl1,el1), s:TNew(c2,tl2,el2)}: c1 == c2 && safe_for_all2 type_iseq tl1 tl2 && safe_for_all2 equal el1 el2
			case {f:TNew(c1,tl1,el1), s:TNew(c2,tl2,el2)}: c1.equals(c2) && core.Ast.safe_for_all2(core.Type.type_iseq, tl1, tl2) && core.Ast.safe_for_all2(equal, el1, el2);
			case {f:TUnop(op1,flag1,e1), s:TUnop(op2,flag2,e2)}: op1.equals(op2) && flag1.equals(flag2) && equal(e1, e2);
			// case {f:TFunction(tf1), s:TFunction(tf2)}: tf1 == tf2;
			case {f:TFunction(tf1), s:TFunction(tf2)}: tf1.equals(tf2);
			// case {f:TVar(v1,None), s:TVar(v2,None)}: v1 == v2;
			case {f:TVar(v1,None), s:TVar(v2,None)}: v1.equals(v2);
			// case {f:TVar(v1,Some e1), s:TVar(v2,Some e2)}: v1 == v2 && equal(e1, e2);
			case {f:TVar(v1,Some(e1)), s:TVar(v2,Some(e2))}: v1.equals(v2) && equal(e1, e2);
			// case {f:TFor(v1,ec1,eb1), s:TFor(v2,ec2,eb2)}: v1 == v2 && equal(ec1, ec2) && equal(eb1, eb2);
			case {f:TFor(v1,ec1,eb1), s:TFor(v2,ec2,eb2)}: v1.equals(v2) && equal(ec1, ec2) && equal(eb1, eb2);
			case {f:TIf(e1,ethen1,None), s:TIf(e2,ethen2,None)}: equal(e1, e2) && equal(ethen1, ethen2);
			case {f:TIf(e1,ethen1,Some(eelse1)), s:TIf(e2,ethen2,Some(eelse2))}: equal(e1, e2) && equal(ethen1, ethen2) && equal(eelse1, eelse2);
			case {f:TWhile(e1,eb1,flag1), s:TWhile(e2,eb2,flag2)}: equal(e1, e2) && equal(eb2, eb2) && flag1.equals(flag2);
			case {f:TSwitch(e1,cases1,eo1), s:TSwitch(e2,cases2,eo2)}:
				equal(e1, e2) &&
				core.Ast.safe_for_all2(function (c1, c2) {
					var el1 = c1.values; var e1 = c1.e;
					var el2 = c2.values; var e2 = c2.e;
					return core.Ast.safe_for_all2(equal, el1, el2) && equal(e1, e2);}, cases1, cases2) &&
				switch {f:eo1,s:eo2} {
					case {f:None, s:None}: true;
					case {f:Some(e1), s:Some(e2)}: equal(e1, e2);
					case _: false;
				};
			// case {f:TTry(e1,catches1),TTry(e2,catches2) -> equal e1 e2 && safe_for_all2 (fun (v1,e1) (v2,e2) -> v1 == v2 && equal e1 e2) catches1 catches2
			case {f:TTry(e1,catches1),s:TTry(e2,catches2)}: equal(e1, e2) && core.Ast.safe_for_all2 (function (c1:{v:core.Type.TVar, e:core.Type.TExpr}, c2:{v:core.Type.TVar, e:core.Type.TExpr}) {
				var v1 = c1.v; var e1 = c1.e;
				var v2 = c2.v; var e2 = c2.e;
				return v1.equals(v2) && equal(e1, e2); }, catches1, catches2);
			case {f:TReturn(None),s:TReturn(None)}: true;
			case {f:TReturn(Some(e1)),s:TReturn(Some(e2))}: equal(e1, e2);
			case {f:TThrow(e1),s:TThrow(e2)}: equal(e1, e2);
			case {f:TCast(e1,None),s:TCast(e2,None)}: equal(e1, e2);
			// case {f:TCast(e1,Some mt1),TCast(e2,Some mt2)}: equal(e1, e2) && mt1 == mt2;
			case {f:TCast(e1,Some(mt1)),s:TCast(e2,Some(mt2))}: equal(e1, e2) && mt1.equals(mt2);
			// case {f:TMeta((m1,el1,_),e1),TMeta((m2,el2,_),e2)}: m1 = m2 && safe_for_all2 (fun e1 e2 -> (* TODO: cheating? *) (Ast.s_expr e1) = (Ast.s_expr e2)) el1 el2 && equal e1 e2
			case {f:TBreak,s:TBreak}, {f:TContinue,s:TContinue}: true;
			// case {f:TEnumParameter(e1,ef1,i1),TEnumParameter(e2,ef2,i2)}: equal(e1, e2) && ef1 == ef2 && i1.equals(i2);
			case {f:TEnumParameter(e1,ef1,i1),s: TEnumParameter(e2,ef2,i2)}: equal(e1, e2) && ef1.equals(ef2) && i1.equals(i2);
			case _: false;
		}
	}

	public static function duplicate_tvars (e:core.Type.TExpr) : core.Type.TExpr {
		var vars = new Hashtbl<Int, core.Type.TVar>();
		function copy_var(v:core.Type.TVar) : core.Type.TVar {
			var v2 = core.Type.alloc_var(v.v_name, v.v_type, v.v_pos);
			v2.v_meta = v.v_meta;
			v2.v_extra = v.v_extra;
			Hashtbl.add(vars, v.v_id, v2);
			return v2;
		}
		function build_expr(e:core.Type.TExpr) : core.Type.TExpr {
			return switch (e.eexpr) {
				case TVar(v, eo):
					var v2 = copy_var(v);
					e.with({eexpr:core.Type.TExprExpr.TVar(v2, ocaml.Option.map(build_expr, eo))});
				case TFor(v, e1, e2):
					var v2 = copy_var(v);
					e.with({eexpr:core.Type.TExprExpr.TFor(v2, build_expr(e1), build_expr(e2))});
				case TTry(e1, cl):
					var cl = List.map(function (arg) {
						var v = arg.v; var e = arg.e;
						var v2 = copy_var(v);
						return {v:v2, e:build_expr(e)};
					}, cl);
					e.with({eexpr:core.Type.TExprExpr.TTry(build_expr(e1), cl)});
				case TFunction(f):
					var args = List.map(function (arg) {
						var v = arg.v; var c = arg.c;
						return {v:copy_var(v), c:c};
					}, f.tf_args);
					var f = {
						tf_args: args,
						tf_type: f.tf_type,
						tf_expr: build_expr(f.tf_expr)
					};
					e.with({eexpr:core.Type.TExprExpr.TFunction(f)});
				case TLocal(v):
					try {
						var v2 = Hashtbl.find(vars, v.v_id);
						e.with({eexpr:core.Type.TExprExpr.TLocal(v2)});
					}
					catch (_:Bool) { trace("Shall not be seen"); throw false; }
					catch (_:Any) {
						e;
					}
				case _: core.Type.map_expr(build_expr,e);

			}
		}
		return build_expr(e);
	}

	public static function skip (e:core.Type.TExpr) : core.Type.TExpr {
		return switch (e.eexpr) {
			case TParenthesis(e1), TMeta(_, e1), TBlock([e1]), TCast(e1, None): skip(e1);
			case _: e;
		}
	}

	public static function foldmap_list<A>(f:(A, TExpr)->{fst:A, snd:TExpr}, acc:A, el:ImmutableList<TExpr>) : {fst:A, snd:ImmutableList<TExpr>} {
		function loop (acc:A, el:ImmutableList<TExpr>, acc2:ImmutableList<TExpr>) {
			return switch (el) {
				case []: {fst:acc, snd:List.rev(acc2)};
				case e1 :: el:
					var _tmp = f(acc, e1);
					var acc = _tmp.fst; var e1 = _tmp.snd;
					loop(acc, el, e1::acc2);
			}
		}
		return loop(acc, el, Tl);
	}
	public static function foldmap_opt<A>(f:(A, TExpr)->{fst:A, snd:TExpr}, acc:A, eo:Option<TExpr>) : {fst:A, snd:Option<TExpr>} {
		return switch (eo) {
			case Some(e):
				var _tmp = f(acc, e);
				var acc = _tmp.fst; var e = _tmp.snd;
				{fst:acc, snd:Some(e)};
			case None: {fst:acc, snd:eo};
		}
	}
	static function foldmap_pairs_objectdecl<A>(f:(A, TExpr)->{fst:A, snd:TExpr}, acc:A, pairs:ImmutableList<TObjectField>): {fst:A, snd:ImmutableList<TObjectField>} {
		var _tmp = List.fold_left(function (arg1, p:TObjectField) {
			var acc = arg1.fst; var el = arg1.snd; var e = p.expr;
			var _tmp = f(acc, e);
			var acc = _tmp.fst; var e = _tmp.snd;
			return {fst:acc, snd:p.with({expr:e})::el};
		}, {fst:acc, snd:[]}, pairs);
		var acc = _tmp.fst; var pairs = _tmp.snd;
		return {fst:acc, snd:List.rev(pairs)};
	}
	static function foldmap_pairs_catches<A>(f:(A, TExpr)->{fst:A, snd:TExpr}, acc:A, pairs:ImmutableList<{v:Any, e:Any}>): {fst:A, snd:ImmutableList<{v:Any, e:Any}>} {
		var _tmp = List.fold_left(function (arg1, p:{v:Any, e:Any}) {
			var acc = arg1.fst; var el = arg1.snd; var e = p.e;
			var _tmp = f(acc, e);
			var acc = _tmp.fst; var e = _tmp.snd;
			return {fst:acc, snd:p.with({e:e})::el};
		}, {fst:acc, snd:[]}, pairs);
		var acc = _tmp.fst; var pairs = _tmp.snd;
		return {fst:acc, snd:List.rev(pairs)};
	}

	public static function foldmap<A> (f:(A, TExpr)->{fst:A, snd:core.Type.TExpr}, acc:A, e:TExpr) : {fst:A, snd:TExpr} {
		return switch (e.eexpr) {
			case TConst(_), TLocal(_), TBreak, TContinue, TTypeExpr(_), TIdent(_):
				{fst:acc, snd:e};
			case TArray(e1, e2):
				var _tmp = f(acc, e1);
				var acc = _tmp.fst; var e1 = _tmp.snd;
				var _tmp = f(acc, e2);
				var acc = _tmp.fst; var e2 = _tmp.snd;
				{fst:acc, snd: e.with({eexpr:TArray(e1, e2)})};
			case TBinop(op, e1, e2):
				var _tmp = f(acc, e1);
				var acc = _tmp.fst; var e1 = _tmp.snd;
				var _tmp = f(acc, e2);
				var acc = _tmp.fst; var e2 = _tmp.snd;
				{fst:acc, snd: e.with({eexpr:TBinop(op, e1, e2)})};
			case TFor(v, e1, e2):
				var _tmp = f(acc, e1);
				var acc = _tmp.fst; var e1 = _tmp.snd;
				var _tmp = f(acc, e2);
				var acc = _tmp.fst; var e2 = _tmp.snd;
				{fst:acc, snd: e.with({eexpr:TFor(v, e1, e2)})};
			case TWhile(e1, e2, flag):
				var _tmp = f(acc, e1);
				var acc = _tmp.fst; var e1 = _tmp.snd;
				var _tmp = f(acc, e2);
				var acc = _tmp.fst; var e2 = _tmp.snd;
				{fst:acc, snd: e.with({eexpr:TWhile(e1, e2, flag)})};
			case TThrow(e1):
				var _tmp = f(acc, e1);
				var acc = _tmp.fst; var e1 = _tmp.snd;
				{fst:acc, snd:e.with({eexpr:TThrow(e1)})};
			case TEnumParameter(e1, ef, i):
				var _tmp = f(acc, e1);
				var acc = _tmp.fst; var e1 = _tmp.snd;
				{fst:acc, snd:e.with({eexpr:TEnumParameter(e1, ef, i)})};
			case TEnumIndex(e1):
				var _tmp = f(acc, e1);
				var acc = _tmp.fst; var e1 = _tmp.snd;
				{fst:acc, snd:e.with({eexpr:TEnumIndex(e1)})};
			case TField(e1, v):
				var _tmp = f(acc, e1);
				var acc = _tmp.fst; var e1 = _tmp.snd;
				{fst:acc, snd:e.with({eexpr:TField(e1, v)})};
			case TParenthesis(e1):
				var _tmp = f(acc, e1);
				var acc = _tmp.fst; var e1 = _tmp.snd;
				{fst:acc, snd:e.with({eexpr:TParenthesis(e1)})};
			case TUnop(op, pre, e1):
				var _tmp = f(acc, e1);
				var acc = _tmp.fst; var e1 = _tmp.snd;
				{fst:acc, snd:e.with({eexpr:TUnop(op, pre, e1)})};
			case TArrayDecl(el):
				var _tmp = foldmap_list(f, acc, el);
				var acc = _tmp.fst; var el = _tmp.snd;
				{fst:acc, snd:e.with({eexpr:TArrayDecl(el)})};
			case TNew(t, pl, el):
				var _tmp = foldmap_list(f, acc, el);
				var acc = _tmp.fst; var el = _tmp.snd;
				{fst:acc, snd:e.with({eexpr:TNew(t, pl ,el)})};
			case TBlock(el):
				var _tmp = foldmap_list(f, acc, el);
				var acc = _tmp.fst; var el = _tmp.snd;
				{fst:acc, snd:e.with({eexpr:TBlock(el)})};
			case TObjectDecl(el):
				var _tmp = foldmap_pairs_objectdecl(f, acc, el);
				var acc = _tmp.fst; var el = _tmp.snd;
				{fst:acc, snd:e.with({eexpr:TObjectDecl(el)})};
			case TCall(e1, el):
				var _tmp = f(acc, e1);
				var acc = _tmp.fst; var e1 = _tmp.snd;
				var _tmp = foldmap_list(f, acc, el);
				var acc = _tmp.fst; var el = _tmp.snd;
				{fst:acc, snd:e.with({eexpr:TCall(e1, el)})};
			case TVar(v, eo):
				var _tmp = foldmap_opt(f, acc, eo);
				var acc = _tmp.fst; var eo = _tmp.snd;
				{fst:acc, snd:e.with({eexpr:TVar(v, eo)})};
			case TFunction(fu):
				var _tmp = f(acc, fu.tf_expr);
				var acc = _tmp.fst; var e1 = _tmp.snd;
				{fst:acc, snd:e.with({eexpr:TFunction(fu.with({tf_expr:e1}))})};
			case TIf(ec, e1, eo):
				var _tmp = f(acc, ec);
				var acc = _tmp.fst; var ec = _tmp.snd;
				var _tmp = f(acc, e1);
				var acc = _tmp.fst; var e1 = _tmp.snd;
				var _tmp = foldmap_opt(f, acc, eo);
				var acc = _tmp.fst; var eo = _tmp.snd;
				{fst:acc, snd:e.with({eexpr:TIf(ec, e1, eo)})};
			case TSwitch (e1, cases, def):
				var _tmp = f(acc, e1);
				var acc = _tmp.fst; var e1 = _tmp.snd;
				var _tmp = List.fold_left(function (arg1, c) {
					var acc = arg1.fst; var cases = arg1.snd; var el = c.values; var e2 = c.e;
					var _tmp = foldmap_list(f, acc, el);
					acc = _tmp.fst; var el = _tmp.snd;
					var _tmp = f(acc, e2);
					acc = _tmp.fst; var e2 = _tmp.snd;
					return {fst:acc, snd: {values:el, e:e2}::cases};
				}, {fst:acc, snd:Tl}, cases);
				acc = _tmp.fst; var cases = _tmp.snd;
				var _tmp = foldmap_opt(f, acc, def);
				acc = _tmp.fst; var def = _tmp.snd;
				{fst:acc, snd:e.with({eexpr:TSwitch(e1, cases, def)})};
			case TTry(e1, catches):
				var _tmp = f(acc, e1);
				var acc = _tmp.fst; var e1 = _tmp.snd;
				var _tmp = foldmap_pairs_catches(f, acc, catches);
				var acc = _tmp.fst; var catches = _tmp.snd;
				{fst:acc, snd:e.with({eexpr:TTry(e1, catches)})};
			case TReturn(eo):
				var _tmp = foldmap_opt(f, acc, eo);
				var acc = _tmp.fst; var eo = _tmp.snd;
				{fst:acc, snd:e.with({eexpr:TReturn(eo)})};
			case TCast(e1, t):
				var _tmp = f(acc, e1);
				var acc = _tmp.fst; var e1 = _tmp.snd;
				{fst:acc, snd:e.with({eexpr:TCast(e1, t)})};
			case TMeta(m, e1):
				var _tmp = f(acc, e1);
				var acc = _tmp.fst; var e1 = _tmp.snd;
				{fst:acc, snd:e.with({eexpr:TMeta(m, e1)})};
		}
	}

	public static function type_constant (basic:core.Type.BasicTypes, c:core.Ast.Constant, p:core.Globals.Pos) : core.Type.TExpr {
		return switch (c) {
			case CInt(s):
				if (s.length > 10 && s.substr(0, 2) == "0x") {
					core.Error.error("Invalid hexadecimal integer", p);
				}
				try {
					core.Type.mk(TConst(TInt(Std.parseInt(s))), basic.tint, p);
				}
				catch (_:Any) {
					core.Type.mk(TConst(TFloat(s)), basic.tfloat, p);
				}
			case CFloat(f):
				core.Type.mk(TConst(TFloat(f)), basic.tfloat, p);
			case CString(s):
				core.Type.mk(TConst(TString(s)), basic.tstring, p);
			case CIdent("true"):
				core.Type.mk(TConst(TBool(true)), basic.tbool, p);
			case CIdent("false"):
				core.Type.mk(TConst(TBool(false)), basic.tbool, p);
			case CIdent("null"):
				core.Type.mk(TConst(TNull), basic.tbool, p);
			case CIdent(t):
				core.Error.error("Invalid constant : "+t, p);
			case CRegexp(_, _):
				core.Error.error("Invalid constant", p);
		}
	}

	public static function type_constant_value (basic:core.Type.BasicTypes, expr:core.Ast.Expr) : core.Type.TExpr {
		var e = expr.expr; var p = expr.pos;
		return switch (e) {
			case EConst(c):
				type_constant(basic, c, p);
			case EParenthesis(e):
				type_constant_value(basic, e);
			case EObjectDecl(el):
				core.Type.mk(TObjectDecl(List.map(function (tmp:core.Ast.ObjectField) : core.Type.TObjectField {
					var e = tmp.expr;
					return {name:tmp.name, pos:tmp.pos, quotes:tmp.quotes, expr:type_constant_value(basic, e)};
				}, el)), TAnon({a_fields:PMap.empty(), a_status:new Ref(Closed)}), p);
			case EArrayDecl(el):
				core.Type.mk(TArrayDecl(List.map(type_constant_value.bind(basic), el)), basic.tarray(core.Type.t_dynamic), p);
			case _:
				core.Error.error("Constant value expected", p);
		}
	}

	public static function for_remap (basic:BasicTypes, v:TVar, e1:TExpr, e2:TExpr, p:core.Globals.Pos) : TExpr {
		var v_ = Type.alloc_var(v.v_name, e1.etype, e1.epos);
		var ev_ = Type.mk(TLocal(v_), e1.etype, e1.epos);
		var t1 = Abstract.follow_with_abstracts(e1.etype);
		var ehasnext = Type.mk(TField(ev_, try { Type.quick_field(t1, "hasNext"); } catch (_:ocaml.Not_found) { core.Error.error(Type.s_type(Type.print_context(), t1) + "has no field hasNext()", p); }), Type.tfun([], basic.tbool), e1.epos);
		var ehasnext = Type.mk(TCall(ehasnext, []), basic.tbool, ehasnext.epos);
		var enext = Type.mk(TField(ev_, Type.quick_field(t1, "next")), Type.tfun([], v.v_type), e1.epos);
		var enext = Type.mk(TCall(enext, []), v.v_type, enext.epos);
		var eassign = Type.mk(TVar(v, Some(enext)), basic.tvoid, p);
		var ebody = Type.concat(eassign, e2);
		return Type.mk(TBlock([
			Type.mk(TVar(v_, Some(e1)), basic.tvoid, e1.epos),
			Type.mk(TWhile(Type.mk(TParenthesis(ehasnext), ehasnext.etype, ehasnext.epos), ebody, NormalWhile), basic.tvoid, e1.epos)
		]), basic.tvoid, p);
	}

	/* -------------------------------------------------------------------------- */
	/* BUILD META DATA OBJECT */
	public static function build_metadata (api:core.Type.BasicTypes, t:core.Type.ModuleType) : Option<core.Type.TExpr> {
		var _tmp = switch (t) {
			case TClassDecl(c):
				var fields = List.map(function (f:core.Type.TClassField) { return {fst:f.cf_name, snd:f.cf_meta}; }, List.append(c.cl_ordered_fields, switch (c.cl_constructor) { case None: Tl; case Some(f): [f.with({cf_name:"_"})]; }));
				var statics = List.map(function (f:core.Type.TClassField) { return {fst:f.cf_name, snd:f.cf_meta}; }, c.cl_ordered_statics);
				{pos:c.cl_pos, meta:Hd({fst:"", snd:c.cl_meta}, Tl), fields:fields, statics:statics};
			case TEnumDecl(e):
				{pos:e.e_pos, meta:Hd({fst:"", snd:e.e_meta}, Tl), fields:List.map(function (n:String) { return {fst:n, snd:PMap.find(n, e.e_constrs).ef_meta}; }, e.e_names), statics:Tl};
			case TTypeDecl(t):
				{pos:t.t_pos, meta:Hd({fst:"", snd:t.t_meta}, Tl), fields:switch(core.Type.follow(t.t_type)) { case TAnon(a): PMap.fold(function (f:core.Type.TClassField, acc) { return {fst:f.cf_name, snd:f.cf_meta} :: acc; }, a.a_fields, Tl); case _: Tl; }, statics:Tl};
			case TAbstractDecl(a):
				{pos:a.a_pos, meta:Hd({fst:"", snd:a.a_meta}, Tl), fields:Tl, statics:Tl};
		}
		var p = _tmp.pos; var meta = _tmp.meta; var fields = _tmp.fields; var statics = _tmp.statics;
		function filter (l:ImmutableList<{fst:String, snd:core.Ast.Metadata}>) {
			var l = List.map(function (tmp:{fst:String, snd:core.Ast.Metadata}) {
				var n = tmp.fst; var ml = tmp.snd;
				return {fst:n, snd:List.filter_map( function (tmp2:core.Ast.MetadataEntry) {
					var m = tmp2.name; var el = tmp2.params; var p = tmp2.pos;
					return switch (m) {
						case Custom(s) if (s.length > 0 && s.charAt(0)!= ":"):
							Some({name:s, params:el, pos:p});
						case _:
							None;
					}
				}, ml)};
			}, l);
			return List.filter(function (tmp:{fst:String, snd:ImmutableList<{name:String, params:ImmutableList<core.Ast.Expr>, pos:core.Globals.Pos}>}) { return tmp.snd != Tl; }, l);
		}
		var meta = filter(meta); var fields = filter(fields); var statics = filter(statics);
		function make_meta_field (ml) : core.Type.TExpr {
			var h:Hashtbl<String, Bool> = Hashtbl.create(0);
			return core.Type.mk( TObjectDecl(List.map(function (tmp:{name:String, params:ImmutableList<core.Ast.Expr>, pos:core.Globals.Pos}) : core.Type.TObjectField {
				var f = tmp.name; var el = tmp.params; var p = tmp.pos;
				if (Hashtbl.mem(h, f)) {
					core.Error.error("Duplicate metadata '"+f+"'", p);
				}
				Hashtbl.add(h, f, true);
				return {name:f, pos:core.Globals.null_pos, quotes:NoQuotes, expr:core.Type.mk(switch (el) { case []: TConst(TNull); case _: TArrayDecl(List.map(type_constant_value.bind(api), el)); }, api.tarray(core.Type.t_dynamic), p)};
			}, ml)), core.Type.t_dynamic, p);
		}
		function make_meta (l:ImmutableList<{fst:String, snd:ImmutableList<{name:String, params:ImmutableList<core.Ast.Expr>, pos:core.Globals.Pos}>}>) : core.Type.TExpr {
			return core.Type.mk(TObjectDecl(List.map(function (tmp) : core.Type.TObjectField {
				var f = tmp.fst; var ml = tmp.snd;
				return {name:f, pos:core.Globals.null_pos, quotes:NoQuotes, expr:make_meta_field(ml)};
			}, l)), core.Type.t_dynamic, p);
		}
		return
		if (meta == Tl && fields == Tl && statics == Tl) {
			None;
		}
		else {
			var meta_obj:ImmutableList<core.Type.TObjectField> = Tl;
			meta_obj = (fields == Tl) ? meta_obj : ({name:"fields", pos:core.Globals.null_pos, quotes:NoQuotes, expr:make_meta(fields)}:core.Type.TObjectField) :: meta_obj;
			meta_obj = (statics == Tl) ? meta_obj : ({name:"statics", pos:core.Globals.null_pos, quotes:NoQuotes, expr:make_meta(statics)}:core.Type.TObjectField) :: meta_obj;
			meta_obj = try {
				({name:"obj", pos:core.Globals.null_pos, quotes:NoQuotes, expr:make_meta_field(List.assoc("", meta))}:core.Type.TObjectField):: meta_obj;
			}
			catch (_:ocaml.Not_found) { meta_obj; }
			Some(core.Type.mk(TObjectDecl(meta_obj), core.Type.t_dynamic, p));
		}
	}
}