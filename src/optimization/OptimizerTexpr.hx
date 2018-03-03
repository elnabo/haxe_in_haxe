package optimization;

import haxe.Int32;
import haxe.Int64;

import ocaml.Hashtbl;
import ocaml.List;

using equals.Equal;
using ocaml.Cloner;

enum T {
	Pure;
	Impure;
	MaybePure;
	ExpectPure(p:core.Globals.Pos);
}

class PurityState {
	public static function get_purity_from_meta (meta:core.Ast.Metadata) : T {
		return
		try {
			switch (core.Meta.get(Pure, meta)) {
				case {params:[{expr:EConst(CIdent(s)), pos:p}]}:
					switch (s) {
						case "true": Pure;
						case "false": Impure;
						case "expect": ExpectPure(p);
						case _: core.Error.error("Unsupported purity value "+s+", expected true or false", p);
					}
				case {params:[]}:
					Pure;
				case {pos:p}:
					core.Error.error("Unsupported purity value", p);
			}
		}
		catch (_:ocaml.Not_found) {
			MaybePure;
		}
	}
	public static function get_purity (c:core.Type.TClass, cf:core.Type.TClassField) : T {
		return switch (get_purity_from_meta(cf.cf_meta)) {
			case Pure: Pure;
			case Impure: Impure;
			case ExpectPure(p): ExpectPure(p);
			case _: get_purity_from_meta(c.cl_meta);
		}
	}
	public static inline function is_pure(c:core.Type.TClass, cf:core.Type.TClassField) : Bool {
		return get_purity(c, cf) == Pure;
	}

	public static function is_pure_field_access (fa:core.Type.TFieldAccess) : Bool {
		return switch (fa) {
			case FInstance(c, _, cf), FClosure(Some({c:c}), cf), FStatic(c, cf): is_pure(c, cf);
			case FAnon(cf), FClosure(None,cf): get_purity_from_meta(cf.cf_meta) == Pure;
			case FEnum(_): true;
			case FDynamic(_): false;
		}
	}

	public static function to_string (t:T) : String {
		return switch (t) {
			case Pure: "pure";
			case Impure: "impure";
			case MaybePure: "maybe";
			case ExpectPure(_): "expect";
		}
	}
}

class OptimizerTexpr {

	/* tells if an expression causes side effects. This does not account for potential null accesses (fields/arrays/ops) */
	public static function has_side_effect(e:core.Type.TExpr) : Bool {
		function loop (e:core.Type.TExpr) : Void {
			switch (e.eexpr) {
				case TConst(_), TLocal(_), TTypeExpr(_), TFunction(_), TIdent(_):
				case TCall({eexpr:TField(e1, fa)}, el) if (PurityState.is_pure_field_access(fa)):
					loop(e1); List.iter(loop, el);
				case TNew(c, _, el) if (switch (c.cl_constructor) { case Some(cf) if (PurityState.is_pure(c, cf)): true; case _: false; }):
					List.iter(loop, el);
				case TNew(_), TCall(_), TBinop((OpAssignOp(_) | OpAssign),_,_), TUnop((OpIncrement|OpDecrement),_,_): throw ocaml.Exit.instance;
				case TReturn(_), TBreak, TContinue, TThrow(_), TCast(_,Some(_)): throw ocaml.Exit.instance;
				case TArray(_), TEnumParameter(_), TEnumIndex(_), TCast(_,None), TBinop(_,_,_), TUnop(_,_,_), TParenthesis(_), TMeta(_,_), TWhile(_,_,_), TFor(_,_,_),
					 TField(_,_), TIf(_,_,_), TTry(_,_), TSwitch(_,_,_), TArrayDecl(_), TBlock(_), TObjectDecl(_), TVar(_):
					core.Type.iter(loop, e);
			}
		}
		return
		try {
			loop(e); false;
		}
		catch (_:ocaml.Exit) {
			true;
		}
	}

	public static function is_exhaustive (e1:core.Type.TExpr) : Bool {
		return switch (e1.eexpr) {
			case TMeta({name:Exhaustive},_): true;
			case TMeta(_, e1), TParenthesis(e1): is_exhaustive(e1);
			case _: false;
		}
	}

	public static function is_read_only_field_access (e:core.Type.TExpr, fa:core.Type.TFieldAccess) : Bool {
		return switch (fa) {
			case FEnum(_): true;
			case FDynamic(_): false;
			case FAnon({cf_kind:Var({v_write:AccNo})}) if (e.eexpr.match(TIdent(_))): true;
			case FInstance(c, _, cf), FStatic(c, cf), FClosure(Some({c:c, params:_}), cf):
				switch (cf.cf_kind) {
					case Method(MethDynamic): false;
					case Method(_): true;
					case Var({v_write:AccNever}) if (!c.cl_interface): true;
					case _: false;
				}
			case FAnon(cf), FClosure(None, cf):
				switch (cf.cf_kind) {
					case Method(MethDynamic): false;
					case Method(_): true;
					case _: false;
				}
		}
	}

	public static function create_affection_checker () : {fst:core.Type.TExpr->Bool, snd:core.Type.TExpr->Void} {
		var modified_locals = new Hashtbl<Int,Bool>();
		function might_be_affected (e:core.Type.TExpr) : Bool {
			function loop (e:core.Type.TExpr) : Void {
				return switch (e.eexpr) {
					case TConst(_), TFunction(_), TTypeExpr(_):
					case TLocal(v) if (Hashtbl.mem(modified_locals,v.v_id)): throw ocaml.Exit.instance;
					case TField(e1, fa) if (!is_read_only_field_access(e1, fa)): throw ocaml.Exit.instance;
					case TCall(_), TNew(_): throw ocaml.Exit.instance;
					case _: core.Type.iter(loop, e);
				}
			}
			return
			try {
				loop(e);
				false;
			}
			catch (_:ocaml.Exit) {
				true;
			}
		}
		function collect_modified_locals(e:core.Type.TExpr) : Void {
			switch(e.eexpr) {
				case TUnop((OpIncrement|OpDecrement), _, {eexpr:TLocal(v)}):
					Hashtbl.add(modified_locals, v.v_id, true);
				case TBinop((OpAssign|OpAssignOp(_)), {eexpr:TLocal(v)}, e2):
					collect_modified_locals(e2);
					Hashtbl.add(modified_locals, v.v_id, true);
				case _:
					core.Type.iter(collect_modified_locals, e);
			}
		}
		return {fst:might_be_affected, snd:collect_modified_locals};
	}

	public static function optimize_binop(e:core.Type.TExpr, op:core.Ast.Binop, e1:core.Type.TExpr, e2:core.Type.TExpr) : core.Type.TExpr {
		function is_float(t:core.Type.T) {
			return switch (core.Type.follow(t)) {
				case TAbstract({ a_path:{a:[], b:"Float"} },_): true;
				case _: false;
			}
		}
		function is_numeric (t:core.Type.T) : Bool {
			return switch (core.Type.follow(t)) {
				case TAbstract({ a_path:{a:[], b:("Float"|"Int")} }, _): true;
				case _: false;
			}
		}
		function check_float (op:Float->Float->Float, f1:Float, f2:Float) : core.Type.TExpr {
			var f = op(f1, f2);
			var fstr = core.Numeric.float_repres(f);
			return switch (ocaml.FloatUtils.classify_float(f)) {
				case FP_nan, FP_infinite: e;
				case _:
					if (Std.parseFloat(fstr) == f) {
						e.with({eexpr:core.Type.TExprExpr.TConst(TFloat(fstr))});
					}
					else {
						e;
					}
			};
		}
		return switch ({f:e1.eexpr, s:e2.eexpr}) {
			case {f:TConst(TInt(0))} if (op == OpAdd && is_numeric(e2.etype)): e2;
			case {f:TConst(TInt(1))} if (op == OpMult): e2;
			case {f:TConst(TFloat(v))} if (op == OpAdd && Std.parseFloat(v) == 0. && is_float(e2.etype)) : e2;
			case {f:TConst(TFloat(v))} if (op == OpMult && Std.parseFloat(v) == 1. && is_float(e2.etype)) : e2;
			case {s:TConst(TInt(0))} if(switch(op) { case OpAdd: is_numeric(e1.etype); case OpSub, OpShr, OpShl: true; case _: false;}): e1; // bits operations might cause overflow
			case {s:TConst(TInt(1))} if (op == OpMult) : e1;
			case {s:TConst(TFloat(v))} if (switch(op) { case OpAdd, OpSub: Std.parseFloat(v) == 0. && is_float(e1.etype); case _: false;}): e1; // bits operations might cause overflow
			case {s:TConst(TFloat(v))} if (op == OpMult && Std.parseFloat(v) == 1. && is_float(e1.etype)): e1;
			case {f:TConst(TNull), s:TConst(TNull)}:
				switch(op) {
					case OpEq:
						e.with({eexpr:core.Type.TExprExpr.TConst(TBool(true))});
					case OpNotEq:
						e.with({eexpr:core.Type.TExprExpr.TConst(TBool(false))});
					case _:
						e;
				}
			case {f:TFunction(_), s:TConst(TNull)}:
				switch(op) {
					case OpEq:
						e.with({eexpr:core.Type.TExprExpr.TConst(TBool(false))});
					case OpNotEq:
						e.with({eexpr:core.Type.TExprExpr.TConst(TBool(true))});
					case _:
						e;
				}
			case {f:TConst(TNull), s:TFunction(_)}:
				switch(op) {
					case OpEq:
						e.with({eexpr:core.Type.TExprExpr.TConst(TBool(false))});
					case OpNotEq:
						e.with({eexpr:core.Type.TExprExpr.TConst(TBool(true))});
					case _:
						e;
				}
			case {f:TConst(TInt(a)), s:TConst(TInt(b))}:
				function opt (f) : core.Type.TExpr {
					return try {
						e.with({eexpr:core.Type.TExprExpr.TConst(TInt(f(a, b)))});
					}
					catch (_:ocaml.Exit) {
						e;
					}
				}
				function check_overflow (f:Int64->Int64->Int64) : core.Type.TExpr {
					return opt(function (a:Int, b:Int) : Int {
						var _a:Int64 = Int64.make(0, a);
						var _b:Int64 = Int64.make(0, b);
						var v:Int64 = f(_a, _b);
						var iv:Int32 = Int64.toInt(v);
						var _tmp1:Int = iv;
						var _tmp2:Int64 = _tmp1;
						if (v != _tmp2) {
							throw ocaml.Exit.instance;
						}
						return iv;
					});
				}
				function ebool (t:Int->Int->Bool) : core.Type.TExpr {
					var _a:Int32 = a;
					var _b:Int32 = b;
					var compare = (_a == _b) ? 0 : (_a > _b) ? 1 : -1;
					return e.with({eexpr:core.Type.TExprExpr.TConst(TBool(t(compare, 0)))});
				}
				switch (op) {
					case OpAdd: check_overflow(function (i:Int64, j:Int64):Int64 {return i + j;});
					case OpSub: check_overflow(function (i:Int64, j:Int64):Int64 {return i - j;});
					case OpMult: check_overflow(function (i:Int64, j:Int64):Int64 {return i * j;});
					case OpDiv: check_float(function (i, j) { return i/j; }, a, b);
					case OpAnd: opt(function (i:Int32, j:Int32):Int32 { return i & j; });
					case OpOr: opt(function (i:Int32, j:Int32):Int32 { return i | j; });
					case OpXor: opt(function (i:Int32, j:Int32):Int32 { return i ^ j; });
					case OpShl: opt(function (i:Int32, j:Int32):Int32 { return i << j; });
					case OpShr: opt(function (i:Int32, j:Int32):Int32 { return i >> j; });
					case OpUShr: opt(function (i:Int32, j:Int32):Int32 { return i >>> j; });
					case OpEq: ebool(function (i, j) { return i == j; });
					case OpNotEq: ebool(function (i, j) { return i != j; });
					case OpGt: ebool(function (i, j) { return i > j; });
					case OpGte: ebool(function (i, j) { return i >= j; });
					case OpLt: ebool(function (i, j) { return i < j; });
					case OpLte: ebool(function (i, j) { return i <= j; });
					case _: e;
				}
			case {f:TConst(ca=(TFloat(_) | TInt(_))), s:TConst(cb=(TFloat(_) | TInt(_)))}:
				var fa:Float = switch (ca) {
					case TFloat(a): Std.parseFloat(a);
					case TInt(a): a;
					case _: trace("Shall not be seen"); throw false;
				}
				var fb:Float = switch(cb) {
					case TFloat(b): Std.parseFloat(b);
					case TInt(b): b;
					case _: trace("Shall not be seen"); throw false;
				}
				function fop (op:Float->Float->Float) : core.Type.TExpr {
					return check_float(op, fa, fb);
				}
				function ebool (t:Int->Int->Bool) : core.Type.TExpr {
					var compare = (fa == fb) ? 0 : (fa > fb) ? 1 : -1;
					return e.with({eexpr:core.Type.TExprExpr.TConst(TBool(t(compare, 0)))});
				}
				return switch (op) {
					case OpAdd: fop(function (i:Float, j:Float) { return i + j; });
					case OpDiv: fop(function (i:Float, j:Float) { return i / j; });
					case OpSub: fop(function (i:Float, j:Float) { return i - j; });
					case OpMult: fop(function (i:Float, j:Float) { return i * j; });
					case OpEq: ebool(function (i:Int, j:Int) { return i == j; });
					case OpNotEq: ebool(function (i:Int, j:Int) { return i != j; });
					case OpGt: ebool(function (i:Int, j:Int) { return i > j; });
					case OpGte: ebool(function (i:Int, j:Int) { return i >= j; });
					case OpLt: ebool(function (i:Int, j:Int) { return i < j; });
					case OpLte: ebool(function (i:Int, j:Int) { return i <= j; });
					case _: e;
				}
			case {f:TConst(TString("")), s:TConst(TString(s))}, {f:TConst(TString(s)),s:TConst(TString(""))} if (op == OpAdd):
				e.with({eexpr:core.Type.TExprExpr.TConst(TString(s))});
			case {f:TConst(TBool(a)), s:TConst(TBool(b))}:
				function ebool (f:Bool->Bool->Bool) : core.Type.TExpr {
					return e.with({eexpr:core.Type.TExprExpr.TConst(TBool(f(a, b)))});
				}
				switch (op) {
					case OpEq: ebool(function (a, b) { return a == b; });
					case OpNotEq: ebool(function (a, b) { return a != b; });
					case OpBoolAnd: ebool(function (a, b) { return a && b; });
					case OpBoolOr: ebool(function (a, b) { return a || b; });
					case _: e;
				}
			case {f:TConst(a), s:TConst(b)} if (op == OpEq || op == OpNotEq):
				function ebool (b:Bool) {
					return e.with({eexpr:core.Type.TExprExpr.TConst(TBool((op==OpEq) ? b : !b))});
				}
				switch {f:a, s:b} {
					case {f:TInt(a), s:TFloat(b)}, {f:TFloat(b), s:TInt(a)}:
						ebool(a == Std.parseFloat(b));
					case _: ebool(a.equals(b));
				}
			case {f:TConst(TBool(a))}:
				switch (op) {
					case OpBoolAnd:
						if (a) { e2; }
						else {
							e.with({eexpr:core.Type.TExprExpr.TConst(TBool(false))});
						}
					case OpBoolOr:
						if (a) {
							e.with({eexpr:core.Type.TExprExpr.TConst(TBool(true))});
						}
						else {
							e2;
						}
					case _: e;
				}
			case {s:TConst(TBool(a))}:
				switch (op) {
					case OpBoolAnd if (a): e1;
					case OpBoolOr if (!a): e1;
					case _: e;
				}
			case {f:TField(_, FEnum(e1, f1)), s:TField(_, FEnum(e2, f2))} if (e1.equals(e2)):
				switch (op) {
					case OpEq:
						e.with({eexpr:core.Type.TExprExpr.TConst(TBool(f1.equals(f2)))});
					case OpNotEq:
						e.with({eexpr:core.Type.TExprExpr.TConst(TBool(!f1.equals(f2)))});
					case _: e;
				}
			case {s:TCall({ eexpr:TField(_,FEnum(_,_))}, _)}, {f:TCall({eexpr:TField(_,FEnum(_,_))}, _)}:
				switch (op) {
					case OpAssign: e;
					case _:
						core.Error.error("You cannot directly compare enums with arguments. Use either 'switch' or 'Type.enumEq'", e.epos);
				}
			case _: e;
		}
	}

	public static function optimize_unop(e:core.Type.TExpr, op:core.Ast.Unop, flag:core.Ast.UnopFlag, esub:core.Type.TExpr) :core.Type.TExpr {
		function is_int (t:core.Type.T) : Bool{
			return switch (core.Type.follow(t)) {
				case TAbstract({a_path:{a:[], b:"Int"}}, _): true;
				case _: false;
			}
		}
		return switch ({f:op, s:esub.eexpr}) {
			case {f:OpNot, s:(TConst(TBool(f)) | TParenthesis({eexpr:TConst(TBool(f))}))}:
				e.with({eexpr:core.Type.TExprExpr.TConst(TBool(!f))});
			case {f:OpNot, s:(TBinop(op, e1, e2) | TParenthesis({eexpr:TBinop(op, e1, e2)}))}:
				var is_int = is_int(e1.etype) && is_int(e2.etype);
				try {
					var op:core.Ast.Binop = switch ({f:is_int, s:op}) {
						case {f:true, s:OpGt}: OpLte;
						case {f:true, s:OpGte}: OpLt;
						case {f:true, s:OpLt}: OpGte;
						case {f:true, s:OpLte}: OpGt;
						case {s:OpEq}: OpNotEq;
						case {s:OpNotEq}: OpEq;
						case _: throw ocaml.Exit.instance;
					}
					e.with({eexpr:core.Type.TExprExpr.TBinop(op, e1, e2)});
				}
				catch (_:ocaml.Exit) {
					e;
				}
			case {f:OpNeg, s:TConst(TInt(i))}:
				e.with({eexpr:core.Type.TExprExpr.TConst(TInt(-1*i))});
			case {f:OpNegBits, s:TConst(TInt(i))}:
				e.with({eexpr:core.Type.TExprExpr.TConst(TInt(~i))});
			case {f:OpNeg, s:TConst(TFloat(f))}:
				var v = 0.0 - Std.parseFloat(f);
				var vstr = core.Numeric.float_repres(v);
				if (v == Std.parseFloat(vstr)) {
					e.with({eexpr:core.Type.TExprExpr.TConst(TFloat(vstr))});
				}
				else {
					e;
				}
			case _: e;
		}
	}
}