package optimization;

import haxe.Int32;
import haxe.Int64;

using equals.Equal;
using ocaml.Cloner;

class OptimizerTexpr {

	public static function optimize_binop(e:core.Type.TExpr, op:core.Ast.Binop, e1:core.Type.TExpr, e2:core.Type.TExpr) : core.Type.TExpr {
		function is_float(t:core.Type.T) {
			return switch (core.Type.follow(t)) {
				case TAbstract({ a_path: path },_) if (path.a.length == 0 && path.b == "Float"): true;
				case _: false;
			}
		}
		function is_numeric (t:core.Type.T) : Bool {
			return switch (core.Type.follow(t)) {
				case TAbstract({ a_path : path }, _) if (path.a.length == 0 && (path.b == "Float" || path.b == "Int")): true;
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
						var _e = e.clone();
						_e.eexpr = TConst(TFloat(fstr));
						_e;
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
						var _e = e.clone(); _e.eexpr = TConst(TBool(true));
						_e;
					case OpNotEq:
						var _e = e.clone(); _e.eexpr = TConst(TBool(false));
						_e;
					case _:
						e;
				}
			case {f:TFunction(_), s:TConst(TNull)}:
				switch(op) {
					case OpEq:
						var _e = e.clone(); _e.eexpr = TConst(TBool(false));
						_e;
					case OpNotEq:
						var _e = e.clone(); _e.eexpr = TConst(TBool(true));
						_e;
					case _:
						e;
				}
			case {f:TConst(TNull), s:TFunction(_)}:
				switch(op) {
					case OpEq:
						var _e = e.clone(); _e.eexpr = TConst(TBool(false));
						_e;
					case OpNotEq:
						var _e = e.clone(); _e.eexpr = TConst(TBool(true));
						_e;
					case _:
						e;
				}
			case {f:TConst(TInt(a)), s:TConst(TInt(b))}:
				function opt (f) : core.Type.TExpr {
					return try {
						var _e = e.clone();
						_e.eexpr = TConst(TInt(f(a, b)));
						_e;
					}
					catch (_:ocaml.Exit) {
						e;
					}
				}
				function check_overflow (f:Int64->Int64->Int64) : core.Type.TExpr {
					return opt(function (a:Int, b:Int) : Int {
						var _a:Int64 = Int64.make(a, a);
						var _b:Int64 = Int64.make(b, b);
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
					var _e = e.clone();
					_e.eexpr = TConst(TBool(t(compare, 0)));
					return _e;
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
					case _: throw false;
				}
				var fb:Float = switch(cb) {
					case TFloat(b): Std.parseFloat(b);
					case TInt(b): b;
					case _: throw false;
				}
				function fop (op:Float->Float->Float) : core.Type.TExpr {
					return check_float(op, fa, fb);
				}
				function ebool (t:Int->Int->Bool) : core.Type.TExpr {
					var _e = e.clone();
					var compare = (fa == fb) ? 0 : (fa > fb) ? 1 : -1;
					_e.eexpr = TConst(TBool(t(compare, 0)));
					return _e;
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
				var _e = e.clone();
				_e.eexpr = TConst(TString(s));
				_e;
			case {f:TConst(TBool(a)), s:TConst(TBool(b))}:
				function ebool (f:Bool->Bool->Bool) : core.Type.TExpr {
					var _e = e.clone();
					_e.eexpr = TConst(TBool(f(a, b)));
					return _e;
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
					var _e = e.clone();
					_e.eexpr = TConst(TBool((op==OpEq) ? b : !b));
					return _e;
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
							var _e = e.clone();
							e.eexpr = TConst(TBool(false));
							_e;
						}
					case OpBoolOr:
						if (a) {
							var _e = e.clone();
							e.eexpr = TConst(TBool(true));
							_e;
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
						var _e = e.clone();
						_e.eexpr = TConst(TBool(f1.equals(f2)));
						_e;
					case OpNotEq:
						var _e = e.clone();
						_e.eexpr = TConst(TBool(!f1.equals(f2)));
						_e;
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
				case TAbstract({a_path:path}, _) if (path.a.length==0 && path.b == "Int"): true;
				case _: false;
			}
		}
		return switch ({f:op, s:esub.eexpr}) {
			case {f:OpNot, s:(TConst(TBool(f)) | TParenthesis({eexpr:TConst(TBool(f))}))}:
				var _e = e.clone();
				_e.eexpr = TConst(TBool(!f));
				_e;
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
					var _e = e.clone();
					_e.eexpr = TBinop(op, e1, e2);
					_e;
				}
				catch (_:ocaml.Exit) {
					e;
				}
			case {f:OpNeg, s:TConst(TInt(i))}:
				var _e = e.clone();
				_e.eexpr = TConst(TInt(-1*i));
				_e;
			case {f:OpNegBits, s:TConst(TInt(i))}:
				var _e = e.clone();
				_e.eexpr = TConst(TInt(~i));
				_e;
			case {f:OpNeg, s:TConst(TFloat(f))}:
				var v = 0.0 - Std.parseFloat(f);
				var vstr = core.Numeric.float_repres(v);
				if (v == Std.parseFloat(vstr)) {
					var _e = e.clone();
					_e.eexpr = TConst(TFloat(vstr));
					_e;
				}
				else {
					e;
				}
			case _: e;
		}
	}
}