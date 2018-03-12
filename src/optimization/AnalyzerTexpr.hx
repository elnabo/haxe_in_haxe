package optimization;

import core.Type.TExprExpr;

import haxe.ds.ImmutableList;
import haxe.ds.Option;

using ocaml.Cloner;

/*
	This module rewrites some expressions to reduce the amount of special cases for subsequent analysis. After analysis
	it restores some of these expressions back to their original form.

	The following expressions are removed from the AST after `apply` has run:
	- OpBoolAnd and OpBoolOr binary operations are rewritten to TIf
	- OpAssignOp on a variable is rewritten to OpAssign
	- Prefix increment/decrement operations are rewritten to OpAssign
	- Postfix increment/decrement operations are rewritten to a TBlock with OpAssign and OpAdd/OpSub
	- `do {} while(true)` is rewritten to `while(true) {}`
	- TWhile expressions are rewritten to `while (true)` with appropriate conditional TBreak
	- TFor is rewritten to TWhile
*/
class TexprFilter {
	public static function apply (com:context.Common.Context, e:core.Type.TExpr) : core.Type.TExpr {
		function loop (e:core.Type.TExpr) {
			return switch (e.eexpr) {
				case TBinop(op=(OpBoolAnd|OpBoolOr), e1, e2):
					var e_then = e2;
					var _tmp = if (op == OpBoolOr) {
						{
							fst:core.Type.mk(TUnop(OpNot, Prefix, e1), com.basic.tbool, e.epos),
							snd:core.Type.mk(TConst(TBool(true)), com.basic.tbool, e.epos)
						};
					}
					else {
						{fst: e1, snd:core.Type.mk(TConst(TBool(false)), com.basic.tbool, e.epos)};
					}
					var e_if = _tmp.fst; var e_else = _tmp.snd;
					loop(core.Type.mk(TIf(e_if, e_then, Some(e_else)), e.etype, e.epos));
				case TBinop(OpAssignOp(op), e1={eexpr:TLocal(_)}, e2):
					var e = e.with({eexpr:TBinop(op, e1, e2)});
					loop(e.with({eexpr:TBinop(OpAssign,e1, e2)}));
				case TUnop(op=(OpIncrement|OpDecrement), flag, e1={eexpr:TLocal(_)}):
					var e_one = core.Type.mk(TConst(TInt(1)), com.basic.tint, e1.epos);
					var e = e.with({eexpr:TBinop(OpAssignOp((op == OpIncrement) ? OpAdd : OpSub), e1, e_one)});
					var e = if (flag == Prefix) {
						e;
					}
					else {
						core.Type.mk(TBlock([
							e.with({eexpr:TBinop(OpAssignOp((op == OpIncrement) ? OpAdd : OpSub), e1, e_one)}),
							e.with({eexpr:TBinop(((op == OpIncrement) ? OpSub : OpAdd), e1, e_one)})
						]), e.etype, e.epos);
					}
					loop(e);
				case TWhile(e1, e2, DoWhile) if (AnalyzerTexpr.is_true_expr(e1)):
					loop(e.with({eexpr:TWhile(e1, e2, NormalWhile)}));
				case TWhile(e1, e2, flag) if (!AnalyzerTexpr.is_true_expr(e1)):
					var p = e.epos;
					var e_break = core.Type.mk(TBreak, core.Type.t_dynamic, p);
					var e_not = core.Type.mk(TUnop(OpNot, Prefix, core.Texpr.Builder.mk_parent(e1)), e1.etype, e1.epos);
					function e_if (eo) { return core.Type.mk(TIf(e_not, e_break, eo), com.basic.tvoid, p); }
					function map_continue (e:core.Type.TExpr) {
						return switch (e.eexpr) {
							case TContinue:
								core.Texpr.duplicate_tvars(e_if(Some(e)));
							case TWhile(_,_,_), TFor(_,_,_):
								e;
							case _:
								core.Type.map_expr(map_continue, e);
						}
					}
					var e2 = (flag == NormalWhile) ? e2 : map_continue(e2);
					var e_if = e_if(None);
					var e_block = (flag == NormalWhile) ? core.Type.concat(e_if, e2) : core.Type.concat(e2, e_if);
					var e_true = core.Type.mk(TConst(TBool(true)), com.basic.tbool, p);
					var e = core.Type.mk(TWhile(core.Texpr.Builder.mk_parent(e_true), e_block, NormalWhile), e.etype, p);
					loop(e);
				case TFor(v, e1, e2):
					var e = core.Texpr.for_remap(com.basic, v, e1, e2, e.epos);
					loop(e);
				case _:
					core.Type.map_expr(loop, e);
			}
		}
		return loop(e);
	}
}

class Purity {
	public static function infer (com:context.Common.Context) : ImmutableList<core.Type.TClassField> {
		trace("TODO Purity.infer");
		throw false;
	}
}

class AnalyzerTexpr {
	public static function is_true_expr(e1:core.Type.TExpr) {
		return switch (e1.eexpr) {
			case TConst(TBool(true)): true;
			case TParenthesis(e1): is_true_expr(e1);
			case _: false;
		}
	}

	public static function map_values (?allow_control_flow:Bool=true, f:core.Type.TExpr->core.Type.TExpr, e:core.Type.TExpr) : {fst:core.Type.TExpr, snd:Option<core.Type.TExpr>} {
		trace("TODO: map_values");
		throw false;
	}

	public static function wrap_meta (s:String, e:core.Type.TExpr) : core.Type.TExpr {
		return core.Type.mk(TMeta({name:Custom(s), params:Tl, pos:e.epos}, e), e.etype, e.epos);
	}

	public static function is_really_unbounded (s:String) : Bool {
		return switch (s) {
			case "`trace", "__int__": false;
			case _: true;
		}
	}

	// ocaml: let r = Str.regexp "^\\([A-Za-z0-9_]\\)+$"
	public static final r = ~/^[A-Za-z0-9_]+$/;
	public static function is_unbound_call_that_might_have_side_effects (s:String, el:ImmutableList<core.Type.TExpr>) : Bool {
		return switch [s, el] {
			case ["__js__", [{eexpr:TConst(TString(s))}]] if (r.match(s)): false;
			case _: true;
		}
	}

}