package optimization;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
using ocaml.Cloner;
using ocaml.List;

class Optimizer {

	// ----------------------------------------------------------------------
	// API OPTIMIZATIONS

	public static function api_inline (ctx:context.Typecore.Typer, c:core.Type.TClass, field:String, params:ImmutableList<core.Type.TExpr>, p:core.Globals.Pos) : Option<core.Type.TExpr> {
		trace("Optimizer.api_inline");
		throw false;
	}

	// ----------------------------------------------------------------------
	// INLINING

	public static function type_inline (ctx:context.Typecore.Typer, cf:core.Type.TClassField, f:core.Type.TFunc, ethis:core.Type.TExpr, params:ImmutableList<core.Type.TExpr>, tret:core.Type.T, config:Option<Any>, p:core.Globals.Pos, ?self_calling_closure:Bool=false, force:Bool) : Option<core.Type.TExpr> {
		trace("Optimizer.type_inline");
		throw false;
	}

	// ----------------------------------------------------------------------
	// LOOPS
	public static function optimize_for_loop (ctx:context.Typecore.Typer, i:String, pi:core.Globals.Pos, e1:core.Type.TExpr, e2:core.Ast.Expr, p:core.Globals.Pos) : Option<core.Type.TExpr> {
		trace("Optimizer.optimize_for_loop");
		throw false;
	}

	public static function optimize_for_loop_iterator (ctx:context.Typecore.Typer, v:core.Type.TVar, e1:core.Type.TExpr, e2:core.Type.TExpr, p:core.Globals.Pos) : core.Type.TExpr {
		trace("Optimizer.optimize_for_loop_iterator");
		throw false;
	}

	// ----------------------------------------------------------------------
	// SANITIZE

	/*
		makes sure that when an AST get generated to source code, it will not
		generate expressions that evaluate differently. It is then necessary to
		add parenthesises around some binary expressions when the AST does not
		correspond to the natural operand priority order for the platform
	*/
	// this is the standard C++ operator precedence, which is also used by both JS and PHP
	public static function standard_precedence (op:core.Ast.Binop) : {fst:Int, snd:Bool} {
		var left = true; var right = false;
		return switch(op) {
			case OpIn: {fst:4, snd:right};
			case OpMult, OpDiv, OpMod: {fst:5, snd:left};
			case OpAdd, OpSub: {fst:6, snd:left};
			case OpShl, OpShr, OpUShr: {fst:7, snd:left};
			case OpLt, OpLte, OpGt, OpGte: {fst:8, snd:left};
			case OpEq, OpNotEq: {fst:9, snd:left};
			case OpAnd: {fst:10, snd:left};
			case OpXor: {fst:11, snd:left};
			case OpOr: {fst:12, snd:left};
			case OpInterval: {fst:13, snd:right}; // haxe specific
			case OpBoolAnd: {fst:14, snd:left};
			case OpBoolOr: {fst:15, snd:left};
			case OpArrow: {fst:16, snd:left};
			case OpAssignOp(OpAssign): {fst:18, snd:right}; // mimics ?:
			case OpAssign, OpAssignOp(_): {fst:19, snd:right};
		}
	}

	public static function need_parent (e:core.Type.TExpr) : Bool {
		return switch(e.eexpr) {
			case TConst(_), TLocal(_), TArray(_), TField(_), TEnumParameter(_), TEnumIndex(_), TParenthesis(_), TCall(_), TNew(_), TTypeExpr(_), TObjectDecl(_), TArrayDecl(_), TIdent(_) : false;
			case TCast(e,None), TMeta(_,e): need_parent(e);
			case TCast(_), TThrow(_), TReturn(_), TTry(_), TSwitch(_), TFor(_), TIf(_), TWhile(_), TBinop(_), TContinue, TBreak, TBlock(_), TVar(_), TFunction(_), TUnop(_): true;
		}
	}

	public static function sanitize_expr (com:context.Common.Context, e:core.Type.TExpr) : core.Type.TExpr {
		function parent (e:core.Type.TExpr) : core.Type.TExpr {
			return switch (e.eexpr) {
				case TParenthesis(_): e;
				case _: core.Type.mk(TParenthesis(e), e.etype, e.epos);
			}
		}
		function block (e:core.Type.TExpr) : core.Type.TExpr {
			return switch (e.eexpr) {
				case TBlock(_): e;
				case _: core.Type.mk(TBlock([e]), e.etype, e.epos);
			}
		}
		function complex (e:core.Type.TExpr) : core.Type.TExpr {
			// complex expressions are the one that once generated to source consists in several expressions
			return switch (e.eexpr) {
				case TVar(_), // needs to be put into blocks
					TFor(_), // a temp var is needed for holding iterator
					TCall({eexpr:TIdent("__js__")}, _): // we never know
					block(e);
				case _: e;
			}
		}
		// tells if the printed expresssion ends with an if without else
		function has_if (e:core.Type.TExpr) : Bool {
			return switch (e.eexpr) {
				case TIf(_, _, None): true;
				case TWhile(_, e, NormalWhile): has_if(e);
				case TFor(_, _, e): has_if(e);
				case _: false;
			}
		}
		return switch (e.eexpr) {
			case TConst(TNull):
				if (com.config.pf_static && !core.Type.is_nullable(e.etype)) {
					function loop (t:core.Type.T) {
						return switch(core.Type.follow(t)) {
							case TMono(_): // in these cases the null will cast to default value
							case TFun(_): // this is a bit a particular case, maybe flash-specific actually
							// TODO: this should use get_underlying_type, but we do not have access to Codegen here.
							case TAbstract(a,tl) if (!core.Meta.has(CoreType, a.a_meta)): loop(core.Type.apply_params(a.a_params, tl, a.a_this));
							case _: com.error("On static platforms, null can't be used as basic type " + core.Type.s_type(core.Type.print_context(), e.etype), e.epos);
						}
					}
					loop(e.etype);
				}
				e;
			case TBinop(op, e1, e2):
				function swap (op1:core.Ast.Binop, op2:core.Ast.Binop) : Bool {
					var _tmp = standard_precedence(op1);
					var p1 = _tmp.fst; var left1 = _tmp.snd;
					var p2 = standard_precedence(op2).fst;
					return left1 && p1 <= p2;
				}
				function loop(ee:core.Type.TExpr, left:Bool) {
					return switch (ee.eexpr) {
						case TBinop(op2, _, _):
							if (left) {
								!swap(op2, op);
							}
							else {
								swap(op, op2);
							}
						case TIf(_):
							if (left) {
								!swap(OpAssignOp(OpAssign),  op);
							}
							else {
								swap(op, OpAssignOp(OpAssign));
							}
						case TCast(e, None), TMeta(_, e): loop(e, left);
						case _: false;
					}
				}
				var e1 = (loop(e1, true)) ? parent(e1) : e1;
				var e2 = (loop(e2, false)) ? parent(e2) : e2;
				var _e = e.clone();
				_e.eexpr = TBinop(op, e1, e2);
				_e;
			case TUnop(op, mode, e1):
				function loop(ee:core.Type.TExpr) : core.Type.TExpr {
					return switch (ee.eexpr) {
						case TBinop(_), TIf(_), TUnop(_): parent(e1);
						case TCast(e, None), TMeta(_, e): loop(e);
						case _: e1;
					}
				}
				var _e = e.clone();
				_e.eexpr = TUnop(op, mode, loop(e1));
				_e;
			case TIf(e1, e2, eelse):
				var e1 = parent(e1);
				var e2 = if ((eelse != None && has_if(e2)) || switch (e2.eexpr) {case TIf(_): true; case _: false; }) {
					block(e2);
				}
				else { complex(e2); }
				var eelse = switch (eelse) { case None: None; case Some(e): Some(complex(e));}
				var _e = e.clone();
				_e.eexpr = TIf(e1, e2, eelse);
				_e;
			case TWhile(e1, e2, flag):
				var e1 = parent(e1);
				var e2 = complex(e2);
				var _e = e.clone();
				_e.eexpr = TWhile(e1, e2, flag);
				_e;
			case TFor(v, e1, e2):
				var e2 = complex(e2);
				var _e = e.clone();
				_e.eexpr = TFor(v, e1, e2);
				_e;
			case TFunction(f):
				var f = switch (f.tf_expr.eexpr) {
					case TBlock(_): f;
					case _:
						var _f = f.clone();
						_f.tf_expr = block(f.tf_expr);
						_f;
				}
				var _e = e.clone();
				_e.eexpr = TFunction(f);
				_e;
			case TCall(e2, args):
				if (need_parent(e2)) {
					var _e = e.clone();
					_e.eexpr = TCall(parent(e2), args);
					_e;
				}
				else {
					e;
				}
			case TEnumParameter(e2, ef, i):
				if (need_parent(e2)) {
					var _e = e.clone();
					_e.eexpr = TEnumParameter(parent(e2), ef, i);
					_e;
				}
				else {
					e;
				}
			case TEnumIndex(e2):
				if (need_parent(e2)) {
					var _e = e.clone();
					_e.eexpr = TEnumIndex(parent(e2));
					_e;
				}
				else {
					e;
				}
			case TField(e2, f):
				if (need_parent(e2)) {
					var _e = e.clone();
					_e.eexpr = TField(parent(e2), f);
					_e;
				}
				else {
					e;
				}
			case TArray(e1, e2):
				if (need_parent(e1)) {
					var _e = e.clone();
					_e.eexpr = TArray(parent(e1), e2);
					_e;
				}
				else {
					e;
				}
			case TTry(e1, catches):
				var e1 = block(e1);
				var catches = List.map(function (c) { return {v:c.v, e:block(c.e)};}, catches);
				var _e = e.clone();
				_e.eexpr = TTry(e1, catches);
				_e;
			case TSwitch(e1, cases, def):
				var e1 = parent(e1);
				var cases = List.map(function (c) { return {values:c.values, e:complex(c.e)};}, cases);
				var def = switch (def) {
					case None: None;
					case Some(e): Some(complex(e));
				}
				var _e = e.clone();
				_e.eexpr = TSwitch(e1, cases, def);
				_e;
			case _: e;
			// case _: throw true;
		}

	}

	public static function reduce_expr (com:Any, e:core.Type.TExpr) : core.Type.TExpr {
		return switch (e.eexpr) {
			case TSwitch (_,cases,_):
				List.iter(function (_c) {
					List.iter (function (e:core.Type.TExpr) {
						switch (e.eexpr) {
							case TCall({eexpr:TField(_, FEnum(_))}, _): core.Error.error("Not-constant enum in switch cannot be matched", e.epos);
							case _:
						}
					}, _c.values);
				}, cases);
				e;
			case TBlock(l):
				switch (List.rev(l)) {
					case []: e;
					case ec::l:
						var l:ImmutableList<core.Type.TExpr> = l;
						// remove all no-ops : not-final constants in blocks
						function _f(e:core.Type.TExpr) {
							return switch (e.eexpr) {
								case TConst(_):false;
								case TBlock([]), TObjectDecl([]): false;
								case _: true;
							}
						}
						switch (List.filter(_f, l)) {
							case []: ec;
							case l:
								var _e = e.clone();
								_e.eexpr = TBlock(ocaml.List.rev(ec :: l));
								_e;
						}
				}
			case TParenthesis(ec):
				var _ec = ec.clone();
				_ec.epos = e.epos;
				_ec;
			case TTry(e,[]):
				e;
			case _:
				e;
		}
	}

	public static function sanitize (com:context.Common.Context, e:core.Type.TExpr) : core.Type.TExpr {
		return sanitize_expr(com, reduce_expr(com, core.Type.map_expr(sanitize.bind(com), e)));
	}

	// ----------------------------------------------------------------------
	// REDUCE

	public static function reduce_control_flow (ctx:context.Typecore.Typer, e:core.Type.TExpr) : core.Type.TExpr {
		return switch (e.eexpr) {
			case TIf({eexpr:TConst(TBool(t))}, e1, e2):
				if (t) { e1; }
				else {
					switch (e2) {
						case None:
							var _e = e.clone();
							_e.eexpr = TBlock([]);
							_e;
						case Some(e): e;
					}
				}
			case TWhile({eexpr:TConst(TBool(false))}, sub, flag):
				switch (flag) {
					case NormalWhile:
						var _e = e.clone();
						_e.eexpr = TBlock([]); // erase sub
						_e;
					case DoWhile: e; // we cant remove while since sub can contain continue/break
				}
			case TSwitch(e1, cases, def):
				var e = switch (core.Texpr.skip(e1)) {
					case e1={eexpr:TConst(ct)}:
						function loop(cases:ImmutableList<{values:ImmutableList<core.Type.TExpr>, e:core.Type.TExpr}>) : core.Type.TExpr {
							return switch (cases) {
								case ({values:el, e:e})::cases:
									var cases:ImmutableList<{values:ImmutableList<core.Type.TExpr>, e:core.Type.TExpr}> = cases;
									if (List.exists(core.Texpr.equal.bind(e1), el)) {
										e;
									}
									else {
										loop(cases);
									}
								case []:
									switch (def) {
										case None: e;
										case Some(e): e;
									}
							}
						}
						loop(cases);
					case _: e;
				}
				e;
			case TBinop(op, e1, e2):
				optimization.OptimizerTexpr.optimize_binop(e, op, e1, e2);
			case TUnop(op, flag, esub):
				optimization.OptimizerTexpr.optimize_unop(e, op, flag, esub);
			case TCall(f={eexpr:TField(o, FClosure(c, cf))}, el):
				var fmode:core.Type.TFieldAccess = switch (c) {
					case None: FAnon(cf);
					case Some({c:c, params:tl}): FInstance(c, tl, cf);
				}
				var _e = e.clone();
				var _f = f.clone();
				_f.eexpr = TField(o, fmode);
				_e.eexpr = TCall(_f, el);
				_e;
			case _: e;
		}
	}

	public static function reduce_loop (ctx:context.Typecore.Typer, e:core.Type.TExpr) : core.Type.TExpr {
		var e = core.Type.map_expr(reduce_loop.bind(ctx), e);
		return sanitize_expr(ctx.com, switch (e.eexpr) {
			case TCall(e1, el):
				switch (core.Texpr.skip(e1)) {
					case ef={eexpr:TFunction(func)}:
						var cf = core.Type.mk_field("", ef.etype, e.epos, core.Globals.null_pos);
						var ethis = core.Type.mk(TConst(TThis), core.Type.t_dynamic, e.epos);
						var rt = switch (core.Type.follow(ef.etype)) {
							case TFun({ret:rt}): rt;
							case _: trace("Shall not be seen"); throw false;
						}
						var inl = try {
							type_inline(ctx, cf, func, ethis, el, rt, None, e.epos, true, false);
						}
						catch (err:core.Error) {
							switch (err.msg) {
								case Custom(_): None;
								case _: throw err;
							}
						}
						switch (inl) {
							case None: reduce_expr(ctx, e);
							case Some(e): reduce_expr(ctx, e);
						}
					case {eexpr:TField({eexpr:TTypeExpr(TClassDecl(c))}, field)}:
						switch (api_inline(ctx, c, core.Type.field_name(field), el, e.epos)) {
							case None: reduce_expr(ctx, e);
							case Some(e): reduce_expr(ctx, e);
						}
					case _: reduce_expr(ctx, e);
				}
			case _:
				reduce_expr(ctx, reduce_control_flow(ctx, e));
		});
	}

	public static function reduce_expression (ctx:context.Typecore.Typer, e:core.Type.TExpr) : core.Type.TExpr {
		return (ctx.com.foptimize) ? reduce_loop(ctx, e) : e;
	}

	public static function make_constant_expression (ctx:context.Typecore.Typer, ?concat_strings:Bool=false, e:core.Type.TExpr) : Option<core.Type.TExpr> {
		var e = reduce_loop(ctx, e);
		return switch (e.eexpr) {
			case TConst(_): Some(e);
			case TBinop(op=(OpAdd|OpSub|OpMult|OpDiv|OpMod|OpShl|OpShr|OpUShr|OpOr|OpAnd|OpXor), e1, e2):
				switch ({fst:make_constant_expression(ctx, e1), snd:make_constant_expression(ctx, e2)}) {
					case {fst:Some({eexpr:TConst(TString(s1))}), snd:Some({eexpr:TConst(TString(s2))})} if (concat_strings):
						Some(core.Type.mk(TConst(TString(s1 + s2)), ctx.com.basic.tstring, core.Ast.punion(e1.epos, e2.epos)));
					case {fst:Some(e1), snd:Some(e2)}:
						Some(core.Type.mk(TBinop(op, e1, e2), e.etype, e.epos));
					case _: None;
				}
			case TUnop(op=(OpNeg|OpNegBits), Prefix, e1):
				switch (make_constant_expression(ctx, e1)) {
					case Some(e1): Some(core.Type.mk(TUnop(op, Prefix, e1), e.etype, e.epos));
					case None: None;
				}
			case TCast(e1, None):
				switch (make_constant_expression(ctx, e1)) {
					case None: None;
					case Some(e1):
						var _e = e.clone();
						_e.eexpr = TCast(e1, None);
						Some(_e);
				}
			case TParenthesis(e1):
				switch (make_constant_expression(ctx, e1)) {
					case None: None;
					case Some(e1):
						var _e = e.clone();
						_e.eexpr = TParenthesis(e1);
						Some(_e);
				}
			case TMeta(m, e1):
				switch (make_constant_expression(ctx, e1)) {
					case None: None;
					case Some(e1):
						var _e = e.clone();
						_e.eexpr = TMeta(m, e1);
						Some(_e);
				}
			case TTypeExpr(_): Some(e);
			// try to inline static function calls
			// Disabled for now, see #4254.
			/* | TCall ({ etype = TFun(_,ret); eexpr = TField (_,FStatic (c,cf)) },el) ->
				(try
					let func = match cf.cf_expr with Some ({eexpr = TFunction func}) -> func | _ -> raise Not_found in
					let ethis = mk (TConst TThis) t_dynamic e.epos in
					let inl = (try type_inline ctx cf func ethis el ret None e.epos false with Error (Custom _,_) -> None) in
					(match inl with
					| None -> None
					| Some e -> make_constant_expression ctx e)
				with Not_found -> None) */
			case _: None;
		}
	}

	// ----------------------------------------------------------------------
	// INLINE CONSTRUCTORS
	// This version is disabled by default, use -D old-constructor-inline to use this

	/*
		First pass :
		We will look at local variables in the form   var v = new ....
		we only capture the ones which have constructors marked as inlined
		then we make sure that these locals are no more referenced except for fields accesses

		Second pass :
		We replace the variables by their fields lists, and the corresponding fields accesses as well
	*/

	// ----------------------------------------------------------------------
	// COMPLETION
	public static function optimize_completion_expr (e:core.Ast.Expr) : core.Ast.Expr {
		trace("Optimizer.optimize_completion_expr");
		throw false;
	}
}