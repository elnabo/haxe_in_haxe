package optimization;

import core.Type.TExprExpr;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
import ocaml.List;
import ocaml.PMap;
import ocaml.Ref;

using ocaml.Cloner;

import optimization.AnalyzerTypes.BasicBlock;
import optimization.AnalyzerTypes.BlockKind;
import optimization.AnalyzerTypes.Graph;


class AnalyzerTexprTransformer {
	/*
		Transforms an expression to a graph, and a graph back to an expression. This module relies on TexprFilter being
		run first.

		The created graph is intact and can immediately be transformed back to an expression, or used for analysis first.
	*/
	public static function func (ctx:AnalyzerTypes.Analyzer_context, bb:BasicBlock, tf:core.Type.TFunc, t:core.Type.T, p:core.Globals.Pos) : {func:BasicBlock, exit:BasicBlock} {
		trace("TODO AnalyzerTexprTransformer.func");
		var g = ctx.graph;
		function create_node (kind:BlockKind, t:core.Type.T, p:core.Globals.Pos) {
			var bb = Graph.create_node(g, kind, t, p);
			bb.bb_loop_groups = ctx.loop_stack;
			return bb;
		}
		var bb_root = create_node(BKFunctionBegin(tf), tf.tf_expr.etype, tf.tf_expr.epos);
		var bb_exit = create_node(BKFunctionEnd, tf.tf_expr.etype, tf.tf_expr.epos);
		Graph.add_function(g, tf, t, p, bb_root);
		BasicBlock.add_cfg_edge(bb, bb_root, CFGFunction);
		function make_block_meta(b:BasicBlock) {
			var e = core.Type.mk(TConst(TInt(b.bb_id)), ctx.com.basic.tint, b.bb_pos);
			return AnalyzerTexpr.wrap_meta(":block", e);
		}
		var bb_breaks = new Ref<ImmutableList<BasicBlock>>(Tl);
		var bb_continue = new Ref<Option<BasicBlock>>(None);
		var b_try_stack = new Ref<ImmutableList<BasicBlock>>(Tl);
		function begin_loop (bb_loop_pre:BasicBlock, bb_continue_:BasicBlock) : Void->ImmutableList<BasicBlock> {
			var old = {fst:bb_breaks.get(), snd:bb_continue.get()};
			bb_breaks.set(Tl);
			bb_continue.set(Some(bb_continue_));
			var id = ctx.loop_counter;
			g.g_loops = PMap.add(id, bb_loop_pre, g.g_loops);
			ctx.loop_stack = id :: ctx.loop_stack;
			bb_continue_.bb_loop_groups = id :: bb_continue_.bb_loop_groups;
			ctx.loop_counter = id + 1;
			return function () {
				var breaks = bb_breaks.get();
				bb_breaks.set(old.fst);
				bb_continue.set(old.snd);
				ctx.loop_stack = List.tl(ctx.loop_stack);
				return breaks;
			}
		}
		function begin_try (b:BasicBlock) : Void->Void {
			b_try_stack.set(b::b_try_stack.get());
			return function () {
				b_try_stack.set(List.tl(b_try_stack.get()));
			}
		}
		function add_terminator (bb:BasicBlock, e:core.Type.TExpr) : BasicBlock {
			BasicBlock.add_texpr(bb, e);
			Graph.close_node(g, bb);
			return g.g_unreachable;
		}
		function check_unbound_call(s:String, el:ImmutableList<core.Type.TExpr>) : Void {
			if (s == "$ref") {
				switch (el) {
					case [{eexpr:TLocal(v)}]: v.v_capture = true;
					case _:
				}
			}
			if (AnalyzerTexpr.is_unbound_call_that_might_have_side_effects(s, el)) { ctx.has_unbound = true; }
		}
		function no_void (t:core.Type.T, p:core.Globals.Pos) {
			if (core.Type.ExtType.is_void(core.Type.follow(t))) {
				core.Error.error("Cannot use void as a value", p);
			}
		}
		function push_name (s:String) : Void->Void {
			ctx.name_stack = s :: ctx.name_stack;
			return function () { ctx.name_stack = List.tl(ctx.name_stack); }
		}

		var value:(bb:BasicBlock, e:core.Type.TExpr)->{bb:BasicBlock, e:core.Type.TExpr};
		var ordered_value_list:(bb:BasicBlock, el:ImmutableList<core.Type.TExpr>)->{bb:BasicBlock, el:ImmutableList<core.Type.TExpr>};
		var bind_to_temp:(?v:Option<core.Type.TVar>, bb:BasicBlock, sequential:Bool, e:core.Type.TExpr)->{bb:BasicBlock, e:core.Type.TExpr};
		var declare_var_and_assign:(bb:BasicBlock, v:core.Type.TVar, e:core.Type.TExpr, p:core.Globals.Pos)->BasicBlock;
		var call:(bb:BasicBlock, e:core.Type.TExpr, e1:core.Type.TExpr, el:ImmutableList<core.Type.TExpr>)->{bb:BasicBlock, e:core.Type.TExpr};
		var array_assign_op:(bb:BasicBlock, op:core.Ast.Binop, e:core.Type.TExpr, ea:core.Type.TExpr, e1:core.Type.TExpr, e2:core.Type.TExpr, e3:core.Type.TExpr)->{bb:BasicBlock, e:core.Type.TExpr};
		var field_assign_op:(bb:BasicBlock, op:core.Ast.Binop, e:core.Type.TExpr, ef:core.Type.TExpr, e1:core.Type.TExpr, fa:core.Type.TFieldAccess, e2:core.Type.TExpr)->{bb:BasicBlock, e:core.Type.TExpr};
		var block_element:(bb:BasicBlock, e:core.Type.TExpr)->BasicBlock;
		var block_element_plus:(bb:BasicBlock, ep:{fst:core.Type.TExpr, snd:Option<core.Type.TExpr>}, f:core.Type.TExpr->core.Type.TExpr)->BasicBlock;
		function value_(bb:BasicBlock, e:core.Type.TExpr) : {bb:BasicBlock, e:core.Type.TExpr} {
			return switch(e.eexpr) {
				case TLocal(_), TIdent(_):
					{bb:bb, e:e};
				case TBinop(OpAssign, e1={eexpr:TLocal(v)}, e2):
					{bb:block_element(bb, e), e:e1};
				case TBlock([e1]):
					value(bb, e1);
				case TBlock(_), TIf(_,_,_), TSwitch(_,_,_), TTry(_,_):
					bind_to_temp(bb, false, e);
				case TCall({eexpr:TIdent(s)}, el) if (AnalyzerTexpr.is_really_unbounded(s)):
					check_unbound_call(s, el);
					{bb:bb, e:e};
				case TCall(e1, el):
					call(bb, e, e1, el);
				case TBinop(OpAssignOp(op), ea={eexpr:TArray(e1, e2)}, e3):
					array_assign_op(bb, op, e, ea, e1, e2, e3);
				case TBinop(OpAssignOp(op), ef={eexpr:TField(e1, fa)}, e2):
					field_assign_op(bb, op, e, ef, e1, fa, e2);
				case TBinop(op=(OpAssign|OpAssignOp(_)), e1, e2):
					var _tmp = value(bb, e1);
					var bb = _tmp.bb; var e1 = _tmp.e;
					var _tmp = value(bb, e2);
					var bb = _tmp.bb; var e2 = _tmp.e;
					{bb:bb, e:e.with({eexpr:TBinop(op, e1, e2)})};
				case TBinop(op, e1, e2):
					var _tmp = switch (ordered_value_list(bb, [e1, e2])) {
						case {bb:bb, el:[e1, e2]}: {bb:bb, e1:e1, e2:e2};
						case _: trace("Shall not be seen"); std.Sys.exit(255); throw false;
					}
					var bb = _tmp.bb; var e1 = _tmp.e1; var e2 = _tmp.e2;
					{bb:bb, e:e.with({eexpr:TBinop(op, e1, e2)})};
				case TUnop(op, flag, e1):
					var _tmp = value(bb, e1);
					var bb = _tmp.bb; var e1 = _tmp.e;
					{bb:bb, e:e.with({eexpr:TUnop(op, flag, e1)})};
				case TArrayDecl(el):
					var _tmp = ordered_value_list(bb, el);
					var bb = _tmp.bb; var el = _tmp.el;
					{bb:bb, e:e.with({eexpr:TArrayDecl(el)})};
				case TObjectDecl(fl):
					var el = List.map(function (of) { return of.expr; }, fl);
					var _tmp = ordered_value_list(bb, el);
					var bb = _tmp.bb; var el = _tmp.el;
					{bb:bb, e:e.with({eexpr:TObjectDecl(List.map2(function (of:core.Type.TObjectField, e:core.Type.TExpr) { return of.with({expr:e}); }, fl, el))})};
				case TField({eexpr:TTypeExpr(_)},fa):
					{bb:bb, e:e};
				case TField(e1, fa):
					var _tmp = value(bb, e1);
					var bb = _tmp.bb; var e1 = _tmp.e;
					{bb:bb, e:e.with({eexpr:TField(e1, fa)})};
				case TArray(e1,e2):
					var _tmp = switch (ordered_value_list(bb, [e1, e2])) {
						case {bb:bb, el:[e1, e2]}: {bb:bb, e1:e1, e2:e2};
						case _: trace("Shall not be seen"); std.Sys.exit(255); throw false;
					}
					var bb = _tmp.bb; var e1 = _tmp.e1; var e2 = _tmp.e2;
					{bb:bb, e:e.with({eexpr:TArray(e1, e2)})};
				case TMeta(m, e1):
					var _tmp = value(bb, e1);
					var bb = _tmp.bb; var e1 = _tmp.e;
					{bb:bb, e:e.with({eexpr:TMeta(m, e1)})};
				case TParenthesis(e1):
					var _tmp = value(bb, e1);
					var bb = _tmp.bb; var e1 = _tmp.e;
					{bb:bb, e:e.with({eexpr:TParenthesis(e1)})};
				case TCast(e1, mto):
					var _tmp = value(bb, e1);
					var bb = _tmp.bb; var e1 = _tmp.e;
					{bb:bb, e:e.with({eexpr:TCast(e1, mto)})};
				case TNew(c, tl, el):
					var _tmp = ordered_value_list(bb, el);
					var bb = _tmp.bb; var el = _tmp.el;
					{bb:bb, e:e.with({eexpr:TNew(c, tl, el)})};
				case TEnumParameter(e1, ef, ei):
					var _tmp = value(bb, e1);
					var bb = _tmp.bb; var e1 = _tmp.e;
					{bb:bb, e:e.with({eexpr:TEnumParameter(e1, ef, ei)})};
				case TEnumIndex(e1):
					var _tmp = value(bb, e1);
					var bb = _tmp.bb; var e1 = _tmp.e;
					{bb:bb, e:e.with({eexpr:TEnumIndex(e1)})};
				case TFunction(tf):
					var _tmp = func(ctx, bb, tf, e.etype, e.epos);
					var bb_func = _tmp.func; var bb_func_end = _tmp.exit;
					var e_fun = core.Type.mk(TConst(TString("fun")), core.Type.t_dynamic, p);
					var econst = core.Type.mk(TConst(TInt(bb_func.bb_id)), ctx.com.basic.tint, e.epos);
					var ec = core.Type.mk(TCall(e_fun, [econst]), core.Type.t_dynamic, p);
					var bb_next = create_node(BKNormal, bb.bb_type, bb.bb_pos);
					BasicBlock.add_cfg_edge(bb, bb_next, CFGGoto);
					BasicBlock.set_syntax_edge(bb, SEMerge(bb_next));
					Graph.close_node(g, bb);
					BasicBlock.add_cfg_edge(bb_func_end, bb_next, CFGGoto);
					{bb:bb_next, e:ec};
				/*| TTypeExpr(TClassDecl {cl_kind = KAbstractImpl a}) when not (Meta.has Meta.RuntimeValue a.a_meta) ->
					error "Cannot use abstract as value" e.epos*/
				case TConst(_), TTypeExpr(_):
					{bb:bb, e:e};
				case TThrow(_), TReturn(_), TBreak, TContinue:
					var bb = block_element(bb, e);
					{bb:bb, e:core.Type.mk(TConst(TNull), core.Type.t_dynamic, e.epos)};
				case TVar(_), TFor(_,_,_), TWhile(_,_,_):
					core.Error.error("Cannot use this expression as value", e.epos);
			}
		}
		value = function (bb:BasicBlock, e:core.Type.TExpr) : {bb:BasicBlock, e:core.Type.TExpr} {
			var _tmp = value_(bb, e);
			var bb = _tmp.bb; var e = _tmp.e;
			no_void(e.etype, e.epos);
			return {bb:bb, e:e};
		}
		ordered_value_list = function (bb, el) {
			var _tmp = OptimizerTexpr.create_affection_checker();
			var might_be_affected = _tmp.fst; var collect_modified_locals = _tmp.snd;
			function can_be_optimized(e:core.Type.TExpr) : Bool {
				return switch (e.eexpr) {
					case TBinop(_,_,_), TArray(_,_), TCall(_,_): true;
					case TParenthesis(e1): can_be_optimized(e1);
					case _: false;
				}
			}
			var el = List.fold_left(function (tmp:{fst:Bool, snd:ImmutableList<{fst:Bool, snd:Bool, trd:core.Type.TExpr}>}, e:core.Type.TExpr) {
				var had_side_effect = tmp.fst; var acc = tmp.snd;
				return
				if (had_side_effect) {
					{fst:true, snd:{fst:OptimizerTexpr.has_side_effect(e) || might_be_affected(e), snd:can_be_optimized(e), trd:e}::acc};
				}
				else {
					var had_side_effect = OptimizerTexpr.has_side_effect(e);
					if (had_side_effect) { collect_modified_locals(e); }
					var opt = can_be_optimized(e);
					{fst:had_side_effect||opt, snd:{fst:false, snd:opt, trd:e}::acc};
				}
			}, {fst:false, snd:Tl}, List.rev(el)).snd;
			var _tmp = List.fold_left(function (tmp1, tmp2) {
				var bb = tmp1.bb; var acc = tmp1.el;
				var aff = tmp2.fst; var opt = tmp2.snd; var e = tmp2.trd;
				var _tmp = (aff || opt) ? bind_to_temp(None, bb, aff, e) : value(bb, e);
				var bb = _tmp.bb; var value = _tmp.e;
				return {bb:bb, el:value::acc};
			}, {bb:bb, el:Tl}, el);
			var bb = _tmp.bb; var values = _tmp.el;
			return {bb:bb, el:List.rev(values)};
		}
		bind_to_temp = function (?v, bb, sequential, e) {
			if (v == null) { v = None; }
			function is_probably_not_affected (e:core.Type.TExpr, e1:core.Type.TExpr, fa:core.Type.TFieldAccess) : Bool {
				return switch (fa) {
					case FAnon(cf), FInstance(_,_,cf), FStatic(_,cf), FClosure(_, cf) if (cf.cf_kind.match(Method(MethNormal))): true;
					case FStatic(_,{cf_kind:Method(MethDynamic)}): false;
					case FEnum(_): true;
					case FDynamic(("cca"|"__Index"|"__s")): true; // This is quite retarded, but we have to deal with this somehow...
					case _:
						switch [core.Type.follow(e.etype), core.Type.follow(e1.etype)] {
							case [TFun(_), TInst(_,_)]: false;
							case [TFun(_), _]: true; // We don't know what's going on here, don't create a temp var (see #5082).
							case _: false;
						}
				}
			}
			function loop (fl:ImmutableList<core.Type.TExpr->core.Type.TExpr>, e:core.Type.TExpr) : {fl:ImmutableList<core.Type.TExpr->core.Type.TExpr>, e:core.Type.TExpr} {
				return switch(e.eexpr) {
					case TField(e1, fa) if (is_probably_not_affected(e, e1, fa)):
						loop(function (e_:core.Type.TExpr) { return e.with({eexpr:TField(e_, fa)}); }::fl, e1);
					case TField(e1, fa):
						var fa:core.Type.TFieldAccess = switch (fa) {
							case FInstance(c, tl, cf={cf_kind:Method(_)}): FClosure(Some({c:c, params:tl}), cf);
							case _: fa;
						}
						{fl:fl, e:e.with({eexpr:TField(e1, fa)})};
					case _:
						{fl:fl, e:e};
				}
			}
			var _tmp = loop(Tl, e);
			var fl = _tmp.fl; var e = _tmp.e;
			function loop2 (e:core.Type.TExpr) : String {
				return switch(e.eexpr) {
					case TLocal(v): v.v_name;
					case TArray(e1,_), TField(e1,_), TParenthesis(e1), TCast(e1, None), TMeta(_,e1): loop2(e1);
					case _:
						switch (ctx.name_stack) {
							case s :: _: s;
							case []: ctx.temp_var_name;
						}
				}
			}
			var v = switch (v) { case Some(v): v; case None: core.Type.alloc_var(loop2(e), e.etype, e.epos); }
			switch (ctx.com.platform) {
				case Cpp if (sequential && !(context.Common.defined(ctx.com, Cppia))):
				case _: v.v_meta = [{name:CompilerGenerated, params:Tl, pos:e.epos}];
			}
			var bb = declare_var_and_assign(bb, v, e, e.epos);
			var e = e.with({eexpr:TLocal(v)});
			var e = List.fold_left(function (e, f) { return f(e); }, e, fl);
			return {bb:bb, e:e};
		}
		declare_var_and_assign = function (bb, v, e, p) {
			/* TODO: this section shouldn't be here because it can be handled as part of the normal value processing */
			function loop(bb:BasicBlock, e:core.Type.TExpr) : {bb:BasicBlock, e:core.Type.TExpr} {
				return switch (e.eexpr) {
					case TParenthesis(e1): loop(bb, e1);
					case TBlock(el):
						function loop2 (bb:BasicBlock, el:ImmutableList<core.Type.TExpr>) {
							return switch (el) {
								case [e]: {bb:bb, e:e};
								case e1 :: el:
									var bb = block_element(bb, e1);
									if (bb == g.g_unreachable) { throw ocaml.Exit.instance; }
									loop2(bb, el);
								case []: trace("Shall not be seen"); std.Sys.exit(255); throw false;
							}
						}
						var _tmp = loop2(bb, el);
						var bb = _tmp.bb; var e = _tmp.e;
						loop(bb, e);
					case _:
						{bb:bb, e:e};
				}
			}
			function generate (bb:BasicBlock, e:core.Type.TExpr) {
				no_void(v.v_type, p);
				var ev = core.Type.mk(TLocal(v), v.v_type, p);
				var was_assigned = new Ref(false);
				function assign(e:core.Type.TExpr) {
					if (!was_assigned.get()) {
						was_assigned.set(true);
						BasicBlock.add_texpr(bb, core.Type.mk(TVar(v, None), ctx.com.basic.tvoid, ev.epos));
					}
					return core.Type.mk(TBinop(OpAssign, ev, e), ev.etype, ev.epos);
				}
				var close = push_name(v.v_name);
				var bb = try {
					block_element_plus(bb, AnalyzerTexpr.map_values(assign, e), function (e:core.Type.TExpr) { return core.Type.mk(TVar(v, Some(e)), ctx.com.basic.tvoid, ev.epos)});
				}
				catch (_:ocaml.Exit) {
					var _tmp = value(bb, e);
					var bb = _tmp.bb; var e = _tmp.e;
					BasicBlock.add_texpr(bb, core.Type.mk(TVar(v, Some(e)), ctx.com.basic.tvoid, ev.epos));
					bb;
				}
				close();
				return bb;
			}
			return
			try {
				var _tmp = loop(bb, e);
				var bb = _tmp.bb; var e = _tmp.e;
				generate(bb, e);
			}
			catch (_:ocaml.Exit) {
				g.g_unreachable;
			}
		}
		block_element_plus = function (bb, ep, f) {
			var e = ep.fst; var efinal = ep.snd;
			var bb = block_element(bb, e);
			var bb = switch(efinal) {
				case None: bb;
				case Some(e): block_element(bb, f(e));
			}
			return bb;
		}
		throw false;
	}

	public static function from_tfunction (ctx:AnalyzerTypes.Analyzer_context, tf:core.Type.TFunc, t:core.Type.T, p:core.Globals.Pos) {
		var g = ctx.graph;
		var _tmp = func(ctx, g.g_root, tf, t, p);
		var bb_func = _tmp.func; var bb_exit = _tmp.exit;
		ctx.entry = bb_func;
		Graph.close_node(g, g.g_root);
		g.g_exit = bb_exit;
	}
}