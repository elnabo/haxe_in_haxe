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
		var block_element_plus:(bb:BasicBlock, ep:{fst:core.Type.TExpr, snd:Option<core.Type.TExpr>}, f:core.Type.TExpr->core.Type.TExpr)->BasicBlock;
		var block_element_value:(bb:BasicBlock, e:core.Type.TExpr, f:core.Type.TExpr->core.Type.TExpr)->BasicBlock;
		var call:(bb:BasicBlock, e:core.Type.TExpr, e1:core.Type.TExpr, el:ImmutableList<core.Type.TExpr>)->{bb:BasicBlock, e:core.Type.TExpr};
		var array_assign_op:(bb:BasicBlock, op:core.Ast.Binop, e:core.Type.TExpr, ea:core.Type.TExpr, e1:core.Type.TExpr, e2:core.Type.TExpr, e3:core.Type.TExpr)->{bb:BasicBlock, e:core.Type.TExpr};
		var field_assign_op:(bb:BasicBlock, op:core.Ast.Binop, e:core.Type.TExpr, ef:core.Type.TExpr, e1:core.Type.TExpr, fa:core.Type.TFieldAccess, e2:core.Type.TExpr)->{bb:BasicBlock, e:core.Type.TExpr};
		var block_element:(bb:BasicBlock, e:core.Type.TExpr)->BasicBlock;
		var block_el:(bb:BasicBlock, e:ImmutableList<core.Type.TExpr>)->BasicBlock;
		var block:(bb:BasicBlock, e:core.Type.TExpr)->BasicBlock;
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
				case TCall({eexpr:TIdent(s)}, el) if (AnalyzerTexpr.is_really_unbound(s)):
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
					block_element_plus(bb, AnalyzerTexpr.map_values(assign, e), function (e:core.Type.TExpr) { return core.Type.mk(TVar(v, Some(e)), ctx.com.basic.tvoid, ev.epos); });
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
		block_element_value = function (bb, e, f) {
			var _tmp = AnalyzerTexpr.map_values(f, e);
			var e = _tmp.fst; var efinal = _tmp.snd;
			return block_element_plus(bb, {fst:e, snd:efinal}, f);
		}
		call = function (bb, e, e1, el) {
			var bb = new Ref(bb);
			function check (e:core.Type.TExpr, t:core.Type.T) {
				return switch (e.eexpr) {
					case TLocal(v) if (AnalyzerTexpr.is_ref_type(t)):
						v.v_capture = true;
						e;
					case _:
						if (AnalyzerTexpr.is_asvar_type(t)) {
							var v = core.Type.alloc_var("tmp", t, e.epos);
							var _tmp = bind_to_temp(Some(v), bb.get(), false, e);
							var bb_ = _tmp.bb; var e = _tmp.e;
							bb.set(bb_);
							e;
						}
						else {
							e;
						}
				}
			}
			var el = codegen.Codegen.UnificationCallback.check_call(check, el, e1.etype);
			var _tmp = ordered_value_list(bb.get(), e1 :: el);
			var bb = _tmp.bb; var el = _tmp.el;
			return switch (el) {
				case e1 :: el: {bb:bb, e:e.with({eexpr:TCall(e1, el)})};
				case _: trace("Shall not be seen"); std.Sys.exit(255); throw false;
			}
		}
		array_assign_op = function (bb, op, e, ea, e1, e2, e3) {
			var _tmp = bind_to_temp(bb, false, e1);
			var bb = _tmp.bb; var e1 = _tmp.e;
			var _tmp = bind_to_temp(bb, false, e2);
			var bb = _tmp.bb; var e2 = _tmp.e;
			var ea = ea.with({eexpr:TArray(e1, e2)});
			var _tmp = bind_to_temp(bb, false, ea);
			var bb = _tmp.bb; var e4 = _tmp.e;
			var _tmp = bind_to_temp(bb, false, e3);
			var bb = _tmp.bb; var e3 = _tmp.e;
			var eop = e.with({eexpr:TBinop(op, e4, e3)});
			BasicBlock.add_texpr(bb, e.with({eexpr:TBinop(OpAssign, ea, eop)}));
			return {bb:bb, e:ea};
		}
		field_assign_op = function (bb, op, e, ef, e1, fa, e2) {
			var _tmp = switch (fa) {
				case FInstance(c,_,_), FClosure(Some({c:c}),_) if (AnalyzerTexpr.is_stack_allocated(c)):
					{bb:bb, e:e1};
				case _: bind_to_temp(bb, false, e1);
			}
			var bb = _tmp.bb; var e1 = _tmp.e;
			var ef = ef.with({eexpr:TField(e1, fa)});
			var _tmp = bind_to_temp(bb, false, ef);
			var bb = _tmp.bb; var e3 = _tmp.e;
			var _tmp = bind_to_temp(bb, false, e2);
			var bb = _tmp.bb; var e2 = _tmp.e;
			var eop = e.with({eexpr:TBinop(op, e3, e2)});
			BasicBlock.add_texpr(bb, e.with({eexpr:TBinop(OpAssign, ef, eop)}));
			return {bb:bb, e:ef};
		}
		block_element = function (bb, e) {
			return switch (e.eexpr) {
				// variables
				case TVar(v, None):
					BasicBlock.add_texpr(bb, e);
					bb;
				case TVar(v, Some(e1)):
					declare_var_and_assign(bb, v, e1, e.epos);
				case TBinop(OpAssign, e1={eexpr:TLocal(v)}, e2):
					function assign (e:core.Type.TExpr) {
						return core.Type.mk(TBinop(OpAssign, e1, e), e.etype, e.epos);
					}
					var close = push_name(v.v_name);
					var bb = try {
						block_element_value(bb, e2, assign);
					}
					catch (_:ocaml.Exit) {
						var _tmp = value(bb, e2);
						var bb = _tmp.bb; var e2 = _tmp.e;
						BasicBlock.add_texpr(bb, e.with({eexpr:TBinop(OpAssign, e1, e2)}));
						bb;
					}
					close();
					bb;
				// branching
				case TMeta({name:MergeBlock}, {eexpr:TBlock(el)}):
					block_el(bb, el);
				case TBlock(el):
					var bb_sub = create_node(BKSub, e.etype, e.epos);
					BasicBlock.add_cfg_edge(bb, bb_sub, CFGGoto);
					Graph.close_node(g, bb);
					var bb_sub_next = block_el(bb_sub, el);
					if (bb_sub_next != g.g_unreachable) {
						var bb_next = create_node(BKNormal, bb.bb_type, bb.bb_pos);
						BasicBlock.set_syntax_edge(bb, SESubBlock(bb_sub, bb_next));
						BasicBlock.add_cfg_edge(bb_sub_next, bb_next, CFGGoto);
						Graph.close_node(g, bb_sub_next);
						bb_next;
					}
					else {
						BasicBlock.set_syntax_edge(bb, SEMerge(bb_sub));
						Graph.close_node(g, bb_sub_next);
						bb_sub_next;
					}
				case TIf(e1, e2, None):
					var _tmp = bind_to_temp(bb, false, e1);
					var bb = _tmp.bb; var e1 = _tmp.e;
					return
					if (bb == g.g_unreachable) {
						bb;
					}
					else {
						var bb_then = create_node(BKConditional, e2.etype, e2.epos);
						BasicBlock.add_texpr(bb, AnalyzerTexpr.wrap_meta(":cond-branch", e1));
						BasicBlock.add_cfg_edge(bb, bb_then, CFGCondBranch(core.Type.mk(TConst(TBool(true)), ctx.com.basic.tbool, e2.epos)));
						var bb_then_next = block(bb_then, e2);
						var bb_next = create_node(BKNormal, bb.bb_type, bb.bb_pos);
						BasicBlock.set_syntax_edge(bb, SEIfThen(bb_then, bb_next, e.epos));
						BasicBlock.add_cfg_edge(bb, bb_next, CFGCondElse);
						Graph.close_node(g, bb);
						BasicBlock.add_cfg_edge(bb_then_next, bb_next, CFGGoto);
						Graph.close_node(g, bb_then_next);
						bb_next;
					}
				case TIf(e1, e2, Some(e3)):
					var _tmp = bind_to_temp(bb, false, e1);
					var bb = _tmp.bb; var e1 = _tmp.e;
					return
					if (bb == g.g_unreachable) {
						bb;
					}
					else {
						var bb_then = create_node(BKConditional, e2.etype, e2.epos);
						var bb_else = create_node(BKConditional, e3.etype, e3.epos);
						BasicBlock.add_texpr(bb, AnalyzerTexpr.wrap_meta(":cond-branch", e1));
						BasicBlock.add_cfg_edge(bb, bb_then, CFGCondBranch(core.Type.mk(TConst(TBool(true)), ctx.com.basic.tbool, e2.epos)));
						BasicBlock.add_cfg_edge(bb, bb_else, CFGCondElse);
						Graph.close_node(g, bb);
						var bb_then_next = block(bb_then, e2);
						var bb_else_next = block(bb_else, e3);
						return
						if (bb_then_next == g.g_unreachable && bb_else_next == g.g_unreachable) {
							BasicBlock.set_syntax_edge(bb, SEIfThenElse(bb_then, bb_else, g.g_unreachable, e.etype, e.epos));
							g.g_unreachable;
						}
						else {
							var bb_next = create_node(BKNormal, bb.bb_type, bb.bb_pos);
							BasicBlock.set_syntax_edge(bb, SEIfThenElse(bb_then, bb_else, bb_next, e.etype, e.epos));
							BasicBlock.add_cfg_edge(bb_then_next, bb_next, CFGGoto);
							BasicBlock.add_cfg_edge(bb_else_next, bb_next, CFGGoto);
							Graph.close_node(g, bb_then_next);
							Graph.close_node(g, bb_else_next);
							bb_next;
						}
					}
				case TSwitch(e1, cases, edef):
					var is_exhaustive = edef != None || OptimizerTexpr.is_exhaustive(e1);
					var _tmp = bind_to_temp(bb, false, e1);
					var bb = _tmp.bb; var e1 = _tmp.e;
					BasicBlock.add_texpr(bb, AnalyzerTexpr.wrap_meta(":cond-branch", e1));
					var reachable = new Ref<ImmutableList<BasicBlock>>(Tl);
					function make_case (e:core.Type.TExpr) {
						var bb_case = create_node(BKConditional, e.etype, e.epos);
						var bb_case_next = block(bb_case, e);
						if (bb_case_next != g.g_unreachable) {
							reachable.set(bb_case_next :: reachable.get());
						}
						Graph.close_node(g, bb_case_next);
						return bb_case;
					}
					var cases = List.map(function (c) {
						var el = c.values; var e = c.e;
						var bb_case = make_case(e);
						List.iter(function (e:core.Type.TExpr) { BasicBlock.add_cfg_edge(bb, bb_case, CFGCondBranch(e)); }, el);
						return {el:el, block:bb_case};
					}, cases);
					var def = switch (edef) {
						case None: None;
						case Some(e):
							var bb_case = make_case(e);
							BasicBlock.add_cfg_edge(bb, bb_case, CFGCondElse);
							Some(bb_case);
					}
					return
					if (is_exhaustive && reachable.get() == Tl) {
						BasicBlock.set_syntax_edge(bb, SESwitch(cases, def, g.g_unreachable, e.epos));
						Graph.close_node(g, bb);
						g.g_unreachable;
					}
					else {
						var bb_next = create_node(BKNormal, bb.bb_type, bb.bb_pos);
						if (!is_exhaustive) { BasicBlock.add_cfg_edge(bb, bb_next, CFGGoto); }
						List.iter(function (bb) { BasicBlock.add_cfg_edge(bb, bb_next, CFGGoto); }, reachable.get());
						BasicBlock.set_syntax_edge(bb, SESwitch(cases, def, bb_next, e.epos));
						Graph.close_node(g, bb);
						bb_next;
					}
				case TWhile(e1, e2, NormalWhile):
					var bb_loop_pre = create_node(BKNormal, e1.etype, e1.epos);
					BasicBlock.add_cfg_edge(bb, bb_loop_pre, CFGGoto);
					BasicBlock.set_syntax_edge(bb, SEMerge(bb_loop_pre));
					Graph.close_node(g, bb);
					var bb_loop_head = create_node(BKLoopHead, e1.etype, e1.epos);
					BasicBlock.add_cfg_edge(bb_loop_pre, bb_loop_head, CFGGoto);
					var close = begin_loop(bb, bb_loop_head);
					var bb_loop_body = create_node(BKNormal, e2.etype, e2.epos);
					var bb_loop_body_next = block(bb_loop_body, e2);
					var bb_breaks = close();
					var bb_next = if (bb_breaks == Tl) {
						/* The loop appears to be infinite, let's assume that something within it throws.
							Otherwise DCE's mark-pass won't see its body and removes everything. */
						BasicBlock.add_cfg_edge(bb_loop_body_next, bb_exit, CFGMaybeThrow);
						g.g_unreachable;
					}
					else {
						create_node(BKNormal, bb.bb_type, bb.bb_pos);
					}
					List.iter(function (bb) { BasicBlock.add_cfg_edge(bb, bb_next, CFGGoto); }, bb_breaks);
					BasicBlock.set_syntax_edge(bb_loop_pre, SEWhile(bb_loop_head, bb_loop_body, bb_next));
					Graph.close_node(g, bb_loop_pre);
					BasicBlock.add_texpr(bb_loop_pre, e.with({eexpr:TWhile(e1, make_block_meta(bb_loop_body), NormalWhile)}));
					BasicBlock.add_cfg_edge(bb_loop_body_next, bb_loop_head, CFGGoto);
					Graph.close_node(g, bb_loop_body_next);
					Graph.close_node(g, bb_loop_head);
					bb_next;
				case TTry(e1, catches):
					var bb_try = create_node(BKNormal, e1.etype, e1.epos);
					var bb_exc = create_node(BKException, core.Type.t_dynamic, e.epos);
					BasicBlock.add_cfg_edge(bb, bb_try, CFGGoto);
					var close = begin_try(bb_exc);
					var bb_try_next = block(bb_try, e1);
					close();
					// We always want to keep catch-blocks, so let's add a pseudo CFG edge if it's unreachable.
					if (bb_exc.bb_incoming == Tl) { BasicBlock.add_cfg_edge((bb_try_next == g.g_unreachable) ? bb_try : bb_try_next, bb_exc, CFGMaybeThrow); }
					var is_reachable = new Ref(!(bb_try_next == g.g_unreachable));
					var catches = List.map(function (c) {
						var v = c.v; var e = c.e;
						var bb_catch = create_node(BKCatch(v), e.etype, e.epos);
						BasicBlock.add_cfg_edge(bb_exc, bb_catch, CFGGoto);
						var bb_catch_next = block(bb_catch, e);
						is_reachable.set(is_reachable.get() || !(bb_catch_next == g.g_unreachable));
						return {fst:v, snd:bb_catch, trd:bb_catch_next};
					}, catches);
					var bb_next = if (is_reachable.get()) { create_node(BKNormal, bb.bb_type, bb.bb_pos); } else { g.g_unreachable; }
					var catches = List.map(function (c) {
						var v = c.fst; var bb_catch = c.snd; var bb_catch_next = c.trd;
						if (bb_catch_next != g.g_unreachable) { BasicBlock.add_cfg_edge(bb_catch_next, bb_next, CFGGoto); }
						Graph.close_node(g, bb_catch_next);
						return {v:v, block:bb_catch};
					}, catches);
					BasicBlock.set_syntax_edge(bb, SETry(bb_try, bb_exc, catches, bb_next, e.epos));
					if (bb_try_next != g.g_unreachable) { BasicBlock.add_cfg_edge(bb_try_next, bb_next, CFGGoto); }
					Graph.close_node(g, bb_try_next);
					Graph.close_node(g, bb_exc);
					Graph.close_node(g, bb);
					bb_next;
				// control flow
				case TReturn(None):
					BasicBlock.add_cfg_edge(bb, bb_exit, CFGGoto);
					add_terminator(bb, e);
				case TReturn(Some(e1)) if (core.Type.ExtType.is_void(core.Type.follow(e1.etype))):
					var bb = block_element(bb, e1);
					block_element(bb, core.Type.mk(TReturn(None), core.Type.t_dynamic, e.epos));
				case TReturn(Some(e1)):
					try {
						function mk_return (e1:core.Type.TExpr) { return core.Type.mk(TReturn(Some(e1)), core.Type.t_dynamic, e1.epos); }
						block_element_value(bb, e1, mk_return);
					}
					catch (_:ocaml.Exit) {
						var _tmp = value(bb, e1);
						var bb = _tmp.bb; var e1 = _tmp.e;
						BasicBlock.add_cfg_edge(bb, bb_exit, CFGGoto);
						add_terminator(bb, e.with({eexpr:TReturn(Some(e1))}));
					}
				case TBreak:
					bb_breaks.set(bb::bb_breaks.get());
					add_terminator(bb, e);
				case TContinue:
					switch (bb_continue.get()) {
						case Some(bb_continue): BasicBlock.add_cfg_edge(bb, bb_continue, CFGGoto);
						case _: trace("Shall not be seen"); std.Sys.exit(255);
					}
					add_terminator(bb, e);
				case TThrow(e1):
					try {
						function mk_throw(e1:core.Type.TExpr) {
							return core.Type.mk(TThrow(e1), core.Type.t_dynamic, e.epos);
						}
						block_element_value(bb, e1, mk_throw);
					}
					catch (_:ocaml.Exit) {
						var _tmp = value(bb, e1);
						var bb = _tmp.bb; var e1 = _tmp.e;
						switch (b_try_stack.get()) {
							case []: BasicBlock.add_cfg_edge(bb, bb_exit, CFGGoto);
							case _: List.iter(function (bb_exc) { BasicBlock.add_cfg_edge(bb, bb_exc, CFGGoto); }, b_try_stack.get());
						}
						add_terminator(bb, e.with({eexpr:TThrow(e1)}));
					}
				// side_effects
				case TCall({eexpr:TIdent(s)}, el) if (AnalyzerTexpr.is_really_unbound(s)):
					check_unbound_call(s, el);
					BasicBlock.add_texpr(bb, e);
					bb;
				case TCall(e1, el):
					var _tmp = call(bb, e, e1, el);
					var bb = _tmp.bb; var e = _tmp.e;
					BasicBlock.add_texpr(bb, e);
					bb;
				case TNew(c, tl, el):
					var _tmp = ordered_value_list(bb, el);
					var bb = _tmp.bb; var el = _tmp.el;
					BasicBlock.add_texpr(bb, e.with({eexpr:TNew(c, tl, el)}));
					bb;
				case TCast(e1, Some(mt)):
					var _tmp = value(bb, e1);
					var bb = _tmp.bb; var e1 = _tmp.e; // is let b, e1 in code at current commit, seems weird
					BasicBlock.add_texpr(bb, e.with({eexpr:TCast(e1, Some(mt))}));
					bb;
				case TBinop(OpAssignOp(op), ea={eexpr:TArray(e1, e2)}, e3):
					var bb = array_assign_op(bb, op, e, ea, e1, e2, e3).bb;
					bb;
				case TBinop(OpAssignOp(op), ef={eexpr:TField(e1, fa)}, e2):
					var bb = field_assign_op(bb, op, e, ef, e1, fa, e2).bb;
					bb;
				case TBinop(OpAssign, ea={eexpr:TArray(e1, e2)}, e3):
					var _tmp = switch (ordered_value_list(bb, [e1, e2, e3])) {
						case {bb:bb, el:[e1, e2, e3]}: {bb:bb, e1:e1, e2:e2, e3:e3};
						case _: trace("Shall not be seen"); std.Sys.exit(255); throw false;
					}
					var bb = _tmp.bb; var e1 = _tmp.e1; var e2 = _tmp.e2; var e3 = _tmp.e3;
					BasicBlock.add_texpr(bb, e.with({eexpr:TBinop(OpAssign, ea.with({eexpr:TArray(e1, e2)}), e3)}));
					bb;
				case TBinop(op=(OpAssign|OpAssignOp(_)), e1, e2):
					var _tmp = value(bb, e1);
					var bb = _tmp.bb; var e1 = _tmp.e;
					var _tmp = value(bb, e2);
					var bb = _tmp.bb; var e2 = _tmp.e;
					BasicBlock.add_texpr(bb, e.with({eexpr:TBinop(op, e1, e2)}));
					bb;
				case TUnop(op=(OpIncrement|OpDecrement), flag, e1):
					var _tmp = value(bb, e1);
					var bb = _tmp.bb; var e1 = _tmp.e;
					BasicBlock.add_texpr(bb, e.with({eexpr:TUnop(op, flag, e1)}));
					bb;
				case TLocal(_) if (!ctx.config.local_dce):
					BasicBlock.add_texpr(bb, e);
					bb;
				// no-side-effect
				case TEnumParameter(_), TEnumIndex(_), TFunction(_), TConst(_), TTypeExpr(_), TLocal(_), TIdent(_):
					bb;
				// no-side-effect composites
				case TParenthesis(e1), TMeta(_, e1), TCast(e1, None), TField(e1, _), TUnop(_, _, e1):
					block_element(bb, e1);
				case TArray(e1, e2), TBinop(_,e1, e2):
					var bb = block_element(bb, e1);
					block_element(bb, e2);
				case TArrayDecl(el):
					block_el(bb, el);
				case TObjectDecl(fl):
					block_el(bb, List.map(function (f) { return f.expr; }, fl));
				case TFor(_,_,_), TWhile(_,_,DoWhile):
					trace("Shall not be seen"); std.Sys.exit(255); throw false;
			}
		}
		block_el = function (bb, el) {
			return switch (b_try_stack.get()) {
				case []:
					function loop(bb:BasicBlock, el:ImmutableList<core.Type.TExpr>) {
						return switch(el) {
							case []: bb;
							case e :: el:
								var bb = block_element(bb, e);
								(bb == g.g_unreachable) ? bb : loop(bb, el);
						}
					}
					loop(bb, el);
				case var bbl:
					function loop(bb:BasicBlock, el:ImmutableList<core.Type.TExpr>) {
						return switch (el) {
							case []: bb;
							case e :: el:
								var bb = if (!AnalyzerTexpr.can_throw(e)) {
									block_element(bb, e);
								}
								else {
									var bb_ = create_node(BKNormal, e.etype, e.epos);
									BasicBlock.add_cfg_edge(bb, bb_, CFGGoto);
									List.iter(function (bb_exc) { BasicBlock.add_cfg_edge(bb, bb_exc, CFGMaybeThrow); }, bbl);
									BasicBlock.set_syntax_edge(bb, SEMerge(bb_));
									Graph.close_node(g, bb);
									block_element(bb_, e);
								}
								(bb == g.g_unreachable) ? bb : loop(bb, el);
						}
					}
					loop(bb, el);
			}
		}
		block = function (bb, e) {
			var el:ImmutableList<core.Type.TExpr> = switch (e.eexpr) {
				case TBlock(el): el;
				case _: [e];
			}
			return block_el(bb, el);
		}
		var bb_last = block(bb_root, tf.tf_expr);
		Graph.close_node(g, bb_last);
		BasicBlock.add_cfg_edge(bb_last, bb_exit, CFGGoto); // implied return
		Graph.close_node(g, bb_exit);
		return {func:bb_root, exit:bb_exit};
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