package optimization;

import core.Type.TExprExpr;

import equals.Equal;
import haxe.ds.ImmutableList;
import ocaml.Hashtbl;
import ocaml.List;

import optimization.AnalyzerTypes;

using ocaml.Cloner;

/* File organization:
	* analyzer.ml: The controlling file with all graph-based optimizations
	* analyzerConfig.ml: The general configuration which is used in multiple modules
	* analyzerTexpr.ml: Transformations and query functions on texpr, independent of graph/blocks
	* analyzerTexprTransformer.ml: Translation of texpr to graph/blocks
	* analyzerTypes.ml: Definition of graph, block and the analyzer context
*/

/*
	Ssa changes the expressions of a graph to conform to SSA rules. All variables are assigned to only once
	and SSA-phi expressions are created where necessary.

	The first pass inserts SSA-phi expressions for each variable in the dominance frontier of all its defining
	blocks.

	The second pass then creates and renames variables to ensure SSA property.
*/
class Ssa {
	public static function apply (ctx:Analyzer_context) : Void {
		trace("TODO: Ssa.apply");
		throw false;
	}
}

class DataFlowApi<T> {
	public var flag:Cfg_edge_Flag;
	public function transfer (actx:Analyzer_context, bb:BasicBlock, e:core.Type.TExpr): T { trace("Shall not be seen"); throw false; return null; } // The transfer function
	public function equals (a:T, b:T): Bool { trace("Shall not be seen"); throw false; return true; } // The equality function
	public var bottom : T; // The bottom element of the lattice
	public var top : T; // The top element of the lattice
	public function get_cell (i:Int): T { trace("Shall not be seen"); throw false; return null; } // Lattice cell getter
	public function set_cell (i:Int, v:T): Void { trace("Shall not be seen"); throw false; } // Lattice cell setter
	public function init (ctx:Analyzer_context): Void { trace("Shall not be seen"); throw false; } // The initialization function which is called at the start
	public function commit (ctx:Analyzer_context): Void { trace("Shall not be seen"); throw false; } // The commit function which is called at the end
	public var conditional : Bool; // Whether or not conditional branches are checked
}

class DataFlow<M> extends DataFlowApi<M> {

	public function get_ssa_edges_from (g:Graph, v:core.Type.TVar) : ImmutableList<TExpr_lookup> {
		return Graph.get_var_info(g, v).vi_ssa_edges;
	}

	public function run(ctx:AnalyzerTypes.Analyzer_context) {
		trace("TODO: DataFlow.run");
		throw false;
	}

	public function apply (ctx:AnalyzerTypes.Analyzer_context) {
		init(ctx);
		run(ctx);
		commit(ctx);
	}
}

/*
	ConstPropagation implements sparse conditional constant propagation using the DataFlow algorithm. Its lattice consists of
	constants and enum values, but only the former are propagated. Enum values are treated as immutable data tuples and allow
	extracting constants, their index or other enum values.

	This module also deals with binop/unop optimization and standard API inlining.
*/
enum ConstPropagation_T {
	Top;
	Bottom;
	Null(t:core.Type.T);
	Const(c:core.Type.TConstant);
	EnumValue(i:Int, el:ImmutableList<ConstPropagation_T>);
}
class ConstPropagation extends DataFlow<ConstPropagation_T> {
	public var lattice:Hashtbl<Int, ConstPropagation_T> = Hashtbl.create(0);

	public function new () {
		conditional = true;
		flag = FlagExecutable;
		top = Top;
		bottom = Bottom;
	}

	public override function get_cell(i) : ConstPropagation_T {
		return try { Hashtbl.find(lattice, i); } catch (_:ocaml.Not_found) { Top; }
	}
	public override function set_cell(i, ct) {
		Hashtbl.replace(lattice, i, ct);
	}
	public override function equals(lat1:ConstPropagation_T, lat2:ConstPropagation_T) {
		return switch [lat1, lat2] {
			case [Top, Top], [Bottom, Bottom]: true;
			case [Const(ct1), Const(ct2)]: Equal.equals(ct1, ct2);
			case [Null(t1), Null(t2)]: t1 == t2;
			case [EnumValue(i1, _), EnumValue(i2, _)]: Equal.equals(i1, i2);
			case _: false;
		}
	}
	public override function transfer(ctx:Analyzer_context, bb:BasicBlock, e:core.Type.TExpr) : ConstPropagation_T {
		function eval (bb:BasicBlock, e:core.Type.TExpr) : ConstPropagation_T {
			function wrap(t:ConstPropagation_T) : core.Type.TExpr {
				return switch(t) {
					case Const(ct): core.Type.mk(TConst(ct), core.Type.t_dynamic, core.Globals.null_pos);
					case Null(t): core.Type.mk(TConst(TNull), t, e.epos);
					case _: throw ocaml.Exit.instance;
				}
			}
			function unwrap(e:core.Type.TExpr) {
				return switch(e.eexpr) {
					case TConst(ct): Const(ct);
					case _: throw ocaml.Exit.instance;
				}
			}
			return
			switch (e.eexpr) {
				case TConst(TSuper|TThis):
					Bottom;
				case TConst(TNull):
					Null(e.etype);
				case TConst(ct):
					Const(ct);
				case TLocal(v):
					if (v.v_capture || core.Type.follow(v.v_type) == core.Type.t_dynamic) {
						Bottom;
					}
					else {
						get_cell(v.v_id);
					}
				case TBinop(OpAssign,_,e2):
					eval(bb, e2);
				case TBinop(op, e1, e2):
					var cl1 = eval(bb, e1);
					var cl2 = eval(bb, e2);
					var e1 = wrap(cl1);
					var e2 = wrap(cl2);
					var e = e.with({eexpr:TBinop(op, e1, e2)});
					var e_ = OptimizerTexpr.optimize_binop(e, op, e1, e2);
					if (e != e_) {
						eval(bb, e_);
					}
					else {
						unwrap(e_);
					}
				case TUnop(op, flag, e1):
					var cl1 = eval(bb, e1);
					var e1 = wrap(cl1);
					var e = e.with({eexpr:TUnop(op,flag,e1)});
					var e_ = OptimizerTexpr.optimize_unop(e, op, flag, e1);
					if (e != e_) {
						eval(bb, e_);
					}
					else {
						unwrap(e_);
					}
				case TField(_, FEnum(_,ef)):
					EnumValue(ef.ef_index, Tl);
				case TCall({eexpr:TField(_, FEnum(_, ef))}, el):
					var cll = List.map(function (e:core.Type.TExpr) : ConstPropagation_T {
						return try { eval(bb, e); } catch (_:ocaml.Exit) { Bottom; }
					}, el);
					EnumValue(ef.ef_index, cll);
				case TEnumParameter(e1,_,i):
					switch (eval(bb, e1)) {
						case EnumValue(_,el): try { List.nth(el, i); } catch (_:ocaml.Failure) { throw ocaml.Exit.instance; }
						case _: throw ocaml.Exit.instance;
					}
				case TEnumIndex(e1):
					switch (eval(bb, e1)) {
						case EnumValue(i,_): Const(TInt(i));
						case _: throw ocaml.Exit.instance;
					}
				case TCall({eexpr:TField(_, FStatic(c={cl_path:{a:[], b:"Type"}}, cf={cf_name:"enumIndex"}))}, [e1]) if (ctx.com.platform == Eval):
					switch [core.Type.follow(e1.etype), eval(bb, e1)] {
						case [TEnum(_), EnumValue(i,_)]: Const(TInt(i));
						case [_, e1]:
							switch (Optimizer.api_inline2(ctx.com, c, cf.cf_name, [wrap(e1)], e.epos)) {
								case None: throw ocaml.Exit.instance;
								case Some(e): eval(bb, e);
							}
					}
				case TCall({eexpr:TField(_,FStatic(c, cf))}, el):
					var el = List.map(eval.bind(bb), el);
					var el = List.map(wrap, el);
					switch (Optimizer.api_inline2(ctx.com, c, cf.cf_name, el, e.epos)) {
						case None: throw ocaml.Exit.instance;
						case Some(e): eval(bb, e);
					}
				case TParenthesis(e1), TMeta(_, e1), TCast(e1, None):
					eval(bb,e1);
				case _:
					var e1 = switch [ctx.com.platform, e.eexpr] {
						case [Js, TArray(e1, {eexpr:TConst(TInt(i))})] if (i == 1): e1;
						case [Cpp, TCall({eexpr:TField(e1, FDynamic("__Index"))}, [])]: e1;
						case [Neko, TField(e1, FDynamic("index"))]: e1;
						case _: throw ocaml.Exit.instance;
					}
					switch [core.Type.follow(e1.etype), eval(bb, e1)] {
						case [TEnum(_), EnumValue(i, _)]: Const(TInt(i));
						case _: throw ocaml.Exit.instance;
					}
			}
		}
		return
		try {
			eval(bb, e);
		}
		catch (_:ocaml.Exit) {
			Bottom;
		}
	}
	public override function init (ctx) { Hashtbl.clear(lattice); }
	public override function commit (ctx:Analyzer_context) {
		function _inline (e:core.Type.TExpr, i:Int) : core.Type.TExpr {
			return switch(get_cell(i)) {
				case Top, Bottom, EnumValue(_), Null(_):
					throw ocaml.Not_found.instance;
				case Const(ct):
					var e_ = core.Texpr.type_constant(ctx.com.basic, core.Type.tconst_to_const(ct), e.epos);
					if (!AnalyzerTexpr.type_change_ok(ctx.com, e_.etype, e.etype)) { throw ocaml.Not_found.instance; }
					e_;
			}
		}
		function is_special_var (v:core.Type.TVar) : Bool { return AnalyzerTexpr.is_asvar_type(v.v_type) || v.v_capture; }
		function commit_ (e:core.Type.TExpr) {
			return switch (e.eexpr) {
				case TLocal(v) if (!is_special_var(v)):
					try {
						_inline(e, v.v_id);
					}
					catch (_:ocaml.Not_found) {
						e;
					}
				case TBinop(op=(OpAssign|OpAssignOp(_)), e1={eexpr:TLocal(v)}, e2):
					var e2 = try {
						if (OptimizerTexpr.has_side_effect(e2)) { throw ocaml.Not_found.instance; }
						_inline(e2, v.v_id);
					}
					catch (_:ocaml.Not_found) {
						commit_(e2);
					}
					e.with({eexpr:TBinop(op, e1, e2)});
				case TVar(v, Some(e1)) if (!OptimizerTexpr.has_side_effect(e1)):
					var e1 = try { _inline(e1, v.v_id); } catch (_:ocaml.Not_found) { commit_(e1); }
					e.with({eexpr:TVar(v, Some(e1))});
				case _: core.Type.map_expr(commit_, e);
			}
		}
		Graph.iter_dom_tree(ctx.graph, function (bb:BasicBlock) {
			if (!List.exists(function (edge:Cfg_edge) { return BasicBlock.has_flag(edge, FlagExecutable); }, bb.bb_incoming)) { bb.bb_dominator = ctx.graph.g_unreachable; }
			AnalyzerTexpr.dynarray_map(commit_, bb.bb_el);
		});
	}
}

/*
	Propagates local variables to other local variables.

	Respects scopes on targets where it matters (all except JS and As3).
*/
enum CopyPropagation_T {
	Top;
	Bottom;
	Local(v:core.Type.TVar);
}

class CopyPropagation extends DataFlow<CopyPropagation_T> {
	public var lattice:Hashtbl<Int, CopyPropagation_T> = Hashtbl.create(0);
	public function new () {
		conditional = false;
		flag = FlagCopyPropagation;
		top = Top;
		bottom = Bottom;
	}
	public function to_string (v:CopyPropagation_T) : String {
		return switch (v) {
			case Top: "Top";
			case Bottom: "Bottom";
			case Local(v): '${v.v_name}<${v.v_id}>';
		}
	}
	public override function get_cell (i:Int) : CopyPropagation_T{
		return try { Hashtbl.find(lattice, i); } catch (_:ocaml.Not_found) { Top; }
	}
	public override function set_cell (i:Int, ct:CopyPropagation_T) {
		Hashtbl.replace(lattice, i, ct);
	}
	public override function equals (t1:CopyPropagation_T, t2:CopyPropagation_T) {
		return switch [t1, t2] {
			case [Top, Top]: true;
			case [Bottom, Bottom]: true;
			case [Local(v1), Local(v2)]: v1.v_id == v2.v_id;
			case _: false;
		}
	}
	public override function transfer (ctx:Analyzer_context, bb:BasicBlock, e:core.Type.TExpr) {
		function loop (e:core.Type.TExpr) {
			return switch (e.eexpr) {
				case TLocal(v) if (!v.v_capture): Local(v);
				case TParenthesis(e1), TMeta(_, e1), TCast(e1, None): loop(e1);
				case _: Bottom;
			}
		}
		return loop(e);
	}
	public override function init (ctx) { Hashtbl.clear(lattice); }
	public override function commit (ctx) {
		function commit_ (bb:BasicBlock, e:core.Type.TExpr) {
			return switch (e.eexpr) {
				case TLocal(v) if (!v.v_capture):
					try {
						var lat = get_cell(v.v_id);
						function leave() : Dynamic {
							Hashtbl.remove(lattice, v.v_id);
							throw ocaml.Not_found.instance;
						}
						var v_ = switch (lat) { case Local(v): v; case _: leave(); }
						if (!AnalyzerTexpr.type_change_ok(ctx.com, v_.v_type, v.v_type)) { leave(); }
						var v__ = Graph.get_var_origin(ctx.graph, v_);
						/* This restriction is in place due to how we currently reconstruct the AST. Multiple SSA-vars may be turned back to
							the same origin var, which creates interference that is not tracked in the analysis. We address this by only
							considering variables whose origin-variables are assigned to at most once. */
						var writes = Graph.get_var_info(ctx.graph, v__).vi_writes;
						switch (writes) {
							case [bb_] if (BasicBlock.in_scope(bb, bb_)):
							case _: leave();
						}
						commit_(bb, e.with({eexpr:TLocal(v_)}));
					}
					catch (_:ocaml.Not_found) {
						e;
					}
				case TBinop(op=(OpAssign|OpAssignOp(_)), e1={eexpr:TLocal(_)}, e2):
					var e2 = commit_(bb, e2);
					e.with({eexpr:TBinop(op, e1, e2)});
				case _: core.Type.map_expr(commit_.bind(bb), e);
			}
		}
		Graph.iter_dom_tree(ctx.graph, function (bb:BasicBlock) {
			AnalyzerTexpr.dynarray_map(commit_.bind(bb), bb.bb_el);
		});
	}
}

/*
	LocalDce implements a mark & sweep dead code elimination. The mark phase follows the CFG edges of the graphs to find
	variable usages and marks variables accordingly. If ConstPropagation was run before, only CFG edges which are
	considered executable are processed.

	If a variable is marked as used, its reaching definition is recursively marked as used too. Furthermore its
	value is processed as an expression.

	The sweep phase removes all variable declarations and assignments of unused variables, keeping only the assigned
	expression in case of side-effects.
*/
class LocalDce {
	public static function apply (ctx:Analyzer_context) : Void {
		trace("TODO: LocalDce.apply");
		throw false;
	}
}

class Debug {
	public static function dot_debug (ctx, c:core.Type.TClass, cf:core.Type.TClassField) {
		trace("TODO: Debug.dot_debug");
		throw false;
	}
}

class Run {
	public static function with_timer<A> (detailed:Bool, s:ImmutableList<String>, f:Void->A) : A {
		var timer = core.Timer.timer((detailed) ? ("analyzer"::s) : ["analyzer"]);
		var r = f();
		timer();
		return r;
	}

	public static function create_analyzer_context (com:context.Common.Context, config:AnalyzerConfig, e:core.Type.TExpr) : optimization.AnalyzerTypes.Analyzer_context {
		var g = AnalyzerTypes.Graph.create(e.etype, e.epos);
		var ctx = {
			com: com,
			config: config,
			graph: g,
			/* For CPP we want to use variable names which are "probably" not used by users in order to
				avoid problems with the debugger, see https://github.com/HaxeFoundation/hxcpp/issues/365 */
			temp_var_name: switch (com.platform) { case Cpp: "_hx_tmp"; case _: "tmp"; },
			entry: g.g_unreachable,
			has_unbound: false,
			loop_counter: 0,
			loop_stack: Tl,
			debug_exprs: Tl,
			name_stack: Tl
		}
		return ctx;
	}

	public static function add_debug_expr (ctx:Analyzer_context, s:String, e:core.Type.TExpr) : Void {
		ctx.debug_exprs = {s:s, expr:e} :: ctx.debug_exprs;
	}

	public static function there (actx:Analyzer_context, e:core.Type.TExpr) : Bool {
		if (actx.com.debug) { add_debug_expr(actx, "intial", e); }
		var e = with_timer(actx.config.detail_times, ["->", "filter-apply"], function() { return AnalyzerTexpr.TexprFilter.apply(actx.com, e); });
		if (actx.com.debug) { add_debug_expr(actx, "after filter-apply", e); }
		var _tmp = switch (e.eexpr) {
			case TFunction(tf):
				{fst:tf, snd:e.etype, trd:true};
			case _:
				// Wrap expression in a function so we don't have to treat it as a special case throughout.
				var e = core.Type.mk(TReturn(Some(e)), core.Type.t_dynamic, e.epos);
				var tf = {tf_args:Tl, tf_type:e.etype, tf_expr: e};
				{fst:tf, snd:core.Type.tfun([], e.etype), trd:false};
		}
		var tf = _tmp.fst; var t = _tmp.snd; var is_real_function = _tmp.trd;
		with_timer(actx.config.detail_times, ["->", "from-texpr"], function () { return AnalyzerTexprTransformer.from_tfunction(actx, tf, t, e.epos); });
		return is_real_function;
	}

	public static function back_again (actx:Analyzer_context, is_real_function:Bool) : core.Type.TExpr {
		var e = with_timer(actx.config.detail_times, ["<-", "to-texpr"], function () { return AnalyzerTexprTransformer.to_texpr(actx); });
		if (actx.com.debug) { add_debug_expr(actx, "after to-texpr", e); }
		for (vi in actx.graph.g_var_infos) {
			vi.vi_var.v_extra = vi.vi_extra;
		}
		var e = (actx.config.fusion) ? with_timer(actx.config.detail_times, ["<-", "fusion"], function () { return AnalyzerTexpr.Fusion.apply(actx.com, actx.config, e); }) : e;
		if (actx.com.debug) { add_debug_expr(actx, "after fusion", e); }
		var e = with_timer(actx.config.detail_times, ["<-", "cleanup"], function () { return AnalyzerTexpr.Cleanup.apply(actx.com, e); });
		if (actx.com.debug) { add_debug_expr(actx, "after cleanup", e); }
		var e = if (is_real_function) {
			e;
		}
		else {
			// Get rid of the wrapping function and its return expressions.
			function loop (first:Bool, e:core.Type.TExpr) : core.Type.TExpr {
				return switch (e.eexpr) {
					case TReturn(Some(e)): e;
					case TFunction(tf) if (first):
						switch (loop(false, tf.tf_expr)) {
							case {eexpr:(TBlock(_)|TIf(_)|TSwitch(_,_,_)|TTry(_,_))} if (actx.com.platform == Cpp || actx.com.platform == Hl):
								core.Type.mk(TCall(e, []), tf.tf_type, e.epos);
							case e:
								e;
						}
					case TBlock([e]): loop(first, e);
					case TFunction(_): e;
					case _: core.Type.map_expr(loop.bind(first), e);
				}
			}
			loop(true, e);
		}
		return e;
	}

	public static function run_on_expr (actx:optimization.Analyzer_context, e:core.Type.TExpr) : core.Type.TExpr {
		var is_real_function = there(actx, e);
		with_timer(actx.config.detail_times, ["->", "idom"], function () { Graph.infer_immediate_dominators(actx.graph); });
		with_timer(actx.config.detail_times, ["->", "infer_scopes"], function () { Graph.infer_scopes(actx.graph); });
		with_timer(actx.config.detail_times, ["->", "var writes"], function () { Graph.infer_var_writes(actx.graph); });
		if (actx.com.debug) { Graph.check_integrity(actx.graph); }
		if (actx.config.optimize && !actx.has_unbound) {
			with_timer(actx.config.detail_times, ["optimize", "ssa-apply"], function () { Ssa.apply(actx); });
			if (actx.config.const_propagation) { with_timer(actx.config.detail_times, ["optimize", "const-propagation"], function () { new ConstPropagation().apply(actx); }); }
			if (actx.config.copy_propagation) { with_timer(actx.config.detail_times, ["optimize", "copy-propagation"], function () { new CopyPropagation().apply(actx); }); }
			with_timer(actx.config.detail_times, ["optimize", "local-dce"], function () { LocalDce.apply(actx); });
		}
		return back_again(actx, is_real_function);
	}

	public static function reduce_control_flow (ctx:context.Typecore.Typer, e:core.Type.TExpr) : core.Type.TExpr {
		var e = core.Type.map_expr(reduce_control_flow.bind(ctx), e);
		return optimization.Optimizer.reduce_control_flow(ctx, e);
	}

	public static function run_on_field (ctx:context.Typecore.Typer, config:AnalyzerConfig, c:core.Type.TClass, cf:core.Type.TClassField) {
		switch (cf.cf_expr) {
			case Some(e) if (!AnalyzerConfig.is_ignored(cf.cf_meta) && !context.Typecore.is_removable_field(ctx, cf)):
				var config = AnalyzerConfig.update_config_from_meta(ctx.com, config, cf.cf_meta);
				switch (e.eexpr) { case TFunction(tf): cf.cf_expr_unoptimized = Some(tf); case _: };
				var actx = create_analyzer_context(ctx.com, config, e);
				function debug() {
					trace("TODO: run_on_field > debug");
					throw false;
				}
				var e = try {
					run_on_expr(actx, e);
				}
				catch (exc:core.Error) { throw exc; }
				catch (exc:context.Common.Abort) { throw exc; }
				catch (exc:Any) { debug(); throw exc;}
				var e = reduce_control_flow(ctx, e);
				switch (config.debug_kind) {
					case DebugNone:
					case DebugDot: Debug.dot_debug(actx, c, cf);
					case DebugFull: debug();
				}
				cf.cf_expr = Some(e);
			case _:
		}
	}

	public static function run_on_class (ctx:context.Typecore.Typer, config:AnalyzerConfig, c:core.Type.TClass) {
		var config = AnalyzerConfig.update_config_from_meta(ctx.com, config, c.cl_meta);
		function process_field (stat:Bool, cf:core.Type.TClassField) {
			return switch (cf.cf_kind) {
				case Var(_) if (!stat):
				case _: run_on_field(ctx, config, c, cf);
			}
		}
		List.iter(process_field.bind(false), c.cl_ordered_fields);
		List.iter(process_field.bind(true), c.cl_ordered_statics);
		switch (c.cl_constructor) {
			case None:
			case Some(f): process_field(false, f);
		}
		switch (c.cl_init) {
			case None:
			case Some(e):
				var tf = {tf_args:Tl, tf_type:e.etype, tf_expr:e};
				var e = core.Type.mk(TFunction(tf), core.Type.tfun([], e.etype), e.epos);
				var actx = create_analyzer_context(ctx.com, config.with({optimize:false}), e);
				var e = run_on_expr(actx, e);
				var e = switch (e.eexpr) {
					case TFunction(tf): tf.tf_expr;
					case _: trace("Shall not be seen"); Sys.exit(255); throw false;
				}
				c.cl_init = Some(e);
		}
	}

	public static function run_on_type(ctx:context.Typecore.Typer, config:AnalyzerConfig, t:core.Type.ModuleType) : Void {
		switch (t) {
			case TClassDecl(c) if (AnalyzerConfig.is_ignored(c.cl_meta)):
			case TClassDecl(c): run_on_class(ctx, config, c);
			case TEnumDecl(_):
			case TTypeDecl(_):
			case TAbstractDecl(_):
		}
	}

	public static function run_on_types (ctx:context.Typecore.Typer, types:ImmutableList<core.Type.ModuleType>) : Void {
		var com = ctx.com;
		var config = AnalyzerConfig.get_base_config(com);
		with_timer(config.detail_times, ["other"], function() {
			var cfl = if (config.optimize && config.purity_inference) {
				with_timer(config.detail_times, ["optimize", "purity-inference"], function () { return AnalyzerTexpr.Purity.infer(com); });
			}
			else {
				Tl;
			}
			List.iter(run_on_type.bind(ctx, config), types);
			List.iter(function (cf:core.Type.TClassField) { cf.cf_meta = List.filter(function (meta:core.Ast.MetadataEntry) { var m = meta.name; return m != Pure; }, cf.cf_meta); }, cfl);
		});
	}
}

class Analyzer {

}