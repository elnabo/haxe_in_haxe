package optimization;

import haxe.ds.ImmutableList;
import ocaml.List;

using ocaml.Cloner;

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

	public static function add_debug_expr (ctx:AnalyzerTypes.Analyzer_context, s:String, e:core.Type.TExpr) : Void {
		ctx.debug_exprs = {s:s, expr:e} :: ctx.debug_exprs;
	}

	public static function there (actx:AnalyzerTypes.Analyzer_context, e:core.Type.TExpr) : Bool {
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

	public static function run_on_expr (actx:optimization.AnalyzerTypes.Analyzer_context, e:core.Type.TExpr) : core.Type.TExpr {
		trace("TODO: run_on_expr");
		var is_real_functon = there(actx, e);
		throw false;
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