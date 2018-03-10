package filters;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
import ocaml.List;
import ocaml.PMap;
import ocaml.Ref;

class Filters {

	/* retrieve string from @:native metadata or raise Not_found */

	/* PASS 1 begin */

	/* Adds final returns to functions as required by some platforms */

	/* -------------------------------------------------------------------------- */
	/* CHECK LOCAL VARS INIT */
	public static function check_local_vars_init (e:core.Type.TExpr) : core.Type.TExpr {
		function intersect(vl1:PMap<Int, Bool>, vl2:PMap<Int, Bool>) : PMap<Int, Bool> {
			return PMap.mapi(function (v:Int, t:Bool) : Bool { return t && PMap.find(v, vl2); }, vl1);
		}
		function join (vars:Ref<PMap<Int, Bool>>, cvars:ImmutableList<PMap<Int, Bool>>) {
			List.iter(function (v:PMap<Int, Bool>) { vars.set(intersect(vars.get(), v)); }, cvars);
		}
		function restore (vars:Ref<PMap<Int, Bool>>, old_vars:PMap<Int, Bool>, declared:ImmutableList<Int>) {
			// restore variables declared in this block to their previous state
			vars.set(List.fold_left(function (acc:PMap<Int, Bool>, v:Int) {
				return
				try {
					PMap.add(v, PMap.find(v, old_vars), acc);
				}
				catch (_:ocaml.Not_found) {
					PMap.remove(v, acc);
				}
			}, vars.get(), declared));
		}
		var declared = new Ref<ImmutableList<Int>>(Tl);
		var outside_vars = new Ref<PMap<Int, Bool>>(PMap.empty());
		function loop (vars:Ref<PMap<Int, Bool>>, e:core.Type.TExpr) {
			switch (e.eexpr) {
				case TLocal(v):
					var init = try { PMap.find(v.v_id, vars.get()); } catch (_:ocaml.Not_found) { true; }
					if (!init && !PMap.mem(v.v_id, outside_vars.get())) {
						if (v.v_name == "this") {
							core.Error.error("Missing this = value", e.epos);
						}
						else {
							core.Error.error("Local variable "+v.v_name + " used without being initialized", e.epos);
						}
					}
				case TVar(v, eo):
					switch (eo) {
						case None if (core.Meta.has(InlineConstructorVariable, v.v_meta)):
						case None:
							declared.set(v.v_id :: declared.get());
							vars.set(PMap.add(v.v_id, false, vars.get()));
						case Some(e):
							loop(vars, e);
					}
				case TBlock(el):
					var old = declared.get();
					var old_vars = vars.get();
					declared.set([]);
					List.iter(loop.bind(vars), el);
					restore(vars, old_vars, List.rev(declared.get()));
					declared.set(old);
				case TBinop(OpAssign, {eexpr:TLocal(v)}, e) if (PMap.mem(v.v_id, vars.get())):
					loop(vars, e);
					vars.set(PMap.add(v.v_id, true, vars.get()));
				case TIf(e1, e2, eo):
					loop(vars, e1);
					var vbase = vars.get();
					loop(vars, e2);
					switch (eo) {
						case None: vars.set(vbase);
						// ignore else false cases (they are added by the side-effect handler)
						case Some({eexpr:TConst(TBool(false))}):
						case Some(e):
							var v1 = vars.get();
							vars.set(vbase);
							loop(vars, e);
							vars.set(intersect(vars.get(), v1));
					}
				case TWhile(cond,e, flag):
					switch (flag) {
						case NormalWhile if (!(cond.eexpr.match(TParenthesis({eexpr:TConst(TBool(true))})))):
							loop(vars, cond);
							var old = vars.get();
							loop(vars, e);
							vars.set(old);
						case _:
							loop(vars, e);
							loop(vars, cond);
					}
				case TTry(e, catches):
					var cvars = List.map(function (c) {
						var v = c.v; var e = c.e;
						var old = vars.get();
						loop(vars, e);
						var v = vars.get();
						vars.set(old);
						return v;
					}, catches);
					loop(vars, e);
					join(vars, cvars);
				case TSwitch(e, cases, def):
					loop(vars, e);
					var cvars = List.map(function (c) {
						var ec = c.values; var e = c.e;
						var old = vars.get();
						List.iter(loop.bind(vars), ec);
						vars.set(old);
						loop(vars, e);
						var v = vars.get();
						vars.set(old);
						return v;
					}, cases);
					switch (def) {
						case None if (switch (e.eexpr) { case TMeta({name:Exhaustive}, _), TParenthesis({eexpr:TMeta({name:Exhaustive}, _)}): true; case _: false; }):
							switch (cvars) {
								case cv :: cvars:
									PMap.iter(function (i, b) { if (b) { vars.set(PMap.add(i, b, vars.get())); } }, cv);
									join(vars, cvars);
								case []:
							}
						case None:
						case Some(e):
							loop(vars, e);
							join(vars, cvars);
					}
				// mark all reachable vars as initialized, since we don't exit the block
				case TBreak, TContinue, TReturn(None):
					vars.set(PMap.map(function (_) { return true; }, vars.get()));
				case TThrow(e), TReturn(Some(e)):
					loop(vars, e);
					vars.set(PMap.map(function (_) { return true; }, vars.get()));
				case TFunction(tf):
					var old = outside_vars.get();
					/* Mark all known variables as "outside" so we can ignore their initialization state within the function.
						We cannot use `vars` directly because we still care about initializations the function might make.
					*/
					PMap.iter(function (i, _) { outside_vars.set(PMap.add(i, true, outside_vars.get())); }, vars.get());
					loop(vars, tf.tf_expr);
					outside_vars.set(old);
				case _:
					core.Type.iter(loop.bind(vars), e);
			}
		}
		loop(new Ref(PMap.empty()), e);
		return e;
	}

	/* -------------------------------------------------------------------------- */
	/* RENAME LOCAL VARS */

	/* PASS 1 end */

	/* Saves a class state so it can be restored later, e.g. after DCE or native path rewrite */

	/* PASS 2 begin */

	/* Removes extern and macro fields, also checks for Void fields */

	/* PASS 2 end */

	/* PASS 3 begin */

	/* PASS 3 end */

	public static final pp_counter = new Ref(1);

	public static function is_cached (t:core.Type.ModuleType) : Bool {
		var m = core.Type.t_infos(t).mt_module.m_extra;
		if (m.m_processed == 0) { m.m_processed = pp_counter.get(); }
		return m.m_processed != pp_counter.get();
	}

	public static function filter_timer (detailed:Bool, s:ImmutableList<String>) : Void->Void {
		return core.Timer.timer((detailed) ? "filters"::s : ["filters"]);
	}

	public static function run (com:context.Common.Context, tctx:context.Typecore.Typer, main:Option<core.Type.TExpr>) : Void {
		trace("TODO: filters.Filters.run");
		var detail_times = context.Common.raw_defined(com, "filter-times");
		var new_types = List.filter(function (t:core.Type.ModuleType) {
			switch (t) {
				case TClassDecl(cls):
					List.iter(function (arg) { var iface = arg.c; core.Type.add_descendant(iface, cls); }, cls.cl_implements);
					switch (cls.cl_super) {
						case Some({c:csup}): core.Type.add_descendant(csup, cls);
						case None:
					}
				case _:
			}
			return !is_cached(t);
		}, com.types);
		/* PASS 1: general expression filters */
		var filters:ImmutableList<core.Type.TExpr->core.Type.TExpr> = [
			filters.VarLazifier.apply.bind(com),
			context.typecore.AbstractCast.handle_abstract_casts.bind(tctx),
			check_local_vars_init,
			(context.Common.defined(com, OldConstructorInline) ? optimization.Optimizer.inline_constructors.bind(tctx) : optimization.InlineConstructors.inline_constructors.bind(tctx)),
			optimization.Optimizer.reduce_expression.bind(tctx),
			CapturedVars.captured_vars.bind(com)
		];
		var filters = switch (com.platform) {
			case Cs:
				trace("TODO: finish Filters.run"); throw false;
				// SetHXGen.run_filter com new_types;
				// filters @ [
				// 	TryCatchWrapper.configure_cs com
				// ]
			case Java:
				trace("TODO: finish Filters.run"); throw false;
				// SetHXGen.run_filter com new_types;
				// filters @ [
				// 	TryCatchWrapper.configure_java com
				// ]
			case Js:
				trace("TODO: finish Filters.run"); throw false;
				// filters @ [JsExceptions.init tctx];
			case _: filters;
		}
		var t = filter_timer(detail_times, ["expr 1"]);
		List.iter(FiltersCommon.run_expression_filters.bind(tctx, filters), new_types);
		t();
		// PASS 1.5: pre-analyzer type filters
		throw false;
	}
}