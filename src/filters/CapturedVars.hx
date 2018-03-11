package filters;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
import ocaml.List;
import ocaml.PMap;
import ocaml.Ref;

using ocaml.Cloner;

class CapturedVars {
	/* BLOCK VARIABLES CAPTURE */
	/*
		For some platforms, it will simply mark the variables which are used in closures
		using the v_capture flag so it can be processed in a more optimized

		For Flash/JS platforms, it will ensure that variables used in loop sub-functions
		have an unique scope. It transforms the following expression :

		for( x in array )
			funs.push(function() return x++);

		Into the following :

		for( _x in array ) {
			var x = [_x];
			funs.push(function(x) { function() return x[0]++; }(x));
		}
	*/
	public static function captured_vars (com:context.Common.Context, e:core.Type.TExpr) : core.Type.TExpr {
		var t = com.basic;
		var impl = switch (com.platform) {
			// optimized versoin for C#/Java - use native arrays
			case Cs, Java:
				var cnativearray = switch (List.find(function (md:core.Type.ModuleType) { return switch (md) { case TClassDecl({cl_path:{a:[("cs"|"java")], b:"NativeArray"}}): true; case _: false; }}, com.types)) {
					case TClassDecl(cl): cl;
					case _: trace("Shall not be seen"); std.Sys.exit(255); throw false;
				}
				{
					captured_type: function (t:core.Type.T):core.Type.T { return TInst(cnativearray, [t]); },
					mk_ref: function (v:core.Type.TVar, ve:Option<core.Type.TExpr>, p:core.Globals.Pos) : core.Type.TExpr {
						return switch (ve) {
							case None:
								var eone = core.Type.mk(TConst(TInt(1)), t.tint, p);
								var t = switch (v.v_type) { case TInst(_, [t]): t; case _: trace("Shal not be seen"); Sys.exit(255); throw false; }
								core.Type.mk(TNew(cnativearray, [t], [eone]), v.v_type, p);
							case Some(e):
								optimization.Optimizer.mk_untyped_call("__array__", p, [e]).with({etype:v.v_type});
						}
					},
					mk_ref_access: function (e:core.Type.TExpr, v:core.Type.TVar): core.Type.TExpr {
						return core.Type.mk(TArray(e.with({etype:v.v_type}), core.Type.mk(TConst(TInt(0)), t.tint, e.epos)), e.etype, e.epos);
					},
					mk_init: function (av:core.Type.TVar, v:core.Type.TVar, pos:core.Globals.Pos) : core.Type.TExpr {
						var elocal = core.Type.mk(TLocal(v), v.v_type, pos);
						var earray = optimization.Optimizer.mk_untyped_call("__array__", pos, [elocal]).with({etype:av.v_type});
						return core.Type.mk(TVar(av, Some(earray)), t.tvoid, pos);
					}
				}
			// default implementation - use haxe array
			case _:
				{
					captured_type: t.tarray,
					mk_ref: function (v:core.Type.TVar, ve:Option<core.Type.TExpr>, p:core.Globals.Pos) : core.Type.TExpr {
						return core.Type.mk(TArrayDecl(switch (ve) {case None: Tl; case Some(e): [e]; }), v.v_type, p);
					},
					mk_ref_access: function (e:core.Type.TExpr, v:core.Type.TVar): core.Type.TExpr {
						return core.Type.mk(TArray(e.with({etype:v.v_type}), core.Type.mk(TConst(TInt(0)), t.tint, e.epos)), e.etype, e.epos);
					},
					mk_init: function (av:core.Type.TVar, v:core.Type.TVar, pos:core.Globals.Pos) : core.Type.TExpr {
						return core.Type.mk(TVar(av, Some(core.Type.mk(TArrayDecl([core.Type.mk(TLocal(v), v.v_type, pos)]), av.v_type, pos))), t.tvoid, pos);
					}
				}
		}

		function mk_var (v:core.Type.TVar, used:PMap<Int,core.Type.T>) : core.Type.TVar {
			var v2 = core.Type.alloc_var(v.v_name, PMap.find(v.v_id, used), v.v_pos);
			v2.v_meta = v.v_meta;
			return v2;
		}

		function wrap (used:PMap<Int, core.Type.T>, e:core.Type.TExpr) : core.Type.TExpr {
			return switch (e.eexpr) {
				case TVar(v, ve):
					var _tmp = if (PMap.mem(v.v_id, used)) {
						{fst:v, snd:Some(impl.mk_ref(v, ocaml.Option.map(wrap.bind(used), ve), e.epos))};
					}
					else {
						{fst:v, snd:switch(ve) { case None: None; case Some(e): Some(wrap(used, e)); }};
					}
					var v = _tmp.fst; var ve = _tmp.snd;
					e.with({eexpr:core.Type.TExprExpr.TVar(v, ve)});
				case TLocal(v) if (PMap.mem(v.v_id, used)):
					impl.mk_ref_access(e, v);
				case TFor(v, it, expr) if (PMap.mem(v.v_id, used)):
					var vtmp = mk_var(v, used);
					var it = wrap(used, it);
					var expr = wrap(used, expr);
					core.Type.mk(TFor(vtmp, it, core.Type.concat(impl.mk_init(v, vtmp, e.epos), expr)), e.etype, e.epos);
				case TTry(expr, catchs):
					var catchs = List.map(function (c) {
						var v = c.v; var e = c.e;
						var e = wrap(used, e);
						return
						try {
							var vtmp = mk_var(v, used);
							{v:vtmp, e:core.Type.concat(impl.mk_init(v, vtmp, e.epos), e)};
						}
						catch (_:ocaml.Not_found) {
							{v:v, e:e};
						}
					}, catchs);
					core.Type.mk(TTry(wrap(used, expr), catchs), e.etype, e.epos);
				case TFunction(f):
					/*
						list variables that are marked as used, but also used in that
						function and which are not declared inside it !
					*/
					var fused = new Ref<PMap<Int, core.Type.TVar>>(PMap.empty());
					var tmp_used = new Ref(used);
					function browse (usage:filters.LocalUsage.Usage) : Void {
						switch (usage) {
							case Block(f), Loop(f), Function(f): f(browse);
							case Use(v), Assign(v):
								if (PMap.mem(v.v_id, tmp_used.get())) {
									fused.set(PMap.add(v.v_id, v, fused.get()));
								}
							case Declare(v):
								tmp_used.set(PMap.remove(v.v_id, tmp_used.get()));
						}
					}
					filters.LocalUsage.local_usage(browse, e);
					var vars = PMap.fold(function (v, acc) { return v :: acc; }, fused.get(), Tl);
					// in case the variable has been marked as used in a parallel scope...
					var fexpr = new Ref(wrap(used, f.tf_expr));
					var fargs = List.map(function (arg) {
						var v = arg.v; var o = arg.c;
						return
						if (PMap.mem(v.v_id, used)) {
							var vtmp = mk_var(v, used);
							fexpr.set(core.Type.concat(impl.mk_init(v, vtmp, e.epos), fexpr.get()));
							{v:vtmp, c:o};
						}
						else {
							{v:v, c:o};
						}
					}, f.tf_args);
					var e = e.with({eexpr:core.Type.TExprExpr.TFunction(f.with({tf_args:fargs, tf_expr:fexpr.get()}))});
					/*
						Create a new function scope to make sure that the captured loop variable
						will not be overwritten in next loop iteration
					*/
					if (com.config.pf_capture_policy == CPLoopVars) {
 						// We don't want to duplicate any variable declarations, so let's make copies (issue #3902)
						var new_vars = List.map(function (v:core.Type.TVar) { return {fst:v.v_id, snd:core.Type.alloc_var(v.v_name, v.v_type, v.v_pos)}; }, vars);
						function loop (e:core.Type.TExpr) {
							return switch (e.eexpr) {
								case TLocal(v):
									try {
										var v_ = List.assoc(v.v_id, new_vars);
										v_.v_capture = true;
										e.with({eexpr:core.Type.TExprExpr.TLocal(v)});
									}
									catch (_:ocaml.Not_found) {
										e;
									}
								case _:
									core.Type.map_expr(loop, e);
							}
						}
						var e = loop(e);
						core.Type.mk(TCall(
							core.Texpr.Builder.mk_parent(
								core.Type.mk(TFunction({
									tf_args:List.map(function (tmp) { var v = tmp.snd; return {v:v, c:None}; }, new_vars),
									tf_type: e.etype,
									tf_expr: core.Type.mk_block(core.Type.mk(TReturn(Some(e)), e.etype, e.epos))
								}), TFun({args:List.map(function (tmp) { var v = tmp.snd; return {name:v.v_name, opt:false, t:v.v_type}; }, new_vars), ret:e.etype}),e.epos)),
							List.map(function (v:core.Type.TVar) { return core.Type.mk(TLocal(v), v.v_type, e.epos); }, vars)
							)
						, e.etype, e.epos);
					}
					else {
						e;
					}
				case _:
					core.Type.map_expr(wrap.bind(used), e);
			}
		}

		function do_wrap (used:PMap<Int, core.Type.TVar>, e:core.Type.TExpr) : core.Type.TExpr {
			return
			if (PMap.is_empty(used)) {
				e;
			}
			else {
				var used = PMap.map(function (v:core.Type.TVar) {
					var vt = v.v_type;
					v.v_type = impl.captured_type(vt);
					v.v_capture = true;
					return vt;
				}, used);
				wrap(used, e);
			}
		}

		function out_loop (e:core.Type.TExpr) {
			return
			switch (e.eexpr) {
				case TFor(_,_,_), TWhile(_,_,_):
					/*
						collect variables that are declared in loop but used in subfunctions
					*/
					var vars = new Ref<PMap<Int, Int>>(PMap.empty());
					var used = new Ref<PMap<Int, core.Type.TVar>>(PMap.empty());
					var depth = new Ref(0);
					function collect_vars(in_loop:Bool, usage:filters.LocalUsage.Usage) : Void {
						switch (usage) {
							case Block(f):
								var old = vars.get();
								f(collect_vars.bind(in_loop));
								vars.set(old);
							case Loop(f):
								var old = vars.get();
								f(collect_vars.bind(true));
								vars.set(old);
							case Function(f):
								depth.set(depth.get()+1);
								f(collect_vars.bind(false));
								depth.set(depth.get()-1);
							case Declare(v):
								if (in_loop) {
									vars.set(PMap.add(v.v_id, depth.get(), vars.get()));
								}
							case Use(v), Assign(v):
								try {
									var d = PMap.find(v.v_id, vars.get());
									if (d != depth.get()) {
										used.set(PMap.add(v.v_id, v, used.get()));
									}
								}
								catch (_:ocaml.Not_found) {

								}
						}
					}
					filters.LocalUsage.local_usage(collect_vars.bind(false), e);
					do_wrap(used.get(), e);
				case _:
					core.Type.map_expr(out_loop, e);
			}
		}

		function all_vars (e:core.Type.TExpr) {
			var vars = new Ref<PMap<Int, Int>>(PMap.empty());
			var used = new Ref<PMap<Int, core.Type.TVar>>(PMap.empty());
			var assigned = new Ref<PMap<Int, core.Type.TVar>>(PMap.empty());
			var depth = new Ref(0);
			function collect_vars(usage:filters.LocalUsage.Usage) : Void {
				switch (usage) {
					case Block(f):
						var old = vars.get();
						f(collect_vars);
						vars.set(old);
					case Loop(f):
						var old = vars.get();
						f(collect_vars);
						vars.set(old);
					case Function(f):
						depth.set(depth.get()+1);
						f(collect_vars);
						depth.set(depth.get()-1);
					case Declare(v):
						vars.set(PMap.add(v.v_id, depth.get(), vars.get()));
					case Use(v):
						try {
							var d = PMap.find(v.v_id, vars.get());
							if (d != depth.get()) {
								used.set(PMap.add(v.v_id, v, used.get()));
							}
						}
						catch (_:ocaml.Not_found) {

						}
					case Assign(v):
						try {
							var d = PMap.find(v.v_id, vars.get());
							if (d != depth.get()) {
								// different depth - needs wrap
								used.set(PMap.add(v.v_id, v, used.get()));
								assigned.set(PMap.add(v.v_id, v, assigned.get()));
							}
						}
						catch (_:ocaml.Not_found) {

						}
				}
			}
			filters.LocalUsage.local_usage(collect_vars, e);
			// mark all capture variables - also used in rename_local_vars at later stage
			PMap.iter(function (_, v) { v.v_capture = true; }, used.get());
			return assigned.get();
		}
		var captured = all_vars(e);
		return switch (com.config.pf_capture_policy) {
			case CPNone: e;
			case CPWrapRef: do_wrap(captured, e);
			case CPLoopVars: out_loop(e);
		}
	}
}