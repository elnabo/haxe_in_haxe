package filters;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
import ocaml.Hashtbl;
import ocaml.List;
import ocaml.PMap;
import ocaml.Ref;

using ocaml.Cloner;

class Filters {

	/* retrieve string from @:native metadata or raise Not_found */
	public static function get_native_name(meta:core.Ast.Metadata) : {name:String, pos:core.Globals.Pos} {
		function get_native(meta:core.Ast.Metadata) : core.Ast.MetadataEntry {
			return switch (meta) {
				case []: throw ocaml.Not_found.instance;
				case (meta={name:Native, params:[v], pos:p}) :: _:
					meta;
				case _ :: meta:
					get_native(meta);
			}
		}
		var _tmp = get_native(meta);
		var e = _tmp.params; var mp = _tmp.pos;
		return
		switch (e) {
			case [{expr:EConst(CString(name)), pos:p}]: {name:name, pos:p};
			case []: throw ocaml.Not_found.instance;
			case _: core.Error.error("String expected", mp);
		}
	}

	/* PASS 1 begin */

	/* Adds final returns to functions as required by some platforms */
	public static function add_final_return (e:core.Type.TExpr) : core.Type.TExpr {
		function loop (e:core.Type.TExpr, t:core.Type.T) : core.Type.TExpr {
			function def_return (p:core.Globals.Pos) : core.Type.TExpr {
				var c:core.Type.TConstant = switch (core.Type.follow(t)) {
					case TAbstract({a_path:{a:[], b:"Int"}}, _): TInt(0);
					case TAbstract({a_path:{a:[], b:"Float"}}, _): TFloat("0.");
					case TAbstract({a_path:{a:[], b:"Bool"}}, _): TBool(false);
					case _: TNull;
				}
				return {eexpr:core.Type.TExprExpr.TReturn(Some({eexpr:core.Type.TExprExpr.TConst(c), epos:p, etype:t})), etype:core.Type.t_dynamic, epos:p};
			}
			return switch (e.eexpr) {
				case TBlock(el):
					switch (List.rev(el)) {
						case []: e;
						case elast :: el:
							switch (loop(elast, t)) {
								case {eexpr:TBlock(el2)}: e.with({eexpr:core.Type.TExprExpr.TBlock(List.append(List.rev(el), el2))});
								case elast: e.with({eexpr:core.Type.TExprExpr.TBlock(List.rev(elast::el))});
							}
					}
				case TReturn(_): e;
				case _: e.with({eexpr:core.Type.TExprExpr.TBlock([e, def_return(e.epos)])});
			}
		}
		var e = core.Type.map_expr(add_final_return, e);
		return
		switch (e.eexpr) {
			case TFunction(f):
				var f = switch (core.Type.follow(f.tf_type)) {
					case TAbstract({a_path:{a:[], b:"Void"}}, []): f;
					case _: f.with({tf_expr:loop(f.tf_expr, f.tf_type)});
				}
				e.with({eexpr:core.Type.TExprExpr.TFunction(f)});
			case _: e;
		}
	}

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
	public static function collect_reserved_local_names (com:context.Common.Context) : PMap<String, Bool> {
		return switch (com.platform) {
			case Js:
				var h = new Ref<PMap<String, Bool>>(PMap.empty());
				function add (name:String) { h.set(PMap.add(name, true, h.get())); }
				List.iter(function (mt) {
					var tinfos = core.Type.t_infos(mt);
					var native_name = try { get_native_name(tinfos.mt_meta).name; } catch (_:ocaml.Not_found) { core.Path.flat_path(tinfos.mt_path); }
					if (native_name == "") {
						switch (mt) {
							case TClassDecl(c):
								List.iter(function (cf) {
									var native_name = try { get_native_name(cf.cf_meta).name; } catch (_:ocaml.Not_found) { cf.cf_name; }
									add(native_name);
								}, c.cl_ordered_statics);
							case _:
						}
					}
					else {
						add(native_name);
					}
				}, com.types);
				h.get();
			case _: PMap.empty();
		}
	}

	public static function rename_local_vars(ctx:context.Typecore.Typer, reserved:PMap<String, Bool>, e:core.Type.TExpr) : core.Type.TExpr {
		var vars = new Ref(Tl);
		function declare (v:core.Type.TVar) {
			vars.set(v :: vars.get());
		}
		var reserved = new Ref(reserved);

		function reserve (name:String) {
			reserved.set(PMap.add(name, true, reserved.get()));
		}

		function check(t:core.Type.ModuleType) {
			switch (core.Type.t_infos(t).mt_path) {
				case {a:[], b:name}, {a: name :: _}: reserve(name);
			}
		}

		function check_type (t:core.Type.T) {
			switch (core.Type.follow(t)) {
				case TInst(c, _): check(TClassDecl(c));
				case TEnum(e, _): check(TEnumDecl(e));
				case TType(t, _): check(TTypeDecl(t));
				case TAbstract(a, _): check(TAbstractDecl(a));
				case TMono(_), TLazy(_), TAnon(_), TDynamic(_), TFun(_):
			}
		}

		function collect (e:core.Type.TExpr) : Void {
			switch (e.eexpr) {
				case TVar(v, eo):
					declare(v);
					switch (eo) { case None: case Some(e): collect(e); }
				case TFor(v, e1, e2):
					declare(v);
					collect(e1);
					collect(e2);
				case TTry(e1, catches):
					collect(e1);
					List.iter(function (c) {
						var v = c.v; var e = c.e;
						declare(v);
						check_type(v.v_type);
						collect(e);
					}, catches);
				case TFunction (tf):
					List.iter(function (arg) { var v = arg.v; declare(v); }, tf.tf_args);
					collect(tf.tf_expr);
				case TTypeExpr(t):
					check(t);
				case TNew(c, _, _):
					core.Type.iter(collect, e);
					check(TClassDecl(c));
				case TCast(e, Some(t)):
					collect(e);
					check(t);
				case TConst(TSuper):
					check_type(e.etype);
				case _:
					core.Type.iter(collect, e);

			}
		}
		// Pass 1: Collect used identifiers and variables.
		reserve("this");
		if (ctx.com.platform == Java) { reserve("_"); }
		switch (ctx.curclass.cl_path) {
			case {a:s :: _}, {a:[], b:s}: reserve(s);
		}
		collect(e);
		// Pass 2: Check and rename variables.
		var count_table:Hashtbl<String, Int> = Hashtbl.create(0);
		function maybe_rename (v:core.Type.TVar) : Void {
			// chop escape char for all local variables generated
			if (context.Typecore.is_gen_local(v)) { v.v_name = "_g" + v.v_name.substr(1); }
			var name = new Ref(v.v_name);
			var count = new Ref(try { Hashtbl.find(count_table, v.v_name); } catch (_:ocaml.Not_found) { 0; });
			while (PMap.mem(name.get(), reserved.get())) {
				count.set(count.get() + 1);
				name.set(v.v_name + count.get());
			}
			reserve(name.get());
			Hashtbl.replace(count_table, v.v_name, count.get());
			if (!core.Meta.has(RealPath, v.v_meta)) {
				v.v_meta = ({name:RealPath, params:[{expr:EConst(CString(v.v_name)), pos:e.epos}], pos:e.epos} : core.Ast.MetadataEntry) :: v.v_meta;
			}
			v.v_name = name.get();
		}
		List.iter(maybe_rename, List.rev(vars.get()));
		return e;
	}

	public static function mark_switch_break_loops (e:core.Type.TExpr) {
		function add_loop_label(n:Int, e:core.Type.TExpr) : core.Type.TExpr {
			return e.with({eexpr:core.Type.TExprExpr.TMeta({name:LoopLabel, params:[{expr:EConst(CInt(Std.string(n))), pos:e.epos}] ,pos:e.epos}, e)});
		}
		var in_switch = new Ref(false);
		var did_found = new Ref(-1);
		var num = new Ref(0);
		var cur_num = new Ref(0);
		function run (e:core.Type.TExpr) : core.Type.TExpr {
			return switch (e.eexpr) {
				case TFunction(_):
					var old_num = num.get();
					num.set(0);
					var ret = core.Type.map_expr(run, e);
					num.set(old_num);
					ret;
				case TWhile(_,_,_), TFor(_,_,_):
					var last_switch = in_switch.get();
					var last_found = did_found.get();
					var last_num = cur_num.get();
					in_switch.set(false);
					num.set(num.get() + 1);
					cur_num.set(num.get());
					did_found.set(-1);
					var new_e = core.Type.map_expr(run, e); // assuming that no loop will be found in the condition
					var new_e = (did_found.get() != -1) ? add_loop_label(did_found.get(), new_e) : new_e;
					did_found.set(last_found);
					in_switch.set(last_switch);
					cur_num.set(last_num);
					new_e;
				case TSwitch(_,_,_):
					var last_switch = in_switch.get();
					in_switch.set(true);
					var new_e = core.Type.map_expr(run, e);
					in_switch.set(last_switch);
					new_e;
				case TBreak:
					if (in_switch.get()) {
						did_found.set(cur_num.get());
						add_loop_label(cur_num.get(), e);
					}
					else {
						e;
					}
				case _: core.Type.map_expr(run, e);
			}
		}
		return run(e);
	}

	public static function check_unification(ctx:context.Typecore.Typer, e:core.Type.TExpr, t:core.Type.T) : core.Type.TExpr {
		switch [e.eexpr, t] {
			case [TLocal(v), TType({t_path:{a:["cs"], b:("Ref"|"Out")}}, _)]:
				// TODO: this smells of hack, but we have to deal with it somehow
				v.v_capture = true;
			case _:
		}
		return e;
	}
	/* PASS 1 end */

	/* Saves a class state so it can be restored later, e.g. after DCE or native path rewrite */
	public static function save_class_state (ctx:context.Typecore.Typer, t:core.Type.ModuleType) : Void {
		switch (t) {
			case TClassDecl(c):
				function mk_field_restore (f:core.Type.TClassField) {
					function mk_overload_restore (f:core.Type.TClassField) {
						return {name:f.cf_name, kind:f.cf_kind, expr:f.cf_expr, type:f.cf_type, meta:f.cf_meta, params:f.cf_params};
					}
					return {f:f, res:mk_overload_restore(f), overloads:List.map(function (f) { return {fst:f, snd:mk_overload_restore(f)}; }, f.cf_overloads)};
				}
				function restore_field (tmp) : core.Type.TClassField {
					var f:core.Type.TClassField = tmp.f; var res = tmp.res; var overloads = tmp.overloads;
					function restore_field_ (tmp): core.Type.TClassField {
						var f:core.Type.TClassField = tmp.fst;
						var name = tmp.snd.name; var kind = tmp.snd.kind; var expr = tmp.snd.expr; var t = tmp.snd.type; var meta = tmp.snd.meta; var params = tmp.snd.params;
						f.cf_name = name; f.cf_kind = kind; f.cf_expr = expr; f.cf_type = t; f.cf_meta = meta; f.cf_params = params;
						return f;
					}
					var f = restore_field_({fst:f, snd:res});
					f.cf_overloads = List.map(restore_field_, overloads);
					return f;
				}
				function mk_pmap (lst:ImmutableList<core.Type.TClassField>) : PMap<String, core.Type.TClassField> {
					return List.fold_left( function (pmap:PMap<String, core.Type.TClassField>, f:core.Type.TClassField) { return PMap.add(f.cf_name, f, pmap); }, PMap.empty(), lst);
				}
				var meta = c.cl_meta; var path = c.cl_path; var ext = c.cl_extern; var over = c.cl_overrides;
				var sup = c.cl_super; var impl = c.cl_implements;
				var csr = ocaml.Option.map(mk_field_restore, c.cl_constructor);
				var ofr = List.map(mk_field_restore, c.cl_ordered_fields);
				var osr = List.map(mk_field_restore, c.cl_ordered_statics);
				var init = c.cl_init;
				c.cl_restore = function () {
					c.cl_super = sup;
					c.cl_implements = impl;
					c.cl_meta = meta;
					c.cl_extern = ext;
					c.cl_path = path;
					c.cl_init = init;
					c.cl_ordered_fields = List.map(restore_field, ofr);
					c.cl_ordered_statics = List.map(restore_field, osr);
					c.cl_fields = mk_pmap(c.cl_ordered_fields);
					c.cl_statics = mk_pmap(c.cl_ordered_statics);
					c.cl_constructor = ocaml.Option.map(restore_field, csr);
					c.cl_overrides = over;
					c.cl_descendants = Tl;
				}

			case _:
		}
	}
	/* PASS 2 begin */

	public static function remove_generic_base (ctx:context.Typecore.Typer, t:core.Type.ModuleType) : Void {
		switch (t) {
			case TClassDecl(c) if (FiltersCommon.is_removable_class(c)):
				c.cl_extern = true;
			case _:
		}
	}

	/* Removes extern and macro fields, also checks for Void fields */
	public static function remove_extern_fields (ctx:context.Typecore.Typer, t:core.Type.ModuleType) : Void {
		switch (t) {
			case TClassDecl(c):
				if (!context.Common.defined(ctx.com, DocGen)) {
					c.cl_ordered_fields = List.filter (function (f:core.Type.TClassField) {
						var b = context.Typecore.is_removable_field(ctx, f);
						if (b) {
							c.cl_fields = PMap.remove(f.cf_name, c.cl_fields);
						}
						return !b;
					}, c.cl_ordered_fields);
					c.cl_ordered_statics = List.filter (function (f:core.Type.TClassField) {
						var b = context.Typecore.is_removable_field(ctx, f);
						if (b) {
							c.cl_statics = PMap.remove(f.cf_name, c.cl_statics);
						}
						return !b;
					}, c.cl_ordered_statics);
				}
			case _:
		}
	}
	/* PASS 2 end */

	/* PASS 3 begin */

	/* Checks if a private class' path clashes with another path */
	public static function check_private_path (ctx:context.Typecore.Typer, t:core.Type.ModuleType) : Void {
		switch (t) {
			case TClassDecl(c) if (c.cl_private):
				var rpath = new core.Path(c.cl_module.m_path.a, "_"+c.cl_module.m_path);
				if (Hashtbl.mem(ctx.g.types_module, rpath)) {
					core.Error.error("This private class name will clash with "+core.Globals.s_type_path(rpath), c.cl_pos);
				}
			case _:
		}
	}

	/* Rewrites class or enum paths if @:native metadata is set */
	public static function apply_native_paths (ctx:context.Typecore.Typer, t:core.Type.ModuleType) : Void {
		function get_real_name (meta:core.Ast.Metadata, name:String) : {meta:core.Ast.MetadataEntry, name:String} {
			var tmp = get_native_name(meta);
			var name_ = tmp.name; var p = tmp.pos;
			var _meta:core.Ast.MetadataEntry = {name:RealPath, params:[{expr:EConst(CString(name)), pos:p}], pos:p};
			return {meta:_meta, name:name_};
		}
		function get_real_path (meta:core.Ast.Metadata, path:core.Path) : {meta:core.Ast.MetadataEntry, path:core.Path} {
			var tmp = get_native_name(meta);
			var name = tmp.name; var p = tmp.pos;
			var _meta:core.Ast.MetadataEntry = {name:RealPath, params:[{expr:EConst(CString(core.Globals.s_type_path(path))), pos:p}], pos:p};
			return {meta:_meta, path:core.Path.parse_path(name)};
		}
		try {
			switch (t) {
				case TClassDecl(c):
					var did_change = new Ref(false);
					function field (cf:core.Type.TClassField) {
						try {
							var _tmp = get_real_name(cf.cf_meta, cf.cf_name);
							var meta = _tmp.meta; var name = _tmp.name;
							cf.cf_name = name;
							cf.cf_meta = meta :: cf.cf_meta;
							List.iter(function (cf) { cf.cf_name = name; }, cf.cf_overloads);
							did_change.set(true);
						}
						catch (_:ocaml.Not_found) {
						}
					}
					function fields (cfs:ImmutableList<core.Type.TClassField>, old_map:PMap<String, core.Type.TClassField>) : PMap<String, core.Type.TClassField> {
						did_change.set(false);
						List.iter(field, cfs);
						return
						if (did_change.get()) {
							List.fold_left(function (map:PMap<String, core.Type.TClassField>, f:core.Type.TClassField) {
								return PMap.add(f.cf_name, f, map);
							}, PMap.empty(), cfs);
						}
						else {
							old_map;
						}
					}
					c.cl_fields = fields(c.cl_ordered_fields, c.cl_fields);
					c.cl_statics = fields(c.cl_ordered_statics, c.cl_statics);
					var _tmp = get_real_path(c.cl_meta, c.cl_path);
					var meta = _tmp.meta; var path = _tmp.path;
					c.cl_meta = meta :: c.cl_meta;
					c.cl_path = path;
				case TEnumDecl(e):
					var _tmp = get_real_path(e.e_meta, e.e_path);
					var meta = _tmp.meta; var path = _tmp.path;
					e.e_meta = meta :: e.e_meta;
					e.e_path = path;
				case _:
			}
		}
		catch (_:ocaml.Not_found) {
		}
	}

	/* Adds the __rtti field if required */
	public static function add_rtti (ctx:context.Typecore.Typer, t:core.Type.ModuleType) : Void {
		function has_rtti(c:core.Type.TClass) : Bool {
			return core.Meta.has(Rtti, c.cl_meta) || (switch (c.cl_super) { case None: false; case Some({c:csup}): has_rtti(csup); });
		}
		switch (t) {
			case TClassDecl(c) if (has_rtti(c) && !PMap.mem("__rtti", c.cl_statics)):
				var f = core.Type.mk_field("__rtti", ctx.t.tstring, c.cl_pos, core.Globals.null_pos);
				var str = codegen.Genxml.gen_type_string(ctx.com, t);
				f.cf_expr = Some(core.Type.mk(TConst(TString(str)), f.cf_type, c.cl_pos));
				c.cl_ordered_statics = f :: c.cl_ordered_statics;
				c.cl_statics = PMap.add(f.cf_name, f, c.cl_statics);
			case _:
		}
	}

	/* Adds member field initializations as assignments to the constructor */
	public static function add_field_inits (reserved:PMap<String, Bool>, ctx:context.Typecore.Typer, t:core.Type.ModuleType) : Void {
		var is_as3 = context.Common.defined(ctx.com, As3) && !(ctx.in_macro);
		function apply (c:core.Type.TClass) : Void {
			var ethis = core.Type.mk(TConst(TThis), TInst(c, List.map(function (p) { return p.t; }, c.cl_params)), c.cl_pos);
			// TODO: we have to find a variable name which is not used in any of the functions
			var v = core.Type.alloc_var("_g", ethis.etype, ethis.epos);
			var need_this = new Ref(false);
			var _tmp = List.fold_left (function (tmp:{fst:ImmutableList<core.Type.TClassField>, snd:ImmutableList<core.Type.TClassField>}, cf:core.Type.TClassField) {
				var inits = tmp.fst; var fields = tmp.snd;
				return switch [cf.cf_kind, cf.cf_expr] {
					case [Var(_), Some(_)]:
						if (is_as3) {
							{fst:inits, snd:cf::fields};
						}
						else {
							{fst:cf::inits, snd:fields};
						}
					case [Method(MethDynamic), Some(e)] if (is_as3):
						/* TODO : this would have a better place in genSWF9 I think - NC */
						/* we move the initialization of dynamic functions to the constructor and also solve the
							'this' problem along the way */
						function use_this(v:core.Type.TVar, e:core.Type.TExpr) : core.Type.TExpr {
							return switch (e.eexpr) {
								case TConst(TThis):
									need_this.set(true);
									core.Type.mk(TLocal(v), v.v_type, e.epos);
								case _:
									core.Type.map_expr(use_this.bind(v), e);
							}
						}
						var e = core.Type.map_expr(use_this.bind(v), e);
						var cf2 = cf.with({cf_expr:Some(e)});
						// if the method is an override, we have to remove the class field to not get invalid overrides
						var fields = if (List.memq(cf, c.cl_overrides)) {
							c.cl_fields = PMap.remove(cf.cf_name, c.cl_fields);
							fields;
						}
						else {
							cf2 :: fields;
						}
						{fst:cf2::inits, snd:fields};
					case _:
						{fst:inits, snd:cf::fields};
				}
			}, {fst:Tl, snd:Tl}, c.cl_ordered_fields);
			var inits = _tmp.fst; var fields = _tmp.snd;
			c.cl_ordered_fields = List.rev(fields);
			switch (inits) {
				case []:
				case _:
					var el = List.map(function (cf:core.Type.TClassField) {
						return switch (cf.cf_expr) {
							case None: trace("Shall not be seen"); std.Sys.exit(255); throw false;
							case Some(e):
								var lhs = core.Type.mk(TField(ethis, FInstance(c, List.map(function (p) { return p.t; }, c.cl_params), cf)), cf.cf_type, e.epos);
								cf.cf_expr = None;
								var eassign = core.Type.mk(TBinop(OpAssign, lhs, e), e.etype, e.epos);
								if (is_as3) {
									var echeck = core.Type.mk(TBinop(OpEq, lhs, core.Type.mk(TConst(TNull), lhs.etype, e.epos)), ctx.com.basic.tbool, e.epos);
									core.Type.mk(TIf(echeck, eassign, None), eassign.etype, e.epos);
								}
								else {
									eassign;
								}
						}
					}, inits);
					var el = (need_this.get()) ? core.Type.mk(TVar(v, Some(ethis)), ethis.etype, ethis.epos) :: el : el;
					var cf = switch (c.cl_constructor) {
						case None:
							var ct:core.Type.T = TFun({args:[], ret:ctx.com.basic.tvoid});
							var ce = core.Type.mk(TFunction({
								tf_args: Tl,
								tf_type: ctx.com.basic.tvoid,
								tf_expr: core.Type.mk(TBlock(el), ctx.com.basic.tvoid, c.cl_pos)
							}), ct, c.cl_pos);
							var ctor = core.Type.mk_field("new", ct, c.cl_pos, core.Globals.null_pos);
							ctor.cf_kind = Method(MethNormal);
							ctor.with({cf_expr:Some(ce)});
						case Some(cf):
							switch (cf.cf_expr) {
								case Some({eexpr:TFunction(f)}):
									var bl:ImmutableList<core.Type.TExpr> = switch (f.tf_expr) { case {eexpr:TBlock(b)}: b; case x: [x]; };
									var ce = core.Type.mk(TFunction(f.with({tf_expr:core.Type.mk(TBlock(List.append(el, bl)), ctx.com.basic.tvoid, c.cl_pos)})), cf.cf_type, cf.cf_pos);
									cf.with({cf_expr:Some(ce)});
								case _:
									trace("Shall not be seen"); std.Sys.exit(255); throw false;
							}
					}
					var config = optimization.AnalyzerConfig.get_field_config(ctx.com, c, cf);
					optimization.Analyzer.Run.run_on_field(ctx, config, c, cf);
					switch (cf.cf_expr) {
						case Some(e):
							// This seems a bit expensive, but hopefully constructor expressions aren't that massive.
							var e = rename_local_vars(ctx, reserved, e);
							var e = optimization.Optimizer.sanitize(ctx.com, e);
							cf.cf_expr = Some(e);
						case _:
					}
					c.cl_constructor = Some(cf);
			}
		}
		switch (t) {
			case TClassDecl(c) : apply(c);
			case _:
		}
	}

	/* Adds the __meta__ field if required */
	public static function add_meta_field (ctx:context.Typecore.Typer, t:core.Type.ModuleType) : Void {
		switch (t) {
			case TClassDecl(c):
				switch (core.Texpr.build_metadata(ctx.com.basic, t)) {
					case None:
					case Some(e):
						context.Common.add_feature(ctx.com, "has_metadata");
						var cf = core.Type.mk_field("__meta__", e.etype, e.epos, core.Globals.null_pos);
						cf.cf_expr = Some(e);
						function can_deal_with_interface_metadata () : Bool {
							return switch (ctx.com.platform) {
								case Flash if (context.Common.defined(ctx.com, As3)): false;
								case Cs, Java: false;
								case _: true;
							}
						}
						if (c.cl_interface && !can_deal_with_interface_metadata()) {
							// borrowed from gencommon, but I did wash my hands afterwards
							var path = new core.Path(c.cl_path.a, c.cl_path.b+"_HxMeta");
							var ncls = core.Type.mk_class(c.cl_module, path, c.cl_pos, core.Globals.null_pos);
							ncls.cl_ordered_statics = cf :: ncls.cl_ordered_statics;
							ncls.cl_statics = PMap.add(cf.cf_name, cf, ncls.cl_statics);
							ctx.com.types = List.append(ctx.com.types, [TClassDecl(ncls)]);
							c.cl_meta = ({name:Custom(":hasMetadata"), params:Tl, pos:e.epos} : core.Ast.MetadataEntry) :: c.cl_meta;
						}
						else {
							c.cl_ordered_statics = cf :: c.cl_ordered_statics;
							c.cl_statics = PMap.add(cf.cf_name, cf, c.cl_statics);
						}
				}
			case _:
		}
	}

	/* Checks for Void class fields */
	public static function check_void_field (ctx:context.Typecore.Typer, t:core.Type.ModuleType) : Void {
		switch (t) {
			case TClassDecl(c):
				function check(f:core.Type.TClassField) {
					switch (core.Type.follow(f.cf_type)) {
						case TAbstract({a_path:{a:[], b:"Void"}}, _):
							core.Error.error("Fields of type Void are not allowed", f.cf_pos);
						case _:
					}
				}
				List.iter(check, c.cl_ordered_fields);
				List.iter(check, c.cl_ordered_statics);
			case _:
		}
	}

	/* Interfaces have no 'super', but can extend many other interfaces.
		This makes the first extended (implemented) interface the super for efficiency reasons (you can get one for 'free')
		and leaves the remaining ones as 'implemented' */
	public static function promote_first_interface_to_super (ctx:context.Typecore.Typer, t:core.Type.ModuleType) : Void {
		switch (t) {
			case TClassDecl(c) if (c.cl_interface):
				switch (c.cl_implements) {
					case {c:{cl_path:{a:["cpp", "rtti"]}}} :: _:
					case first_interface :: remaining:
						c.cl_super = Some(first_interface);
						c.cl_implements = remaining;
					case _:
				}
			case _:
		}
	}

	public static function commit_features (ctx:context.Typecore.Typer, t:core.Type.ModuleType) : Void {
		var m = core.Type.t_infos(t).mt_module;
		Hashtbl.iter(function (k, v) {
			context.Common.add_feature(ctx.com, k);
		}, m.m_extra.m_features);
	}

	public static function check_reserved_type_paths (ctx:context.Typecore.Typer, t:core.Type.ModuleType) : Void {
		function check (path:core.Path, pos:core.Globals.Pos) {
			if (List.mem(path, ctx.com.config.pf_reserved_type_paths)) {
				ctx.com.warning("Type path "+core.Globals.s_type_path(path)+ " is reserved on this target", pos);
			}
		}
		switch (t) {
			case TClassDecl(c) if (!c.cl_extern): check(c.cl_path, c.cl_pos);
			case TEnumDecl(e) if (!e.e_extern): check(e.e_path, e.e_pos);
			case _:
		}
	}

	/* Removes interfaces tagged with @:remove metadata */
	public static function check_remove_metadata (ctx:context.Typecore.Typer, t:core.Type.ModuleType) : Void {
		switch (t) {
			case TClassDecl(c):
				c.cl_implements = List.filter(function (tmp:{c:core.Type.TClass, params:core.Type.TParams}) { var c = tmp.c; return !core.Meta.has(Remove, c.cl_meta); }, c.cl_implements);
			case _:
		}
	}

	/* PASS 3 end */

	public static final pp_counter = new Ref(1);

	public static function is_cached (t:core.Type.ModuleType) : Bool {
		var m = core.Type.t_infos(t).mt_module.m_extra;
		if (m.m_processed == 0) { m.m_processed = pp_counter.get(); }
		return m.m_processed != pp_counter.get();
	}

	public static function apply_filters_once (ctx:context.Typecore.Typer, filters:ImmutableList<core.Type.TExpr->core.Type.TExpr>, t:core.Type.ModuleType) : Void {
		if (!is_cached(t)) {
			FiltersCommon.run_expression_filters(ctx, filters, t);
		}
	}

	public static inline function next_compilation () : Void {
		pp_counter.set(pp_counter.get() + 1);
	}

	public static function filter_timer (detailed:Bool, s:ImmutableList<String>) : Void->Void {
		return core.Timer.timer((detailed) ? "filters"::s : ["filters"]);
	}

	public static function run (com:context.Common.Context, tctx:context.Typecore.Typer, main:Option<core.Type.TExpr>) : Void {
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
				trace("TODO: finish Filters.run"); std.Sys.exit(255); throw false;
				// SetHXGen.run_filter com new_types;
				// filters @ [
				// 	TryCatchWrapper.configure_cs com
				// ]
			case Java:
				trace("TODO: finish Filters.run"); std.Sys.exit(255); throw false;
				// SetHXGen.run_filter com new_types;
				// filters @ [
				// 	TryCatchWrapper.configure_java com
				// ]
			case Js:
				filters = List.append(filters, [JsExceptions.init(tctx)]);
			case _: filters;
		}
		var t = filter_timer(detail_times, ["expr 1"]);
		List.iter(FiltersCommon.run_expression_filters.bind(tctx, filters), new_types);
		t();
		// PASS 1.5: pre-analyzer type filters
		var filters:ImmutableList<core.Type.ModuleType->Void> = switch (com.platform) {
			case Cs:
				// [
				// 	check_cs_events tctx.com;
				// 	DefaultArguments.run com;
				// ]
				trace("TODO: finish Filters.run"); std.Sys.exit(255); throw false;
			case Java:
				// [
				// 	DefaultArguments.run com;
				// ]
				trace("TODO: finish Filters.run"); std.Sys.exit(255); throw false;
			case _: [];
		}
		var t = filter_timer(detail_times, ["type 1"]);
		List.iter(function (f) { List.iter(f, new_types); }, filters);
		t();
		if (com.platform != Cross) {
			optimization.Analyzer.Run.run_on_types(tctx, new_types);
		}
		var reserved = collect_reserved_local_names(com);
		var filters:ImmutableList<core.Type.TExpr->core.Type.TExpr> = [
			optimization.Optimizer.sanitize.bind(com),
			(com.config.pf_add_final_return) ? add_final_return : function (e) { return e;},
			rename_local_vars.bind(tctx, reserved),
			mark_switch_break_loops
		];
		var t = filter_timer(detail_times, ["expr 2"]);
		List.iter(FiltersCommon.run_expression_filters.bind(tctx, filters), new_types);
		t();
		next_compilation();
		var t = filter_timer(detail_times, ["callbacks"]);
		List.iter(function (f) { f(); }, List.rev(com.callbacks.before_dce)); // macros onGenerate etc.
		t();
		var t = filter_timer(detail_times, ["save state"]);
		List.iter(save_class_state.bind(tctx), new_types);
		t();
		var t = filter_timer(detail_times, ["type 2"]);
		// PASS 2: type filters pre-DCE
		List.iter( function (t:core.Type.ModuleType) {
			remove_generic_base(tctx, t);
			remove_extern_fields(tctx, t);
			codegen.Codegen.update_cache_dependencies(t);
			// check @:remove metadata before DCE so it is ignored there (issue #2923)
			check_remove_metadata(tctx, t);

		}, com.types);
		t();
		var t = filter_timer(detail_times, ["dce"]);
		// DCE
		var dce_mode = if (context.Common.defined(com, As3)) {
			"no";
		}
		else {
			try { context.Common.defined_value(com, Dce); }
			catch (_:Any) { "no"; }
		}
		switch (dce_mode) {
			case "full": optimization.Dce.run(com, main, !context.Common.defined(com, Interp));
			case "std": optimization.Dce.run(com, main, false);
			case "no": optimization.Dce.fix_accessors(com);
			case _: throw ("Unknown DCE mode" + dce_mode);
		}
		t();
		// PASS 3: type filters post-DCE
		var type_filters:ImmutableList<(context.Typecore.Typer, core.Type.ModuleType)->Void> = [
			check_private_path,
			apply_native_paths,
			add_rtti,
			switch (com.platform) { case Java, Cs: function (_,_) {}; case _: add_field_inits.bind(reserved); },
			switch (com.platform) { case Hl: function (_,_) {}; case _: add_meta_field; },
			check_void_field,
			switch (com.platform) { case Cpp: promote_first_interface_to_super; case _: function (_,_) {}; },
			commit_features,
			(com.config.pf_reserved_type_paths != Tl) ? check_reserved_type_paths : function (_,_) {}
		];
		var type_filters = switch (com.platform) {
			case Cs:
				// type_filters @ [ fun _ t -> InterfaceProps.run t ]
				trace("TODO: finish Filters.run"); std.Sys.exit(255); throw false;
			case Js:
				JsExceptions.inject_callstack(com, type_filters);
			case _:
				type_filters;
		}
		var t = filter_timer(detail_times, ["type 3"]);
		List.iter(function (t:core.Type.ModuleType) {
			List.iter(function (f) { f(tctx, t); }, type_filters);
		}, com.types);
		t();
	}
}