package typing;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
import ocaml.Hashtbl;
import ocaml.List;
import ocaml.PMap;
import ocaml.Ref;

using equals.Equal;
using ocaml.Cloner;

enum State {
	Generating;
	Done;
	NotYet;
}

class Typer {

	public static function type_type (ctx:context.Typecore.Typer, tpath:core.Path, p:core.Globals.Pos) {
		trace("TODO: context.Typer.type_type");
		return null;
		// return type_module_type ctx (Typeload.load_type_def ctx p { tpackage = fst tpath; tname = snd tpath; tparams = []; tsub = None }) None p
	}

	public static function type_expr (ctx:context.Typecore.Typer, e:core.Ast.Expr, with_type:context.Typecore.WithType) : core.Type.TExpr {
		trace("TODO: typing.Type.type_expr");
		return null;
	}

	public static function get_main(ctx:context.Typecore.Typer, types:ImmutableList<core.Type.ModuleType>) : Option<core.Type.TExpr> {
		switch (ctx.com.main_class) {
			case None: return None;
			case Some(cl):
				var t = typing.Typeload.load_type_def(ctx, core.Globals.null_pos, {tpackage:cl.a, tname:cl.b, tparams:[], tsub:None});
				var fmode : core.Type.TFieldAccess = null;
				var ft : core.Type.T = null;
				var r: core.Type.T = null;
				switch (t) {
					case TEnumDecl(_), TTypeDecl(_), TAbstractDecl(_):
						core.Error.error("Invalid -main : "+core.Globals.s_type_path(cl)+ " is not a class", core.Globals.null_pos);
					case TClassDecl(c):
						try  {
							var f = PMap.find("main",c.cl_statics);
							var t = core.Type.field_type(f);
							switch (t) {
								case TFun({args:[], ret:_r}):
									fmode = core.Type.TFieldAccess.FStatic(c,f);
									ft = t;
									r = _r;
								default:
									core.Error.error("Invalid -main : " + core.Globals.s_type_path(cl) + " does not have static function main", c.cl_pos);
							}
						}
						catch (_:ocaml.Not_found) {
							core.Error.error("Invalid -main : " + core.Globals.s_type_path(cl) + " does not have static function main", c.cl_pos);
						}
				}
				var emain = type_type(ctx, cl, core.Globals.null_pos);
				var main = core.Type.mk(core.Type.TExprExpr.TCall(core.Type.mk(core.Type.TExprExpr.TField(emain, fmode), ft, core.Globals.null_pos), []), r, core.Globals.null_pos);
				main = try {
					var et = List.find(function (t) {
						return core.Type.t_path(t).equals(new core.Path(["haxe"], "EntryPoint"));
					}, types);
					var ec = switch (et) {
						case TClassDecl(c): c;
						default: throw false;
					}
					var ef = PMap.find("run", ec.cl_statics);
					var p = core.Globals.null_pos;
					var _et = core.Type.mk(
						core.Type.TExprExpr.TTypeExpr(et),
						core.Type.T.TAnon({a_fields: new Map<String, core.Type.TClassField>(), a_status: new ocaml.Ref(core.Type.AnonStatus.Statics(ec))}),
						p
					);
					var call = core.Type.mk(
						core.Type.TExprExpr.TCall(
							core.Type.mk(
								core.Type.TExprExpr.TField(_et, core.Type.TFieldAccess.FStatic(ec, ef)),
								ef.cf_type, p), []),
						ctx.t.tvoid,
						p
					);
					core.Type.mk(core.Type.TExprExpr.TBlock([main, call]),ctx.t.tvoid,p);
				}
				catch (_:ocaml.Not_found) {
					main;
				}
				return Some(main);
		}
	}

	public static function finalize (ctx:context.Typecore.Typer) : Void {
		context.Typecore.flush_pass(ctx, PFinal, "final");
		var fl = ctx.com.callbacks.after_typing;
		if (List.length(fl) > 0) {
			function loop(handled_types:ImmutableList<core.Type.ModuleType>) {
				var all_types = Hashtbl.fold(function (_, m, acc:ImmutableList<core.Type.ModuleType>) {
					return List.append(m.m_types, acc);
				}, ctx.g.modules, []);
				switch (List.filter(function (mt:core.Type.ModuleType){ return !List.memq(mt, handled_types);}, all_types)) {
					case []:
					case new_types:
						List.iter(function (f) { f(new_types);}, fl);
						context.Typecore.flush_pass(ctx, PFinal, "final");
						loop(all_types);
				}
			}
			loop([]);
		}
	}

	public static function format_string(ctx:context.Typecore.Typer, s:String, p:core.Globals.Pos) : core.Ast.Expr {
		trace("TODO: typing.Typer.format_string");
		return null;
	}

	public static function sort_types(com:context.Common.Context, modules:Map<core.Path, core.Type.ModuleDef>) : {types:ImmutableList<core.Type.ModuleType>, modules:ImmutableList<core.Type.ModuleDef>} {
		// var types = new Ref<ImmutableList<core.Type.ModuleType>>([]);
		var types:ImmutableList<core.Type.ModuleType> = [];
		var states = new Map<core.Path, typing.Typer.State>();
		
		function state (p:core.Path) {
			if (states.exists(p)) {
				return states.get(p);
			}
			return NotYet;
		}
		var statics = new Map<{path:core.Path, s:String}, Bool>();

		var walk_static_field:core.Path->core.Type.TClass->core.Type.TClassField->Void;
		var walk_expr:core.Path->core.Type.TExpr->Void;
		var walk_class:core.Path->core.Type.TClass->Void;

		function loop (t:core.Type.ModuleType) {
			var p = core.Type.t_path(t);
			switch (state(p)) {
				case Done:
				case Generating:
					com.warning("Warning : maybe loop in static generation of " + core.Globals.s_type_path(p), core.Type.t_infos(t).mt_pos);
				case NotYet:
					Hashtbl.add(states,p, Generating);
					var t = switch(t) {
						case TClassDecl(c):
							walk_class(p, c);
							t;
						case TEnumDecl(_), TTypeDecl(_), TAbstractDecl(_):
							t;
					}
					Hashtbl.replace(states, p, Done);
					types = t::types;
			}
		}
		function loop_class (p:core.Path, c:core.Type.TClass) {
			if (c.cl_path != p) {
				loop(TClassDecl(c));
			}
		}
		function loop_enum (p:core.Path, e:core.Type.TEnum) {
			if (e.e_path != p) {
				loop(TEnumDecl(e));
			}
		}
		function loop_abstract (p:core.Path, a:core.Type.TAbstract) {
			if (a.a_path != p) {
				loop(TAbstractDecl(a));
			}
		}
		walk_static_field = function (p:core.Path, c:core.Type.TClass, cf:core.Type.TClassField) {
			switch (cf.cf_expr) {
				case None:
				case Some(e):
					if (PMap.mem({path:c.cl_path, s:cf.cf_name}, statics)) {}
					else {
						statics = PMap.add({s:cf.cf_name, path:c.cl_path}, true, statics);
						walk_expr(p, e);
					}
			}
		}

		walk_expr = function (p:core.Path, e:core.Type.TExpr) {
			switch (e.eexpr) {
				case TTypeExpr(t):
					switch (t) {
						case TClassDecl(c): loop_class(p, c);
						case TEnumDecl(en): loop_enum(p, en);
						case TAbstractDecl(a): loop_abstract(p, a);
						case TTypeDecl(_): throw false;
					}
				case TNew(c,_,_):
					core.Type.iter(walk_expr.bind(p), e);
					loop_class(p, c);
					function inner_loop (c:core.Type.TClass) {
						if (PMap.mem({path:c.cl_path, s:"new"}, statics)) {}
						else {
							statics = PMap.add({s:"new", path:c.cl_path}, true, statics);
							switch (c.cl_constructor) {
								case Some(v):
									switch (v.cf_expr) {
										case Some(ex): walk_expr(p, ex);
										case None:
									}
								case None:
							}
							switch (c.cl_super) {
								case Some(v): inner_loop(v.c);
								case None:
							}
						}
					}
					inner_loop(c);
				case TField(e1, FStatic(c,cf)):
					walk_expr(p, e1);
					walk_static_field(p, c, cf);
				default:
					core.Type.iter(walk_expr.bind(p), e);
			}
		}

		walk_class = function (p:core.Path, c:core.Type.TClass) {
			switch (c.cl_super) {
				case None:
				case Some(v): loop_class(p, v.c);
			}
			List.iter(function(implement) {
				loop_class(p, implement.c);
			}, c.cl_implements);
			switch (c.cl_init) {
				case None:
				case Some(e): walk_expr(p, e);
			}
			PMap.iter(function (_, f:core.Type.TClassField) {
				switch (f.cf_expr) {
					case None:
					case Some(e):
						switch (e.eexpr) {
							case TFunction(_):
							default: walk_expr(p, e);
						}
				}
			}, c.cl_statics);
		}
		var sorted_modules = List.sort(function (m1:core.Type.ModuleDef, m2:core.Type.ModuleDef) {
			return core.Path.compare(m1.m_path, m2.m_path);
		}, Hashtbl.fold(function (_, m, acc:ImmutableList<core.Type.ModuleDef>) { return m::acc; }, modules, []));
		List.iter(function (m) { List.iter(loop, m.m_types); }, sorted_modules);
		
		return {types:List.rev(types), modules:sorted_modules};
	}

	public static function generate(ctx:context.Typecore.Typer) : {main:Option<core.Type.TExpr>, types:ImmutableList<core.Type.ModuleType>, modules:ImmutableList<core.Type.ModuleDef>} {
		var sorted = sort_types(ctx.com, ctx.g.modules);
		var types = sorted.types;
		var modules = sorted.modules;
		return {main:get_main(ctx, types), types:types, modules:modules};
	}

	// ----------------------------------------------------------------------
	// TYPER INITIALIZATION

	public static function create (com:context.Common.Context) : context.Typecore.Typer {
		var ctx:context.Typecore.Typer = {
			com : com,
			t : com.basic,
			g : {
				core_api : None,
				macros : None,
				modules : new Map<core.Path, core.Type.ModuleDef>(),
				types_module : new Map<core.Path, core.Path>(),
				type_patches : new Map<core.Path, {map:Map<{s:String, b:Bool}, context.Typecore.TypePatch>, tp:context.Typecore.TypePatch}>(),
				global_metadata : [],
				module_check_policies : [],
				delayed : [],
				debug_delayed : [],
				delayed_macros : [],
				doinline : (com.display.dms_inline && !context.Common.defined(com, NoInline)),
				hook_generate : [],
				get_build_infos : function () {return None;},
				std : core.Type.null_module,
				global_using : [],
				do_inherit : typing.MagicTypes.on_inherit,
				do_create : typing.Typer.create,
				do_macro : typing.MacroContext.type_macro,
				do_load_module : typing.Typeload.load_module,
				do_optimize : optimization.Optimizer.reduce_expression,
				do_build_instance : typing.Typeload.build_instance,
				do_format_string : format_string,
				do_finalize : finalize,
				do_generate : generate,
			},
			m : {
				curmod : core.Type.null_module,
				module_types : [],
				module_using : [],
				module_globals : new Map<String, {a:core.Type.ModuleType, b:String, pos:core.Globals.Pos}>(),
				wildcard_packages : [],
				module_imports : [],
			},
			is_display_file : false,
			meta : [],
			this_stack : [],
			with_type_stack : [],
			call_argument_stack : [],
			pass: context.Typecore.TyperPass.PBuildModule,
			macro_depth : 0,
			untyped_ : false,
			curfun : context.Typecore.CurrentFun.FunStatic,
			in_loop : false,
			in_display : false,
			in_macro : context.Common.defined(com, Macro),
			ret : core.Type.mk_mono(),
			locals : new Map<String, core.Type.TVar>(),
			type_params : [],
			curclass : core.Type.null_class(),
			curfield : core.Type.null_field(),
			tthis : core.Type.mk_mono(),
			opened : [],
			vthis : None,
			in_call_args : false,
			on_error : function (ctx:context.Typecore.Typer, msg:String, p:core.Globals.Pos) {
				ctx.com.error(msg, p);
			}
		};
		ctx.g.std = try {
			// problem here
			typing.Typeload.load_module(ctx, new core.Path([],"StdTypes"), core.Globals.null_pos);
		}
		catch (e:core.Error) {
			switch (e.msg) {
				case Module_not_found(p) if (p.equals(new core.Path([], "StdTypes"))): 
					core.Error.error("Standard library not found", core.Globals.null_pos);
				default: throw e;
			}
		}
		// We always want core types to be available so we add them as default imports (issue #1904 and #3131).
		ctx.m.module_types = List.map (function (t:core.Type.ModuleType) { return {mt:t, pos:core.Globals.null_pos};}, ctx.g.std.m_types);
		List.iter(function (t:core.Type.ModuleType) {
			switch(t) {
				case TAbstractDecl(a):
					switch (a.a_path.b) {
						case "Void": ctx.t.tvoid = TAbstract(a, []);
						case "Float": ctx.t.tfloat = TAbstract(a, []);
						case "Int": ctx.t.tint = TAbstract(a, []);
						case "Bool": ctx.t.tbool = TAbstract(a, []);
						case "Dynamic": core.Type.t_dynamic_def.set(TAbstract(a, List.map(function (ap) {return ap.t; }, a.a_params)));
						case "Null":
							function mk_null (t) : core.Type.T {
								return try {
									if (!core.Type.is_null(true, t)) {
										TAbstract(a, [t]);
									}
									else {
										t;
									}
								}
								catch (_:ocaml.Exit) {
									// don't force lazy evaluation
									var r = new ocaml.Ref(core.Type.lazy_available(core.Type.t_dynamic));
									r.set(core.Type.lazy_wait(
										function () : core.Type.T {
											var t : core.Type.T = !core.Type.is_null(t) ? TAbstract(a, [t]) : t;
											r.set(core.Type.lazy_available(t));
											return t;
										}
									));
									TLazy(r);
								}
							}
							ctx.t.tnull = mk_null;
						case _:
					}
				case TEnumDecl(_), TClassDecl(_), TTypeDecl(_):
			}
		}, ctx.g.std.m_types);

		var m = typing.Typeload.load_module(ctx, new core.Path([], "String"), core.Globals.null_pos);
		switch (m.m_types) {
			case [TClassDecl(c)]: ctx.t.tstring = TInst(c, []);
			case _: throw false;
		}
		
		m = typing.Typeload.load_module(ctx, new core.Path([], "Array"), core.Globals.null_pos);
		try {
			List.iter(function (t:core.Type.ModuleType) {
				switch (t) {
					case TClassDecl(c={cl_path:path}) if (path.equals(new core.Path([], "Array"))):
						ctx.t.tarray = function (t) { return TInst(c, [t]); }
						throw ocaml.Exit.instance;
					case _:
				}
			}, m.m_types);
			throw false;
		}
		catch (_:ocaml.Exit) {}

		m = typing.Typeload.load_module(ctx, new core.Path(["haxe"], "EnumTools"), core.Globals.null_pos);
		switch (m.m_types) {
			case [TClassDecl(c1), TClassDecl(c2)]: 
				ctx.g.global_using = {a:c1, pos:c1.cl_pos} :: ({a:c2, pos:c2.cl_pos} :: ctx.g.global_using);
			case [TClassDecl(c1)]:
				var m = typing.Typeload.load_module(ctx, new core.Path(["haxe"], "EnumValueTools"), core.Globals.null_pos);
				switch (m.m_types) {
					case [TClassDecl(c2)]:
						ctx.g.global_using = {a:c1, pos:c1.cl_pos} :: ({a:c2, pos:c2.cl_pos} :: ctx.g.global_using);
					case _: throw false;
				}
			case _: throw false;
		}
		return ctx;
	}
}