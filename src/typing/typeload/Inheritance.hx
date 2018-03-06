package typing.typeload;

import haxe.ds.ImmutableList;
import haxe.ds.Option;

import ocaml.List;
import ocaml.PMap;
import ocaml.Ref;
using equals.Equal;
using ocaml.Cloner;

class Inheritance {

	public static function check_extends (ctx:context.Typecore.Typer, c:core.Type.TClass, t:core.Type.T, p:core.Globals.Pos) : {c:core.Type.TClass, params:core.Type.TParams} {
		return switch (core.Type.follow(t)) {
			case TInst({cl_path:{a:[], b:"Array"}, cl_extern:basic_extern}, _),
				TInst({cl_path:{a:[], b:"String"}, cl_extern:basic_extern}, _),
				TInst({cl_path:{a:[], b:"Date"}, cl_extern:basic_extern}, _),
				TInst({cl_path:{a:[], b:"Xml"}, cl_extern:basic_extern}, _) if (!c.cl_extern && basic_extern):
					core.Error.error("Cannot extend basic class", p);
			case TInst(csup, params):
				if (core.Type.is_parent(c, csup)) { core.Error.error("Recursive class", p); }
				switch (csup.cl_kind) {
					case KTypeParameter(_):
						if (typing.Typeload.is_generic_parameter(ctx, csup)) { core.Error.error("Extending generic type parameters is no longer allowed in Haxe 4", p); }
						core.Error.error("Cannot extend type parameters", p);
					case _: {c:csup, params:params};
				}
			case _: core.Error.error("Should extend by using a class", p);
		}
	}

	public static function check_interface(ctx:context.Typecore.Typer, c:core.Type.TClass, intf:core.Type.TClass, params:core.Type.TParams) : Void {
		var p = c.cl_pos;
		function check_field (i:String, f:core.Type.TClassField) {
			if (ctx.com.config.pf_overload) {
				List.iter(function (_tmp) {
					switch (_tmp) {
						case f2 if (f != f2):
							check_field(i, f2);
						case _:
					}
				}, f.cf_overloads);
			}
			var is_overload = new Ref(false);
			try {
				var _tmp = typing.Typeload.class_field_no_interf(c, i);
				var t2 = _tmp.fst; var f2 = _tmp.snd;
				var _tmp = if (ctx.com.config.pf_overload && (f2.cf_overloads != Tl || core.Meta.has(Overload, f2.cf_meta))) {
					var overloads = codegen.Overloads.get_overloads(c, i);
					is_overload.set(true);
					var t = core.Type.apply_params(intf.cl_params, params, f.cf_type);
					List.find(function (_tmp) {
						var t1 = _tmp.t; var f1 = _tmp.cf;
						return codegen.Overloads.same_overload_args(t, t1, f, f1);
					}, overloads);
				}
				else {
					{t:t2, cf:f2};
				}
				var t2 = _tmp.t; var f2 = _tmp.cf;
				if (ctx.com.display.dms_collect_data) {
					var h = ctx.com.display_information;
					h.interface_field_implementations = {c1:intf, cf1:f, c2:c, cf2:Some(f2)} :: h.interface_field_implementations;
				}
				core.Type.follow(f2.cf_type); // force evaluation
				var p = switch (f2.cf_expr) { case None: p; case Some(e): e.epos; }
				function mkind (mk:core.Type.MethodKind) : Int {
					return switch(mk) {
						case MethNormal, MethInline: 0;
						case MethDynamic: 1;
						case MethMacro: 2;
					}
				}
				if (f.cf_public && !f2.cf_public && !core.Meta.has(CompilerGenerated, f.cf_meta)) {
					context.Typecore.display_error(ctx, "Field " + i + " should be public as requested by " + core.Globals.s_type_path(intf.cl_path), p);
				}
				/*
					not (unify_kind f2.cf_kind f.cf_kind) ||
					not (match f.cf_kind, f2.cf_kind with Var _ , Var _ -> true | Method m1, Method m2 -> mkind m1 = mkind m2 | _ -> false) then
				*/
				else if (!(switch [f.cf_kind, f2.cf_kind] { case [Var(_), Var(_)]: true; case [Method(m1), Method(m2)]: mkind(m1) == mkind(m2); case _: false; }) || !core.Type.unify_kind(f2.cf_kind, f.cf_kind)) {
					context.Typecore.display_error(ctx, "Field " + i + " has different property access than in " + core.Globals.s_type_path(intf.cl_path) + " (" + core.Type.s_kind(f2.cf_kind) + " should be " + core.Type.s_kind(f.cf_kind) + ")", p);
				}
				else {
					try {
						typing.Typeload.valid_redefinition(ctx, f2, t2, f, core.Type.apply_params(intf.cl_params, params, f.cf_type));
					}
					catch (ue:core.Type.Unify_error) {
						var l = ue.l;
						if (!core.Meta.has(CsNative, c.cl_meta) && c.cl_extern) {
							context.Typecore.display_error(ctx, "Field " + i + " has different type than in " + core.Globals.s_type_path(intf.cl_path), p);
							context.Typecore.display_error(ctx, "Interface field is defined here", f.cf_pos);
							context.Typecore.display_error(ctx, core.Error.error_msg(Unify(l)), p);
						}
					}
				}
			}
			catch (_:ocaml.Not_found) {
				if (!c.cl_interface) {
					var msg = if (is_overload.get()) {
						var ctx = core.Type.print_context();
						var args = switch (core.Type.follow(f.cf_type)) {
							case TFun({args:args}):
								List.join(", ", List.map(function (a:core.Type.TSignatureArg) {
									var n = a.name; var o = a.opt; var t = a.t;
									return ((o) ? "?" : "") + n + " : " + core.Type.s_type(ctx, t);
								}, args));
							case _: trace("Shall not be seen"); std.Sys.exit(255); throw false;
						}
						"No suitable overload for " + i+ "( " +args + "), as needed by " + core.Globals.s_type_path(intf.cl_path) + " was found";
					}
					else {
						"Field " + i + " needed by " + core.Globals.s_type_path(intf.cl_path) + " is missing";
					}
					context.Typecore.display_error(ctx, msg, p);
				}
			}
		}
		PMap.iter(check_field, intf.cl_fields);
		List.iter(function (_tmp) {
			var i2 = _tmp.c;  var p2 = _tmp.params;
			check_interface(ctx, c, i2, List.map(core.Type.apply_params.bind(intf.cl_params, params), p2));
		}, intf.cl_implements);
	}

	public static function check_interfaces (ctx:context.Typecore.Typer, c:core.Type.TClass) {
		switch (c.cl_path) {
			case {a:"Proxy"::_}:
			case _ if (c.cl_extern && core.Meta.has(CsNative, c.cl_meta)):
			case _:
				List.iter(function (_tmp) {
					var intf = _tmp.c; var params = _tmp.params;
					check_interface(ctx, c, intf, params);
				}, c.cl_implements);
		}
	}

	public static function set_heritance (ctx:context.Typecore.Typer, c:core.Type.TClass, herits:ImmutableList<core.Ast.ClassFlag>, p:core.Globals.Pos) : ImmutableList<Void->Void> {
		var is_lib = core.Meta.has(LibType, c.cl_meta);
		var ctx = ctx.with({
			curclass:c,
			type_params: c.cl_params
		});
		var old_meta = c.cl_meta;
		function process_meta (csup:core.Type.TClass) {
			List.iter(function (m:core.Ast.MetadataEntry) {
				switch (m) {
					case {name:Final} if (!core.Meta.has(Hack, c.cl_meta) || switch (c.cl_kind) { case KTypeParameter(_): true; case _: false;} ):
						core.Error.error("Cannot extend a final class", p);
					case {name:AutoBuild, params:el, pos:p}:
						var pos = new core.Globals.Pos(c.cl_pos.pfile, c.cl_pos.pmin, c.cl_pos.pmin);
						c.cl_meta = {name:core.Meta.StrictMeta.Build, params:el, pos:pos} :: m :: c.cl_meta; // prevent display metadata
					case _:
				}
			}, csup.cl_meta);
		}
		function check_cancel_build(csup:core.Type.TClass) {
			switch (csup.cl_build()) {
				case Built:
				case state:
					// for macros reason, our super class is not yet built - see #2177
					// let's reset our build and delay it until we are done
					c.cl_meta = old_meta;
					throw new typing.Typeload.Build_canceled(state);
			}
		}
		var has_interf = new Ref(false);
		/*
		 * resolve imports before calling build_inheritance, since it requires full paths.
		 * that means that typedefs are not working, but that's a fair limitation
		*/
		function resolve_import (tp:core.Ast.PlacedTypePath) : core.Ast.PlacedTypePath {
			var t = tp.tp; var p = tp.pos;
			return switch (t.tpackage) {
				case _::_: {tp:t, pos:p};
				case []:
					try {
						function path_matches (lt) : Bool {
							return core.Type.t_path(lt).b == t.tname;
						}
						var lt = try {
							List.find(path_matches, ctx.m.curmod.m_types);
						}
						catch (_:ocaml.Not_found) {
							var _tmp = List.find(function (mt:{mt:core.Type.ModuleType, pos:core.Globals.Pos}) { return path_matches(mt.mt); }, ctx.m.module_types);
							var t = _tmp.mt; var pi = _tmp.pos;
							context.display.ImportHandling.mark_import_position(ctx.com, pi);
							t;
						}
						{tp:t.with({tpackage:core.Type.t_path(lt).a}), pos:p};
					}
					catch (_:ocaml.Not_found) {
						{tp:t, pos:p};
					}
			}
		}
		var herits = List.filter_map(function (a:core.Ast.ClassFlag) {
			return switch (a) {
				case HExtends(t): Some({is_extends:true, tp:resolve_import(t)});
				case HImplements(t): Some({is_extends:false, tp:resolve_import(t)});
				case _: None;
			}
		}, herits);
		herits = List.filter(ctx.g.do_inherit.bind(ctx, c, p), herits);
		// Pass 1: Check and set relations
		function check_herit (t, is_extends:Bool) : Void->Void {
			if (is_extends) {
				if (c.cl_super != None) {
					core.Error.error("Cannot extend several classes", p);
				}
				var _tmp = check_extends(ctx, c, t, p);
				var csup = _tmp.c; var params = _tmp.params;
				if (c.cl_interface) {
					if (!csup.cl_interface) {
						core.Error.error("Cannot extend by using a class", p);
					}
					c.cl_implements = {c:csup, params:params} :: c.cl_implements;
					if (!has_interf.get()) {
						if (!is_lib) {
							context.Typecore.delay(ctx, PForce, function () { check_interfaces(ctx, c);});
						}
						has_interf.set(true);
					}
				}
				else {
					if (csup.cl_interface) {
						core.Error.error("Cannot extend by using an interface", p);
					}
					c.cl_super = Some({c:csup, params:params});
				}
				return function () {
					check_cancel_build(csup);
					process_meta(csup);
				};
			}
			else {
				return switch (core.Type.follow(t)) {
					case TInst({cl_path:{a:[], b:"ArrayAccess"}, cl_extern:true}, [t]):
						if (c.cl_array_access != None) {
							core.Error.error("Duplicate array access", p);
						}
						c.cl_array_access = Some(t);
						function () {};
					case TInst(intf, params):
						if (core.Type.is_parent(c, intf)) { core.Error.error("Recursive class", p); }
						if (c.cl_interface) { core.Error.error("Interfaces cannot implement another interface (use extends instaed)", p); }
						if (!intf.cl_interface) { core.Error.error("You can only implement an interface", p); }
						c.cl_implements = {c:intf, params:params} :: c.cl_implements;
						if (!has_interf.get() && !is_lib && !core.Meta.has(Custom("$do_not_check_interf"), c.cl_meta)) {
							context.Typecore.delay(ctx, PForce, function() { check_interfaces(ctx, c); });
							has_interf.set(true);
						}
						function () {
							check_cancel_build(intf);
							process_meta(intf);
						};
					case TDynamic(_.get()=>t):
						if (c.cl_dynamic != None) {
							core.Error.error("Cannot have several dynamics",p);
						}
						c.cl_dynamic = Some(t);
						function () {};
					case _:
						core.Error.error("Should implement by using an interface", p);
				};
			}
		}
		var fl = List.filter_map(function (h) {
			var is_extends = h.is_extends; var t = h.tp;
			return try {
				var t = typing.Typeload.load_instance(true, ctx, t, false, p);
				Some(check_herit(t, is_extends));
			}
			catch (err:core.Error) {
				switch (err.msg) {
					case Module_not_found({a:[], b:name}) if (ctx.com.display.dms_display):
						if (context.display.Diagnostics.is_diagnostics_run(ctx)) {
							context.DisplayToplevel.handle_unresolved_identifier(ctx, name, p, true);
						}
						None;
					case _: throw err;
				}
			}
		}, herits);
		return fl;
	}
}