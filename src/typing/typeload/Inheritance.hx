package typing.typeload;

import ocaml.Ref;
using equals.Equal;
using ocaml.Cloner;

class Inheritance {

	public static function check_extends (ctx:context.Typecore.Typer, c:core.Type.TClass, t:core.Type.T, p:core.Globals.Pos) : {c:core.Type.TClass, params:core.Type.TParams} {
		trace("TODO typeing.typeload.Inheritance.check_extends");
		return null;
	}

	public static function check_interfaces (ctx:context.Typecore.Typer, c:core.Type.TClass) {
		trace("TODO typing.typeload.Inheritance.check_interfaces");
	}

	public static function set_heritance (ctx:context.Typecore.Typer, c:core.Type.TClass, herits:Array<core.Ast.ClassFlag>, p:core.Globals.Pos) : Array<Void->Void> {
		var is_lib = core.Meta.has(LibType, c.cl_meta);
		var ctx = ctx.clone();
		ctx.curclass = c;
		ctx.type_params = c.cl_params;
		var old_meta = c.cl_meta;
		function process_meta (csup:core.Type.TClass) {
			ocaml.List.iter(function (m:core.Ast.MetadataEntry) {
				switch (m) {
					case {name:Final} if (!core.Meta.has(Hack, c.cl_meta) || switch (c.cl_kind) { case KTypeParameter(_): true; case _: false;} ):
						core.Error.error("Cannot extend a final class", p);
					case {name:AutoBuild, params:el, pos:p}:
						// var pos = c.cl_pos.clone();
						// pos.pmax = c.cl_pos.pmin;
						var pos = new core.Globals.Pos(c.cl_pos.pfile, c.cl_pos.pmin, c.cl_pos.pmin);
						c.cl_meta.unshift(m);
						c.cl_meta.unshift({name:Build, params:el, pos:pos}); // prevent display metadata
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
			if (t.tpackage.length > 0) { return tp; }
			else {
				try {
					function path_matches (lt) : Bool {
						return core.Type.t_path(lt).b == t.tname;
					}
					var lt = try {
						ocaml.List.find(path_matches, ctx.m.curmod.m_types);
					}
					catch (_:ocaml.Not_found) {
						var _tmp = ocaml.List.find(function (mt:{mt:core.Type.ModuleType, pos:core.Globals.Pos}) { return path_matches(mt.mt); }, ctx.m.module_types);
						var t = _tmp.mt; var pi = _tmp.pos;
						context.display.ImportHandling.mark_import_position(ctx.com, pi);
						t;
					}
					var _t = t.clone();
					_t.tpackage = core.Type.t_path(lt).a;
					return {tp:_t, pos:p};
				}
				catch (_:ocaml.Not_found) {
					return tp;
				}
			}
		}
		var herits = ocaml.List.filter_map(function (a:core.Ast.ClassFlag) {
			return switch (a) {
				case HExtends(t): Some({is_extends:true, tp:resolve_import(t)});
				case HImplements(t): Some({is_extends:false, tp:resolve_import(t)});
				case _: None;
			}
		}, herits);
		herits = herits.filter(ctx.g.do_inherit.bind(ctx, c, p));
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
					c.cl_implements.unshift({c:csup, params:params});
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
					case TInst(_c, ts) if (_c.cl_extern && _c.cl_path.equals(new core.Path([], "ArrayAccess")) && ts.length == 1):
						if (c.cl_array_access != None) {
							core.Error.error("Duplicate array access", p);
						}
						c.cl_array_access = Some(ts[0]);
						function () {};
					case TInst(intf, params):
						if (core.Type.is_parent(c, intf)) { core.Error.error("Recursive class", p); }
						if (c.cl_interface) { core.Error.error("Interfaces cannot implement another interface (use extends instaed)", p); }
						if (!intf.cl_interface) { core.Error.error("You can only implement an interface", p); }
						c.cl_implements.unshift({c:intf, params:params});
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
		return ocaml.List.filter_map(function (h) {
			var is_extends = h.is_extends; var t = h.tp;
			return try {
				var t = typing.Typeload.load_instance(true, ctx, t, false, p);
				Some(check_herit(t, is_extends));
			}
			catch (err:core.Error) {
				switch (err.msg) {
					case Module_not_found(path) if (path.a.length == 0 && ctx.com.display.dms_display):
						if (context.display.Diagnostics.is_diagnostics_run(ctx)) {
							context.DisplayToplevel.handle_unresolved_identifier(ctx, path.b, p, true);
						}
						None;
					case _: throw err;
				}
			}
		}, herits);
	}
}