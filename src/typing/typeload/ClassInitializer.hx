package typing.typeload;

import haxe.ds.Option;
import ocaml.PMap;
import ocaml.Ref;

using equals.Equal;
using ocaml.Cloner;

typedef Class_init_ctx = {
	tclass : core.Type.TClass, // I don't trust ctx.curclass because it's mutable.
	is_lib : Bool,
	is_native : Bool,
	is_core_api : Bool,
	is_class_debug : Bool,
	extends_public : Bool,
	_abstract : Option<core.Type.TAbstract>,
	context_init : Void -> Void,
	delayed_expr : Array<{typer:context.Typecore.Typer, olazy:Option<Ref<core.Type.TLazy>>}>,
	force_constructor : Bool,
	uninitialized_final : Option<core.Globals.Pos>
}

enum Field_kind {
	FKNormal;
	FKConstructor;
	FKInit;
}

typedef Field_init_ctx = {
	is_inline : Bool,
	is_final :Bool,
	is_static : Bool,
	is_override : Bool,
	is_extern : Bool,
	is_macro : Bool,
	is_abstract_member : Bool,
	is_display_field : Bool,
	is_field_debug : Bool,
	field_kind : Field_kind,
	do_bind : Bool,
	do_add : Bool,
}

class ClassInitializer {

	public static function dump_class_context (cctx:Class_init_ctx) : String {
		return core.type.Printer.s_record_fields ("", [
			{fst:"tclass", snd:core.type.Printer.s_tclass("\t", cctx.tclass)},
			{fst:"is_lib", snd:""+cctx.is_lib},
			{fst:"is_native", snd:""+cctx.is_native},
			{fst:"is_core_api",snd:""+cctx.is_core_api},
			{fst:"is_class_debug",snd:""+cctx.is_class_debug},
			{fst:"extends_public",snd:""+cctx.extends_public},
			{fst:"abstract", snd:core.type.Printer.s_opt(core.type.Printer.s_tabstract.bind("\t"), cctx._abstract)},
			{fst:"force_constructor",snd:""+cctx.force_constructor},
		]);
	}

	public static function s_field_kind (fk:Field_kind) : String {
		return switch (fk) {
			case FKNormal: "FKNormal";
			case FKConstructor: "FKConstructor";
			case FKInit: "FKInit";
		}
	}

	public static function dump_field_context (fctx:Field_init_ctx) : String {
		return core.type.Printer.s_record_fields("",[
			{fst:"is_inline",snd:""+ fctx.is_inline},
			{fst:"is_static",snd:""+ fctx.is_static},
			{fst:"is_override",snd:""+ fctx.is_override},
			{fst:"is_extern",snd:""+ fctx.is_extern},
			{fst:"is_macro",snd:""+ fctx.is_macro},
			{fst:"is_abstract_member",snd:""+ fctx.is_abstract_member},
			{fst:"is_display_field",snd:""+ fctx.is_display_field},
			{fst:"is_field_debug",snd:""+ fctx.is_field_debug},
			{fst:"field_kind",snd:s_field_kind(fctx.field_kind)},
			{fst:"do_bind",snd:""+ fctx.do_bind},
			{fst:"do_add",snd:""+fctx.do_add}
		]);
	}

	public static function create_class_context (ctx:context.Typecore.Typer, c:core.Type.TClass, context_init:Void->Void, p:core.Globals.Pos) : {fst:context.Typecore.Typer, snd:Class_init_ctx} {
		typing.Typeload.locate_macro_error.set(true);
		context.Common.stats.s_classes_built.set(context.Common.stats.s_classes_built.get()+1);
		var _abstract = switch (c.cl_kind) {
			case KAbstractImpl(a): Some(a);
			case _: None;
		}
		var _ctx = ctx.clone();
		_ctx.curclass = c;
		_ctx.type_params = c.cl_params.clone();
		_ctx.pass = PBuildClass;
		_ctx.tthis = switch (_abstract) {
			case Some(a):
				switch (a.a_this) {
					case TMono(_.get()=>r) if (r == None):
						TAbstract(a, c.cl_params.map(function (p) { return p.t; }));
					case t: t; 
				}
			case None:
				TInst(c, c.cl_params.map(function (p) { return p.t; }));
		};
		_ctx.on_error = function (ctx, msg, ep) {
			ctx.com.error(msg, ep);
			// macros expressions might reference other code, let's recall which class we are actually compiling
			if (typing.Typeload.locate_macro_error.get() && (ep.pfile!=c.cl_pos.pfile || ep.pmax < c.cl_pos.pmin || ep.pmin > c.cl_pos.pmax)) {
				ctx.com.error("Defined in this class", c.cl_pos);
			}
		};
		// a lib type will skip most checks
		var is_lib = core.Meta.has(LibType, c.cl_meta);
		if (is_lib && !c.cl_extern) {
			ctx.com.error("@:libType can only be used in extern classes", c.cl_pos);
		}
		// a native type will skip one check: the static vs non-static field
		var is_native = core.Meta.has(JavaNative, c.cl_meta) || core.Meta.has(CsNative, c.cl_meta);
		if (core.Meta.has(Macro, c.cl_meta)) {
			context.Typecore.display_error(ctx, "Macro classes are no longer allowed in haxe 3", c.cl_pos);
		}
		function extends_public (c:core.Type.TClass) {
			return core.Meta.has(PublicFields, c.cl_meta) || switch (c.cl_super) { case None: false; case Some({c:c}): extends_public(c); };
		}
		var cctx = {
			tclass: c,
			is_lib: is_lib,
			is_native: is_native,
			is_core_api: core.Meta.has(CoreApi, c.cl_meta),
			is_class_debug: false,
			extends_public: extends_public(c),
			_abstract: _abstract,
			context_init: context_init,
			force_constructor: false,
			uninitialized_final: None,
			delayed_expr: []
		}
		return {fst:_ctx, snd:cctx};
	}

	public static function create_field_context (ctx:context.Typecore.Typer, cctx:Class_init_ctx, c:core.Type.TClass, cff:core.Ast.ClassField) : {fst:context.Typecore.Typer, snd:Field_init_ctx} {
		var ctx = ctx.clone();
		ctx.pass = PBuildClass; // will be set later to PTypeExpr
		var is_static = ocaml.List.mem(core.Ast.Access.AStatic, cff.cff_access);
		var is_extern = core.Meta.has(Extern, cff.cff_meta) || c.cl_extern;
		var allow_inline = cctx._abstract != None || switch (cff.cff_kind) {
			case FFun(_): ctx.g.doinline || is_extern;
			case _: true;
		};
		var is_inline = allow_inline && ocaml.List.mem(core.Ast.Access.AInline, cff.cff_access);
		var is_override = ocaml.List.mem(core.Ast.Access.AOverride, cff.cff_access);
		var is_macro = ocaml.List.mem(core.Ast.Access.AMacro, cff.cff_access);
		var field_kind = switch (cff.cff_name.pack) {
			case "new": FKConstructor;
			case "__init__" if (is_static): FKInit;
			case _: FKNormal;
		}
		var fctx:Field_init_ctx = {
			is_inline: is_inline,
			is_static: is_static,
			is_override: is_override,
			is_macro: is_macro,
			is_extern: is_extern,
			is_final: ocaml.List.mem(core.Ast.Access.AFinal, cff.cff_access),
			is_display_field: ctx.is_display_file && context.Display.is_display_position(cff.cff_pos),
			is_field_debug: cctx.is_class_debug,
			is_abstract_member: cctx._abstract != None && core.Meta.has(Impl, cff.cff_meta),
			field_kind: field_kind,
			do_bind: (((!c.cl_extern || is_inline) && !c.cl_interface) || field_kind == FKInit),
			do_add: true
		};
		return {fst:ctx, snd:fctx};
	}

	public static function is_public (ctx:context.Typecore.Typer, cctx:Class_init_ctx, access:Array<core.Ast.Access>, parent:Option<core.Type.TClassField>) : Bool {
		var c = cctx.tclass;
		if (ocaml.List.mem(core.Ast.Access.APrivate, access)) {
			return false;
		}
		else if (ocaml.List.mem(core.Ast.Access.APublic, access)) {
			return true;
		}
		else {
			return switch(parent) {
				case Some({cf_public:p}):p;
				case _: c.cl_extern || c.cl_interface || cctx.extends_public;
			}
		}
	}

	public static function get_parent (c:core.Type.TClass, name:String) : Option<core.Type.TClassField> {
		return switch (c.cl_super) {
			case None: None;
			case Some({c:csup}):
				try {
					Some(PMap.find(name, csup.cl_fields));
				}
				catch (_:ocaml.Not_found) {
					get_parent(csup, name);
				}
		}
	}

	public static function add_field (c:core.Type.TClass, cf:core.Type.TClassField, is_static:Bool) : Void {
		if (is_static) {
			c.cl_statics.set(cf.cf_name, cf);
			c.cl_ordered_statics.unshift(cf);
		}
		else {
			c.cl_fields.set(cf.cf_name, cf);
			c.cl_ordered_fields.unshift(cf);
		}
	}

	public static function type_opt (ctx:context.Typecore.Typer, cctx:Class_init_ctx, p:core.Globals.Pos, t:Option<core.Ast.TypeHint>) : core.Type.T {
		var c = cctx.tclass;
		return switch (t) {
			case None if (c.cl_extern || c.cl_interface):
				context.Typecore.display_error(ctx, "Type required for extern classes and interfaces", p);
				core.Type.t_dynamic;
			case None if (cctx.is_core_api):
				context.Typecore.display_error(ctx, "Type required for core api classes", p);
				core.Type.t_dynamic;
			case _:
				typing.Typeload.load_type_hint(ctx, p, t);
		}
	}

	public static function build_field (ctx:context.Typecore.Typer, cctx:Class_init_ctx, c:core.Type.TClass, fields:Array<core.Ast.ClassField>) {
		var fields = new Ref(fields);
		function get_fields () {
			return fields.get();
		}
		var pending = new Ref([]);
		c.cl_build = function () { return BuildMacro(pending); };
		typing.Typeload.build_module_def(ctx, TClassDecl(c), c.cl_meta, get_fields, cctx.context_init, function (expr:core.Ast.Expr) {
			var e = expr.expr; var p = expr.pos;
			switch (e) {
				case EVars(arr) if (arr.length == 1 && arr[0].expr == None):
					switch (arr[0].type) {
						case Some({ct:CTAnonymous(f), pos:p}):
							var f = f.map(function (f) {
								var f = switch (cctx._abstract) {
									case Some(a):
										var a_t = core.type.TExprToExpr.convert_type_(TAbstract(a, a.a_params.map(function (p) {return p.t; })));
										var this_t = core.type.TExprToExpr.convert_type_(a.a_this); // TODO: better pos?
										typing.Typeload.transform_abstract_field(ctx.com, this_t, a_t, a, f);
									case None:
										f;
								}
								if (ocaml.List.mem(core.Ast.Access.AMacro, f.cff_access)) {
									switch (ctx.g.macros) {
										case Some({t:mctx}) if (ocaml.Hashtbl.mem(mctx.g.types_module, c.cl_path)):
											// assume that if we had already a macro with the same name, it has not been changed during the @:build operation
											if (!ocaml.List.exists(function(f2) { return f2.cff_name == f.cff_name && ocaml.List.mem(core.Ast.Access.AMacro, f2.cff_access); }, fields.get())) {
												core.Error.error("Class build macro cannot return a macro function when the class has already been compiled into the macro context", p);
											}
										case _:
									}
								}
								return f;
							});
							fields.set(f);
							return;
						case _:
					}
				case _:
			}
			core.Error.error("Class build macro must return a single variable with anonymous fields", p);
		});
		c.cl_build = function () {return Building([c]); };
		ocaml.List.iter(function (f) { f(); }, pending.get());
		return fields.get();
	}

	public static function bind_type (ctx:context.Typecore.Typer, cctx:Class_init_ctx, fctx:Field_init_ctx, cf:core.Type.TClassField, r:Ref<core.Type.TLazy>, p:core.Globals.Pos) : Void {
		var c = cctx.tclass;
		function is_full_type (t:core.Type.T) : Bool {
			return switch (t) {
				case TFun({args:args, ret:ret}): is_full_type(ret) && ocaml.List.for_all(function (arg) { return is_full_type(arg.t); }, args);
				case TMono(r):
					switch (r.get()) {
						case None: false;
						case Some(t): is_full_type(t);
					}
				case TAbstract(_), TInst(_), TEnum(_), TLazy(_), TDynamic(_), TAnon(_), TType(_): return true;
			}
		}
		function force_macro () {
			// force macro system loading of this class in order to get completion
			context.Typecore.delay(ctx, PTypeField, function () {
				try {
					ctx.g.do_macro(ctx, MDisplay, c.cl_path, cf.cf_name, [], p);
				}
				catch (_:ocaml.Exit) {}
				catch (_:core.Error) {}
			});
		}
		function handle_display_field () {
			if (fctx.is_macro && !ctx.in_macro) {
				force_macro();
			}
			else {
				cf.cf_type = TLazy(r);
				cctx.delayed_expr.unshift({typer:ctx, olazy:Some(r)});
			}
		}
		if (ctx.com.display.dms_full_typing) {
			if (fctx.is_macro && !ctx.in_macro) {}
			else {
				cf.cf_type = TLazy(r);
				// is_lib ?
				cctx.delayed_expr.unshift({typer:ctx, olazy:Some(r)});
			}
		}
		else if (ctx.com.display.dms_force_macro_typing && fctx.is_macro && !ctx.in_macro) {
			force_macro();
		}
		else {
			if (fctx.is_display_field) {
				handle_display_field();
			}
			else {
				if (!is_full_type(cf.cf_type)) {
					cctx.delayed_expr.unshift({typer:ctx, olazy:None});
					cf.cf_type = TLazy(r);
				}
			}
		}
	}

	public static function bind_var (ctx:context.Typecore.Typer, cctx:Class_init_ctx, fctx:Field_init_ctx, cf:core.Type.TClassField, e:Option<core.Ast.Expr>) : Void {
		var c = cctx.tclass;
		var p = cf.cf_pos;
		function get_declared (f:String, t:Option<{c:core.Type.TClass, params:core.Type.TParams}>) {
			return switch (t) {
				case None: None;
				case Some(ca={c:c}) if (PMap.exists(f, c.cl_fields)):
					Some(ca);
				case Some({c:c}):
					var ret = get_declared(f, c.cl_super);
					switch (ret) {
						case Some(r): Some(r);
						case None:
							function loop (ifaces:Array<{c:core.Type.TClass, params:core.Type.TParams}>) {
								if (ifaces.length == 0) { return None; }
								return switch (get_declared(f, Some(ifaces[0]))) {
									case Some(r): Some(r);
									case None: loop(ifaces.slice(1));
								}
							}
							loop(c.cl_implements);
					}
			}
		}
		if (!fctx.is_static && !cctx.is_lib) {
			switch (get_declared(cf.cf_name, c.cl_super)) {
				case None:
				case Some({c:csup}):
					// this can happen on -net-lib generated classes if a combination of explicit interfaces and variables with the same name happens
					if (!(csup.cl_interface && core.Meta.has(CsNative, c.cl_meta))) {
						core.Error.error("Redefinition of variable " + cf.cf_name + " in subclass is not allowed. Previously declared at " + core.Globals.s_type_path(csup.cl_path), p);
					}
			}
		}
		var t = cf.cf_type;
		switch (e) {
			case None:
				if (fctx.is_display_field)  {
					context.display.DisplayEmitter.maybe_display_field(ctx, cf.cf_name_pos, cf);
				}
			case Some(e):
				if (typing.Typeload.requires_value_meta(ctx.com, Some(c))) {
					cf.cf_meta.unshift({name:Value, params:[e], pos:core.Globals.null_pos});
				}
				function check_cast(e:core.Type.TExpr) {
					// insert cast to keep explicit field type (issue #1901)
					if (core.Type.type_iseq(e.etype, cf.cf_type)) {
						return e;
					}
					else {
						return switch ({fst:e.eexpr, snd:core.Type.follow(cf.cf_type)}) {
							case {fst:TConst(TInt(i)), snd:TAbstract({a_path:path}, _)} if (path.a.length == 0 && path.b == "Float"):
								// turn int constant to float constant if expected type is float *)
								var _e = e.clone();
								_e.eexpr = TConst(TFloat(Std.string(i)));
								return _e;
							case _:
								core.Type.mk_cast(e, cf.cf_type, e.epos);
						}
					}
				}
				var r = context.Typecore.exc_protect(false, ctx, function (r) {
					// type constant init fields (issue #1956)
					if (!typing.Typeload.return_partial_type.get() || switch (e.expr) { case EConst(_):true; case _: false;}) {
						r.set(core.Type.lazy_processing(function () {return t;}));
						cctx.context_init();
						if (ctx.com.verbose) {
							 context.Common.log(ctx.com, "Typing " + ((ctx.in_macro) ? "macro " : "") + core.Globals.s_type_path(c.cl_path) + "." + cf.cf_name);
						}
						var e = typing.Typeload.type_var_field(ctx, t, e, fctx.is_static, fctx.is_display_field, p);
						function maybe_run_analyzer (e:core.Type.TExpr) :core.Type.TExpr {
							return switch (e.eexpr) {
								case TConst(_), TLocal(_), TFunction(_): e;
								case _: context.Typecore.analyser_run_on_expr_ref.get()(ctx.com, e);
							}
						}
						function require_constant_expression (e:core.Type.TExpr, msg) : core.Type.TExpr {
							if (ctx.com.display.dms_display && ctx.com.display.dms_error_policy != EPCollect) {
								return e;
							}
							else {
								return switch (optimization.Optimizer.make_constant_expression(ctx, maybe_run_analyzer(e))) {
									case Some(e): e;
									case None: context.Typecore.display_error(ctx, msg, p); e;
								}
							}
						}
						var e = switch (cf.cf_kind) {
							case Var(v) if (c.cl_extern || core.Meta.has(Extern, cf.cf_meta)):
								if (!fctx.is_static) {
									context.Typecore.display_error(ctx, "Extern non-static variables may not be initialized", p);
									e;
								}
								else if (!fctx.is_inline) {
									context.Typecore.display_error(ctx, "Extern non-inline variables may not be initialized", p);
									e;
								}
								else {
									require_constant_expression(e, "Extern variable initialization must be a constant value");
								}
							case Var(v) if (!core.Type.is_physical_field(cf)):
								// disallow initialization of non-physical fields (issue #1958)
								context.Typecore.display_error(ctx, "This field cannot be initialized because it is not a real variable", p);
								e;
							case Var(v) if (!fctx.is_static):
								var e = if (ctx.com.display.dms_display && ctx.com.display.dms_error_policy != EPCollect) {
									e;
								}
								else {
									function check_this(e:core.Type.TExpr) {
										switch (e.eexpr) {
											case TConst(TThis):
												context.Typecore.display_error(ctx, "Cannot access this or other member field in variable initialization", e.epos);
												throw ocaml.Exit.instance;
											case TLocal(v) if (switch(ctx.vthis) { case Some(v2): v == v2; case None: false;}):
												context.Typecore.display_error(ctx, "Cannot access this or other member field in variable initialization", e.epos);
												throw ocaml.Exit.instance;
											case _:
											core.Type.iter(check_this, e);
										}
									}
									try {
										check_this(e);
										switch (optimization.Optimizer.make_constant_expression(ctx, maybe_run_analyzer(e))) {
											case Some(_e): _e;
											case None: e;
										}
									}
									catch (_:ocaml.Exit) {
										e;
									}
								}
								e;
							case Var(v) if (v.v_read == AccInline):
								var e = require_constant_expression(e, "Inline variable initialization must be a constant value");
								switch (c.cl_kind) {
									case KAbstractImpl(a) if (core.Meta.has(Enum, cf.cf_meta) && core.Meta.has(Enum, a.a_meta)):
										context.Typecore.unify(ctx, t, TAbstract(a, a.a_params.map(function(_) {return core.Type.mk_mono(); })), p);
										switch (e.eexpr) {
											case TCast(e1,None): context.Typecore.unify(ctx, e1.etype, a.a_this, e1.epos);
											case _: throw false;
										}
									case _:
								}
								e;
							case _: e;
						}
						e = check_cast(e);
						cf.cf_expr = Some(e);
						cf.cf_type = t;
						if (fctx.is_display_field) {
							context.display.DisplayEmitter.maybe_display_field(ctx, cf.cf_name_pos, cf);
						}
					}
					return t;
				}, "bind_var");
			if (!fctx.is_static) {
				cctx.force_constructor = true;
			}
			bind_type(ctx, cctx, fctx, cf, r, e.pos);
		}
	}

	public static function create_variable (ctx:context.Typecore.Typer, cctx:Class_init_ctx, fctx:Field_init_ctx, c:core.Type.TClass, f:core.Ast.ClassField, t:Option<core.Ast.TypeHint>, eo:Option<core.Ast.Expr>, p:core.Globals.Pos) : core.Type.TClassField {
		if (!fctx.is_static && cctx._abstract != None) {
			core.Error.error(f.cff_name.pack + ": Cannot declare member variable in abstract", p);
		}
		if (fctx.is_inline && !fctx.is_static) {
			core.Error.error(f.cff_name.pack + ": Inline variable must be static", p);
		}
		if (fctx.is_inline && eo == None) {
			core.Error.error(f.cff_name.pack + ": Inline variable must be initialized", p);
		}
		if (fctx.is_final && eo == None) {
			if (fctx.is_static) {
				core.Error.error(f.cff_name.pack + ": Static final variable must be initialized", p);
			}
			else {
				cctx.uninitialized_final = Some(f.cff_pos);
			}
		}
		var t = switch (t) {
			case None if (!fctx.is_static && eo == None):
				core.Error.error("Type required for member variable " + f.cff_name.pack, p);
			case None:
				core.Type.mk_mono();
			case Some(t):
				// TODO is_lib: only load complex type if needed
				var old = ctx.type_params;
				if (fctx.is_static) {
					ctx.type_params = switch (cctx._abstract) {
						case Some(a): a.a_params;
						case _: [];
					}
				}
				var t = typing.Typeload.load_complex_type(ctx, true, p, t);
				if (fctx.is_static) {
					ctx.type_params = old;
				}
				t;
		}
		var kind:core.Type.VarKind = if (fctx.is_inline) {
			{v_read: AccInline, v_write:AccNever};
		}
		else if (fctx.is_final) {
			{v_read:AccNormal, v_write:(fctx.is_static) ? AccNever : AccCtor};
		}
		else {
			{v_read: AccNormal, v_write: AccNormal};
		};
		var _meta = f.cff_meta.clone();
		if (fctx.is_final && !core.Meta.has(Final, f.cff_meta)) {
			_meta.unshift({name:Final, params:[], pos:core.Globals.null_pos});
		}
		var cf = core.Type.mk_field(f.cff_name.pack, t, f.cff_pos, f.cff_name.pos);
		cf.cf_doc = f.cff_doc;
		cf.cf_meta = _meta;
		cf.cf_kind = Var(kind);
		cf.cf_public = is_public(ctx, cctx, f.cff_access, None);
		ctx.curfield = cf;
		bind_var(ctx, cctx, fctx, cf, eo);
		return cf;
	}

	public static function check_abstract (ctx:context.Typecore.Typer, cctx:Class_init_ctx, fctx:Field_init_ctx, c:core.Type.TClass, cf:core.Type.TClassField, fd:core.Ast.Func, t:core.Type.T, ret:core.Type.T, p:core.Globals.Pos) : Void {
		switch (cctx._abstract) {
			case Some(a):
				var m = core.Type.mk_mono();
				var ta:core.Type.T = TAbstract(a, a.a_params.map(function (_) { return core.Type.mk_mono(); }));
				var tthis = (fctx.is_abstract_member || core.Meta.has(To, cf.cf_meta)) ? core.Type.monomorphs(a.a_params, a.a_this) : a.a_this;
				var allows_no_expr = new Ref(core.Meta.has(CoreType, a.a_meta));
				function loop (ml:core.Ast.Metadata) {
					if (ml.length == 0) {}
					else {
						var first = ml[0];
						switch (first.name) {
							case From:
								var r = context.Typecore.exc_protect(ctx, function (r) {
									r.set(core.Type.lazy_processing(function () {return t;}));
									// the return type of a from-function must be the abstract, not the underlying type
									if (!fctx.is_macro) {
										try {
											core.Type.type_eq(EqStrict, ret, ta);
										}
										catch (ue:core.Type.Unify_error) {
											core.Error.error(core.Error.error_msg(Unify(ue.l)), p);
										}
									}
									return switch (t) {
										case TFun({args:args}) if (args.length == 1):
											args[0].t;
										case _:
											core.Error.error(cf.cf_name+": @:from cast functions must accept exactly one argument", p);
									}
								}, "@:from");
								a.a_from_field.unshift({t:TLazy(r), cf:cf});
							case To:
								if (fctx.is_macro) {
									core.Error.error(cf.cf_name + ": Macro cast functions are not supported", p);
								}
								// TODO: this doesn't seem quite right...
								if (!core.Meta.has(Impl, cf.cf_meta)) {
									cf.cf_meta.unshift({name:Impl,params:[],pos:core.Globals.null_pos});
								}
								function resolve_m (args) {
									try {
										context.Typecore.unify_raise(ctx, t, core.Type.tfun([tthis].concat(args), m), cf.cf_pos);
									}
									catch (err:core.Error) {
										switch (err.msg) {
											case Unify(l):
												core.Error.error(core.Error.error_msg(Unify(l)), err.pos);
											case _: throw err;
										}
									}
									return switch (core.Type.follow(m)) {
										case TMono(_) if (switch (t) { case TFun({ret:r}): r == core.Type.t_dynamic; case _: false; }): core.Type.t_dynamic;
										case m: m;
									}
								}
								var r = context.Typecore.exc_protect(ctx, function (r) {
									r.set(core.Type.lazy_processing(function () {return t; }));
									var args = if (core.Meta.has(MultiType, a.a_meta)) {
										var ctor = try {
											ocaml.PMap.find("_new", c.cl_statics);
										}
										catch (_:ocaml.Not_found) {
											core.Error.error("Constructor of multi-type abstract must be defined before the individual @:to-functions are", cf.cf_pos);
										}
										// delay ctx PFinal (fun () -> unify ctx m tthis f.cff_pos);
										var args = switch (core.Type.follow(core.Type.monomorphs(a.a_params, ctor.cf_type))) {
											case TFun({args:args}): args.map(function (arg) {return arg.t; });
											case _: throw false;
										};
										args;
									}
									else {
										[];
									}
									return resolve_m(args);
						 		}, "@:to");
								a.a_to_field.unshift({t:TLazy(r), cf:cf});
							case ArrayAccess:
								if (fctx.is_macro) {
									core.Error.error(cf.cf_name + ": Macro array-access functions are not supported", p);
								}
								a.a_array.unshift(cf);
							case Op:
								if (first.params.length == 1) {
									switch (first.params[0].expr) {
										case EArrayDecl(_):
											if (fctx.is_macro) {
												core.Error.error(cf.cf_name + ": Macro array-access functions are not supported", p);
											}
											a.a_array.unshift(cf);
										case EBinop(op, _, _):
											if (fctx.is_macro) {
												core.Error.error(cf.cf_name +": Macro operator functions are not supported", p);
											}
											var targ = (fctx.is_abstract_member) ? tthis : ta;
											var _tmp = switch (core.Type.follow(t)) {
												case TFun({args:args}) if (args.length == 2):
													var t1 = args[0].t; var t2 = args[1].t;
													{fst:core.Type.type_iseq(targ, t1), snd:core.Type.type_iseq(targ, t2)};
												case _:
													if (fctx.is_abstract_member) {
														core.Error.error(cf.cf_name + ": Member @:op functions must accept exactly one argument", cf.cf_pos);
													}
													else {
														core.Error.error(cf.cf_name + ": Static @:op functions must accept exactly two arguments", cf.cf_pos);
													}
											};
											var left_eq = _tmp.fst; var right_eq = _tmp.snd;
											if (!(left_eq || right_eq)) {
												core.Error.error(cf.cf_name + ": The left or right argument type must be " + core.Type.s_type(core.Type.print_context(), targ), cf.cf_pos);
											}
											if (right_eq && core.Meta.has(Commutative, cf.cf_meta)) {
												core.Error.error(cf.cf_name + ": @:commutative is only allowed if the right argument is not " + core.Type.s_type(core.Type.print_context(), targ), cf.cf_pos);
											}
											a.a_ops.unshift({op:op, cf:cf});
											allows_no_expr.set(true);
										case EUnop(op, flag, _):
											if (fctx.is_macro) {
												core.Error.error(cf.cf_name + ": Macro operator functions are not supported", p);
											}
											var targ = (fctx.is_abstract_member) ? tthis : ta;
											try {
												core.Type.type_eq(EqStrict, t, core.Type.tfun([targ], core.Type.mk_mono()));
											}
											catch (ue:core.Type.Unify_error) {
												throw new core.Error(Unify(ue.l),cf.cf_pos);
											}
											a.a_unops.unshift({op:op, flag:flag, cf:cf});
											allows_no_expr.set(true);
										case EField(_):
											if (a.a_resolve != None) {
												core.Error.error("Multiple resolve methods are not supported", cf.cf_pos);
											}
											var targ = (fctx.is_abstract_member) ? tthis : ta;
											switch (core.Type.follow(t)) {
												case TFun({args:args}) if (args.length == 2):
													var t1 = args[0].t; var t2 = args[1].t;
													if (!fctx.is_macro) {
														if (!core.Type.type_iseq(targ, t1)) {
															core.Error.error("First argument type must be " + core.Type.s_type(core.Type.print_context(), targ), cf.cf_pos);
														}
														if (!core.Type.type_iseq(ctx.t.tstring, t2)) {
															core.Error.error("Second argument type must be String", cf.cf_pos);
														}
													}
												case _:
													core.Error.error("Field type of resolve must be " + core.Type.s_type(core.Type.print_context(), targ) + " -> String -> T", cf.cf_pos);
											}
											a.a_resolve= Some(cf);
										case _: loop(ml.slice(1));
									}
								}
							case Impl if (cf.cf_name != "_new" && !fctx.is_macro):
								switch (core.Type.follow(t)) {
									case TFun({args:args}) if (args.length > 0 && core.Type.type_iseq(tthis, args[0].t)):
									case _:
										context.Typecore.display_error(ctx, "First argument of implementation function must be " + core.Type.s_type(core.Type.print_context(), tthis), cf.cf_pos);
								}
								loop(ml.slice(1));
							case Resolve:
								if (a.a_resolve != None) {
									core.Error.error("Multiple resolve methods are not supported", cf.cf_pos);
								}
								var targ = (fctx.is_abstract_member) ? tthis : ta;
								switch (core.Type.follow(t)) {
									case TFun({args:args}) if (args.length == 2):
										var t1 = args[0].t; var t2 = args[1].t;
										if (!fctx.is_macro) {
											if (!core.Type.type_iseq(targ, t1)) {
												core.Error.error("First argument type must be " + core.Type.s_type(core.Type.print_context(), targ), cf.cf_pos);
											}
											if (!core.Type.type_iseq(ctx.t.tstring, t2)) {
												core.Error.error("Second argument type must be String", cf.cf_pos);
											}
										}
									case _:
										core.Error.error("Field type of resolve must be " + core.Type.s_type(core.Type.print_context(), targ) + " -> String -> T", cf.cf_pos);
								}
								a.a_resolve= Some(cf);
							case _: loop(ml.slice(1));
						}
					}
				}
				loop(cf.cf_meta);
				function check_bind () {
					if (fd.f_expr == None) {
						if (fctx.is_inline) {
							core.Error.error(cf.cf_name + ": Inline functions must have an expression", cf.cf_pos);
						}
						switch (fd.f_type) {
							case None:
								core.Error.error(cf.cf_name + ": Functions without expressions must have an explicit return type", cf.cf_pos);
							case Some(_):
						}
					}
					cf.cf_meta.unshift({name:NoExpr,params:[],pos:core.Globals.null_pos});
					fctx.do_bind = false;
				}
				if (cf.cf_name == "_new" && core.Meta.has(MultiType, a.a_meta)) {
					fctx.do_bind = false;
				}
				if (allows_no_expr.get()) {
					check_bind();
				}
			case _:
		}
	}

	public static function create_method (ctx:context.Typecore.Typer, cctx:Class_init_ctx, fctx:Field_init_ctx, c:core.Type.TClass, f:core.Ast.ClassField, fd:core.Ast.Func, p:core.Globals.Pos) : core.Type.TClassField {
		var params = typing.Typeload.type_function_params(ctx, fd, f.cff_name.pack, p);
		if (core.Meta.has(Generic, f.cff_meta)) {
			if (params.length == 0) {
				core.Error.error(f.cff_name.pack+": Generic functions must have type parameters", p);
			}
		}
		var fd = if (fctx.is_macro && !ctx.in_macro && !fctx.is_static) {
			// remove display of first argument which will contain the "this" expression
			var _fd = fd.clone();
			_fd.f_args = if (fd.f_args.length == 0) {
				[];
			}
			else {
				fd.f_args.slice(1);
			}
			_fd;
		}
		else {
			fd;
		}

		fd = if (!fctx.is_macro) {
			fd;
		}
		else {
			if (ctx.in_macro) {
				// a class with a macro cannot be extern in macro context (issue #2015)
				c.cl_extern = false;
				var texpr:core.Ast.ComplexType = CTPath({tpackage:["haxe", "macro"], tname:"Expr", tparams:[], tsub:None});
				// ExprOf type parameter might contain platform-specific type, let's replace it by Expr
				function no_expr_of (tp:core.Ast.TypeHint) : Option<core.Ast.TypeHint> {
					var t = tp.ct;
					switch (t) {
						case CTPath(ct):
							if (ct.tparams.length == 1) {
								switch (ct.tparams[0]) {
										case TPType(_):
											if ((ct.tname == "Expr" && ct.tpackage.equals(["haxe", "macro"]) && ct.tsub.equals(Some("ExprOf")))
											|| (ct.tname == "ExprOf" && ct.tpackage.length == 0 && ct.tsub == None)) {
												return Some({ct:texpr, pos:tp.pos});
											}
										case _:
									}
							}
						case _:
					}
					return Some({ct:t, pos:tp.pos});
				}
				{
					f_params: fd.f_params.clone(),
					f_type: switch (fd.f_type) { case None: Some({ct:texpr, pos:core.Globals.null_pos}); case Some(t): no_expr_of(t);},
					f_args: fd.f_args.map(function (arg:core.Ast.FunArg) : core.Ast.FunArg {
						return {
							name:arg.name,
							opt:arg.opt,
							meta:arg.meta.clone(),
							type: switch (arg.type) {
								case None: Some({ct:texpr, pos:core.Globals.null_pos});
								case Some(t): no_expr_of(t);
							},
							value:arg.value.clone()
						};
					} ),
					f_expr: fd.f_expr.clone()
				};
			}
			else {
				var tdyn:Option<core.Ast.TypeHint> = Some({ct:CTPath({tpackage:[], tname:"Dynamic", tparams:[], tsub:None}), pos:core.Globals.null_pos});
				function to_dyn (p:core.Globals.Pos, t:core.Ast.TypePath) : Option<core.Ast.TypeHint> {
					if (t.tparams.length == 0) {
						if (t.tpackage.equals(["haxe"]) && t.tname == "PosInfos" && t.tsub == None) {
							return core.Error.error("haxe.PosInfos is not allowed on macro functions, use Context.currentPos() instead", p);
						}
					}
					else {
						switch (t.tparams[0]) {
							case TPType(_t):
								if ((t.tpackage.equals(["haxe", "macro"]) && t.tname == "Expr" && t.tsub.equals(Some("ExprOf")))
								|| (t.tpackage.length == 0 && t.tname == "ExprOf" && t.tsub == None)) {
									Some(_t);
								}
							case _:
						}
					}
					return tdyn;
				}
				{
					f_params: fd.f_params.clone(),
					f_type: switch (fd.f_type) { case Some({ct:CTPath(t), pos:p}): to_dyn(p, t); case _: tdyn;},
					f_args: fd.f_args.map(function (arg:core.Ast.FunArg) : core.Ast.FunArg {
						return {
							name:arg.name,
							opt:arg.opt,
							meta:arg.meta.clone(),
							type: switch (arg.type) {
								case Some({ct:CTPath(t), pos:p}): to_dyn(p, t);
								case _: tdyn;
							},
							value:None
						};
					} ),
					f_expr: None
				};
			}
		}
		switch {f:c.cl_interface, s:fctx.field_kind} {
			case {f:true, s:FKConstructor}:
				core.Error.error("An interface cannot have a constructor", p);
			case {f:true}:
				if (!fctx.is_static && fd.f_expr != None) {
					core.Error.error(f.cff_name.pack + ": An interface method cannot have a body", p);
				}
				if (fctx.is_inline && c.cl_interface) {
					core.Error.error(f.cff_name.pack + ": You can't declare inline methods in interfaces", p);
				}
			case {f:false, s:FKConstructor}:
				if (fctx.is_static) {
					core.Error.error("A constructor must not be static", p);
				}
				switch (fd.f_type) {
					case None:
					case Some({ct:CTPath({tpackage:pack, tname:"Void"})}) if (pack.length == 0):
					case _:
						core.Error.error("A class constructor can't have a return value", p);

				}
			case {f:false}:
		}
		var parent:Option<core.Type.TClassField> = (!fctx.is_static) ? get_parent(c, f.cff_name.pack) : None;
		var _dynamic = ocaml.List.mem(core.Ast.Access.ADynamic, f.cff_access) || switch (parent) { case Some({cf_kind:Method(MethDynamic)}): true; case _: false; };
		if (fctx.is_inline && _dynamic) {
			core.Error.error(f.cff_name.pack+": You can't have both 'inline' and 'dynamic'", p);
		}
		ctx.type_params = switch (cctx._abstract) {
			case Some(a) if (fctx.is_abstract_member): params.concat(a.a_params);
			case _: (fctx.is_static) ? params: params.concat(ctx.type_params);
		}
		// TODO is_lib: avoid forcing the return type to be typed
		var ret = (fctx.field_kind == FKConstructor) ? ctx.t.tvoid : type_opt(ctx, cctx, p, fd.f_type);
		function loop (args:Array<core.Ast.FunArg>) {
			if (args.length == 0) { return []; }
			else {
				var _tmp = args[0];
				var name = _tmp.name.pack; var p = _tmp.name.pos;
				var opt = _tmp.opt; var m = _tmp.meta; var t = _tmp.type;
				var ct = _tmp.value;
				// TODO is_lib: avoid forcing the field to be typed
				var __t = typing.Typeload.type_function_arg(ctx, type_opt(ctx, cctx, p, t), ct, opt, p);
				context.Typecore.delay(ctx, PTypeField, function() {
					switch (core.Type.follow(__t.fst)) {
						case TAbstract({a_path:path}, _) if (path.equals(new core.Path(["haxe", "extern"], "Rest"))):
							if (!c.cl_extern) {
								core.Error.error("Rest argument are only supported for extern methods", p);
							}
							if (opt) {
								core.Error.error("Rest argument cannot be optional", p);
							}
							switch (ct) {
								case None:
								case Some({pos:p}): core.Error.error("Rest argument cannot have default value", p);
							}
							if (args.length > 0) {
								core.Error.error("Rest should only be used for the last function argument", p);
							}
						case _:
					}
				});
				var _res = loop(args.slice(1));
				_res.unshift({name:name, opt:__t.snd, t:__t.fst});
				return _res;
			}
		}
		var args = loop(fd.f_args);
		var t:core.Type.T = TFun({args:core.Type.fun_args(args), ret:ret});
		var cf = core.Type.mk_field(f.cff_name.pack, t, f.cff_pos, f.cff_name.pos);
		cf.cf_doc = f.cff_doc.clone();
		cf.cf_meta = f.cff_meta.clone();
		if (fctx.is_final && !core.Meta.has(Final, f.cff_meta)) {
			cf.cf_meta.unshift({name:Final, params:[], pos:core.Globals.null_pos});
		}
		cf.cf_kind = Method((fctx.is_macro) ? MethMacro : ((fctx.is_inline) ? MethInline : ((_dynamic) ? MethDynamic : MethNormal)));
		cf.cf_public = is_public(ctx, cctx, f.cff_access, parent);
		cf.cf_params = params.clone();
		cf.cf_meta = cf.cf_meta.map(function (meta) {
			var m = meta.name; var el = meta.params; var p = meta.pos;
			if (m == AstSource && el.length == 0) {
				return {name:m, params: switch (fd.f_expr) {
					case None: [];
					case Some(e): [e];
				}, pos:p};
			}
			else {
				return meta.clone();
			}
		});
		typing.Typeload.generate_value_meta(ctx.com, Some(c), cf, fd.f_args);
		check_abstract(ctx, cctx, fctx, c, cf, fd, t, ret, p);
		typing.Typeload.init_meta_overloads(ctx, Some(c), cf);
		ctx.curfield = cf;
		var r = context.Typecore.exc_protect(false, ctx, function (r) {
			if (!typing.Typeload.return_partial_type.get()) {
				r.set(core.Type.lazy_processing(function () {return t;}));
				cctx.context_init();
				context.Common.stats.s_methods_typed.set(context.Common.stats.s_methods_typed.get()+1);
				if (ctx.com.verbose) {
					context.Common.log(ctx.com, "Typing "+ ((ctx.in_macro) ? "macro ": "") + core.Globals.s_type_path(c.cl_path) + "." + f.cff_name.pack);
				}
				var fmode:context.Typecore.CurrentFun = switch (cctx._abstract) {
					case Some(_):
						if (args.length > 0 && args[0].name == "this") {
							FunMemberAbstract;
						}
						else if (f.cff_name.pack == "_new") {
							FunMemberAbstract;
						}
						else {
							FunStatic;
						}
					case None:
						if (fctx.field_kind == FKConstructor) {
							FunConstructor;
						}
						else if (fctx.is_static) {
							FunStatic;
						}
						else {
							FunMember;
						}
				};
				switch (ctx.com.platform) {
					case Java if (typing.Typeload.is_java_native_function(cf.cf_meta)):
						if (fd.f_expr != None) {
							ctx.com.warning("@:native function definitions shouldn't include an expression. This behaviour is deprecated.", cf.cf_pos);
						}
						cf.cf_expr = None;
						cf.cf_type = t;
					case _:
						var _tmp = typing.Typeload.type_function(ctx, args, ret, fmode, fd, fctx.is_display_field, p);
						var e = _tmp.fst; var fargs = _tmp.snd;
						switch (fctx.field_kind) {
							case FKNormal if (!fctx.is_static):
								typing.Typeload.check_overriding(ctx, c, cf);
							case _:
						}
						// Disabled for now, see https://github.com/HaxeFoundation/haxe/issues/3033
						/* List.iter (fun (v,_) ->
							if v.v_name <> "_" && has_mono v.v_type then ctx.com.warning "Uninferred function argument, please add a type-hint" v.v_pos;
						) fargs; */
						var tf:core.Type.TFunc = {
							tf_args: fargs.clone(),
							tf_type: ret.clone(),
							tf_expr: e.clone()
						};
						if (fctx.field_kind == FKInit) {
							switch (e.eexpr) {
								case TConst(_):
								case TBlock(l) if (l.length == 0):
								case TBlock(l) if (l.length == 1 && switch (l[0].eexpr) { case TConst(_): true; case _:false;}):
								case TObjectDecl(l) if (l.length == 0):
								case _: c.cl_init = Some(e);
							}
						}
						cf.cf_expr = Some(core.Type.mk(TFunction(tf), t, p));
						cf.cf_type = t;
						if (fctx.is_display_field) {
							context.display.DisplayEmitter.maybe_display_field(ctx, cf.cf_name_pos, cf);
						}
				}
			}
			throw false;
		}, "type_fun");
		if (fctx.do_bind) {
			bind_type(ctx,cctx,fctx, cf, r, switch(fd.f_expr) {
				case Some(e) : e.pos; case None: f.cff_pos;
			});
		}
		else if (fctx.is_display_field) {
			context.display.DisplayEmitter.maybe_display_field(ctx, cf.cf_name_pos, cf);
		}
		return cf;
	}

	public static function init_field (ctx:context.Typecore.Typer, cctx:Class_init_ctx, fctx:Field_init_ctx, f:core.Ast.ClassField) : core.Type.TClassField {
		var c = cctx.tclass;
		var name = f.cff_name.pack;
		typing.Typeload.check_global_metadata(ctx, f.cff_meta, function (m) {
			f.cff_meta.unshift(m);
		}, c.cl_module.m_path, c.cl_path, Some(name));
		var p = f.cff_pos;
		if (name.charAt(0) == "$") {
			context.Typecore.display_error(ctx, "Field names starting with a dollar are not allowed", p);
		}
		for (acc in f.cff_access) {
			switch ({fst:acc, snd:f.cff_kind}) {
				case {fst:APublic}, {fst:APrivate}, {fst:AStatic}, {fst:AFinal}:
				case {fst:ADynamic, snd:FFun(_)}, {fst:AOverride, snd:FFun(_)}, {fst:AMacro, snd:FFun(_)}, {fst:AInline, snd:FFun(_)}, {fst:AInline, snd:FVar(_)}:
				case {snd:FVar(_)}: core.Error.error("Invalid accessor '"+core.Ast.s_access(acc)+"' for variable "+name, p);
				case {snd:FProp(_)}: core.Error.error("Invalid accessor '"+core.Ast.s_access(acc)+"' for property "+name, p);
			}
		}
		if (fctx.is_override) {
			switch (c.cl_super) {
				case None: core.Error.error("Invalid override on field '" + name + "': class has no super class", p);
				case _:
			}
		}
		return switch (f.cff_kind) {
			case FVar(t, e):
				create_variable(ctx, cctx, fctx, c, f, t, e, p);
			case FFun(fd):
				create_method(ctx, cctx, fctx, c, f, fd, p);
			case FProp(get, set, t, eo):
				// create_property(ctx, cctx, fctx, c, f, , p);
				trace("ClassInitializer.init_field: todo"); throw false;
		}
	}

	public static function check_overloads (ctx:context.Typecore.Typer, c:core.Type.TClass) : Void {
		// check if field with same signature was declared more than once
		for (f in c.cl_ordered_fields.concat(c.cl_ordered_statics)) {
			if (!core.Meta.has(Overload, f.cf_meta)) {
				var arr = [f].concat(f.cf_overloads);
				for (f2 in arr) {
					try {
						ocaml.List.find(function (f3) {
							return f3 != f2 && codegen.Overloads.same_overload_args(None, f2.cf_type, f3.cf_type, f2, f3);
						}, arr);
						context.Typecore.display_error(ctx, "Another overloaded field of same signature was already declared : " + f2.cf_name, f2.cf_pos);
					}
					catch (_:ocaml.Not_found) {
					}
				}
			}
		}
	}

	public static function init_class (ctx:context.Typecore.Typer, c:core.Type.TClass, p:core.Globals.Pos, context_init:Void->Void, herits:Array<core.Ast.ClassFlag>, fields:Array<core.Ast.ClassField>) {
		var _tmp = create_class_context(ctx, c, context_init, p);
		var ctx = _tmp.fst; var cctx = _tmp.snd;
		if (cctx.is_class_debug) {
			Sys.println("Created class context: " + dump_class_context(cctx));
		}
		var fields = typing.Typeload.patch_class(ctx, c, fields);
		fields = build_field(ctx, cctx, c, fields);
		if (cctx.is_core_api && ctx.com.display.dms_check_core_api) {
			context.Typecore.delay(ctx, PForce, function() {
				typing.Typeload.init_core_api(ctx, c);
			});
		}
		if (!cctx.is_lib && ctx.com.config.pf_overload) {
			context.Typecore.delay(ctx, PForce, function() {
				check_overloads(ctx, c);
			});
		}
		
		function has_field (f, s:Option<{params:core.Type.TParams, c:core.Type.TClass}>) : Bool {
			return switch (s) {
				case None: false;
				case Some({c:c}):
					ocaml.PMap.exists(f, c.cl_fields) || has_field(f, c.cl_super) || ocaml.List.exists(function (i) { return has_field(f, Some(i)); }, c.cl_implements);
			}
		}
		function check_require (metas:core.Ast.Metadata) {
			if (metas.length == 0) {
				return None;
			}
			var l = metas.slice(1);
			return switch (metas[0]) {
				case {name:Require, params:conds}:
					function loop (arr:Array<core.Ast.Expr>) {
						if (arr.length == 0) {
							return check_require(l);
						}
						var _l = arr.slice(1);
						var e = arr[0];
						var sc = switch (e.expr) {
							case EConst(CIdent(s)): s;
							case EBinop(op=(OpEq|OpNotEq|OpGt|OpGte|OpLt|OpLte), {expr:EConst(CIdent(s))}, {expr:EConst(c=(CInt(_)|CFloat(_)|CString(_)))}): 
								s + core.Ast.s_binop(op) + core.Ast.s_constant(c);
							case _: "";
						}
						if (!syntax.ParserEntry.is_true(syntax.ParserEntry.eval(ctx.com.defines, e))) {
							var _tmp = None;
							if (_l.length > 0) {
								switch (_l[_l.length - 1].expr) {
									case EConst(CString(msg)):
										_tmp = Some(msg);
									case _: None;
								}
							}
							return Some({fst:sc, snd:_tmp});
						}
						else {
							return loop(_l);
						}
					}
					loop(conds);
				case _: check_require(l);
			}
		}
		function check_if_feature (metas:core.Ast.Metadata) : Array<String> {
			if (metas.length == 0) { return []; }
			return switch (metas[0]) {
				case {name:IfFeature, params:el}:
					el.map(function (expr) {
						return switch (expr.expr) {
							case EConst(CString(s)): s;
							case _: core.Error.error("String expected", p);
					}});
				case _:
					check_if_feature(metas.slice(1));
			}
		}
		var cl_if_feature = check_if_feature(c.cl_meta);
		var cl_req = check_require(c.cl_meta);
		for (f in fields) {
			var p = f.cff_pos;
			try {
				var _tmp = create_field_context(ctx, cctx, c, f);
				var ctx = _tmp.fst; var fctx = _tmp.snd;
				if (fctx.is_field_debug) {
					Sys.println("Created field context: " + dump_field_context(fctx));
				}
				var cf = init_field(ctx, cctx, fctx, f);
				if (fctx.is_field_debug) {
					Sys.println("Created field: " + core.type.Printer.s_tclass_field("", cf));
				}
				if (fctx.is_static && c.cl_interface && fctx.field_kind != FKInit && !cctx.is_lib) {
					core.Error.error("You can't declare static fields in interfaces", p);
				}
				function set_feature (s) {
					ctx.m.curmod.m_extra.m_if_feature.unshift({s:s,c:c,cf:cf,b:fctx.is_static});
				}
				ocaml.List.iter(set_feature, cl_if_feature);
				ocaml.List.iter(set_feature, check_if_feature(cf.cf_meta));
				var req = check_require(f.cff_meta);
				req = switch (req) {
					case None:
						(fctx.is_static || fctx.field_kind == FKConstructor) ? cl_req : None;
					case _: req;
				}
				switch (req) {
					case None:
					case Some(r):
						cf.cf_kind = Var({v_read:AccRequire(r.fst, r.snd), v_write:AccRequire(r.fst, r.snd)});
				}
				switch (fctx.field_kind) {
					case FKConstructor:
						switch (c.cl_constructor) {
							case None: c.cl_constructor = Some(cf);
							case Some(ctor) if (ctx.com.config.pf_overload):
								if (core.Meta.has(Overload, cf.cf_meta) && core.Meta.has(Overload, ctor.cf_meta)) {
									ctor.cf_overloads.unshift(cf);
								}
								else {
									context.Typecore.display_error(ctx, "If using overloaded constructors, all constructors must be declared with @:overload", (core.Meta.has(Overload, cf.cf_meta)) ? ctor.cf_pos : cf.cf_pos);
								}
							case Some(_):
								context.Typecore.display_error(ctx, "Duplicate constructor", p);
						}
					case FKInit:
					case FKNormal:
						var dup = if (fctx.is_static) {
							ocaml.PMap.exists(cf.cf_name, c.cl_fields) || has_field(cf.cf_name, c.cl_super);
						}
						else {
							ocaml.PMap.exists(cf.cf_name, c.cl_statics);
						}
						if (!cctx.is_native && !c.cl_extern && dup) {
							core.Error.error("Same field name can't be use for both static and instance : " + cf.cf_name, p);
						}
						if (fctx.is_override) {
							c.cl_overrides.unshift(cf);
						}
						function is_var (f:Any) : Bool { // TODO : might bug
							return switch (cf.cf_kind) { // or switch (f.cf_kind) {
								case Var(_): true;
								case _: false;
							}
						}
						if (ocaml.PMap.mem(cf.cf_name, (fctx.is_static) ? c.cl_statics : c.cl_fields)) {
							if (ctx.com.config.pf_overload && core.Meta.has(Overload, cf.cf_meta) && !is_var(f)) {
								var mainf = ocaml.PMap.find(cf.cf_name, (fctx.is_static) ? c.cl_statics : c.cl_fields);
								if (is_var(mainf)) {
									context.Typecore.display_error(ctx, "Cannot declare a variable with same name as a method", mainf.cf_pos);
								}
								if (!core.Meta.has(Overload, mainf.cf_meta)) {
									context.Typecore.display_error(ctx, "Overloaded methods must have @:overload metadata", mainf.cf_pos);
								}
								mainf.cf_overloads.unshift(cf);
							}
							else {
								context.Typecore.display_error(ctx, "Duplicate class field declaration : " + cf.cf_name, p);
							}
						}
						else {
							if (fctx.do_add) {
								add_field(c, cf, (fctx.is_static || fctx.is_macro && ctx.in_macro));
							}
						}
				}
			}
			catch (err:core.Error) {
				switch (err.msg) {
					case Custom(str) if (p.equals(err.pos)):
						context.Typecore.display_error(ctx, str, p);
					case _: throw err;
				}
			}
		}
		switch (cctx._abstract) {
			case Some(a):
				a.a_to_field.reverse();
				a.a_from_field.reverse();
				a.a_ops.reverse();
				a.a_unops.reverse();
				a.a_array.reverse();
			case None:
		}
		c.cl_ordered_statics.reverse();
		c.cl_ordered_fields.reverse();
		// make sure a default contructor with same access as super one will be added to the class structure at some point.
		switch (cctx.uninitialized_final) {
			case Some(pf) if (c.cl_constructor == None):
				context.Typecore.display_error(ctx, "This class has uninitialized final vars, which requires a constructor", p);
				core.Error.error("Example of an uninitialized final var", pf);
			case _:
		}
		// add_constructor does not deal with overloads correctly
		if (!ctx.com.config.pf_overload) {
			typing.Typeload.add_constructor(ctx, c, cctx.force_constructor, p);
		}
		if (core.Meta.has(StructInit, c.cl_meta)) {
			typing.Typeload.check_struct_init_constructor(ctx, c, p);
		}
		// check overloaded constructors
		if (ctx.com.config.pf_overload && !cctx.is_lib) {
			switch (c.cl_constructor) {
				case Some(ctor):
					context.Typecore.delay(ctx, PTypeField, function() {
						var _tmpArray = [ctor].concat(ctor.cf_overloads);
						for (f in _tmpArray) {
							try {
								// TODO: consider making a broader check, and treat some types, like TAnon and type parameters as Dynamic
								ocaml.List.find(function (f2) {
									return !f.equals(f2) && codegen.Overloads.same_overload_args(f.cf_type, f2.cf_type, f, f2);
								}, _tmpArray);
								context.Typecore.display_error(ctx, "Another overloaded field of same signature was already declared : " + f.cf_name, f.cf_pos);
							}
							catch (_:ocaml.Not_found) {}
						}
					});
				case _:
			}
		}
		// push delays in reverse order so they will be run in correct order
		for (de in cctx.delayed_expr) {
			var ctx = de.typer; var r = de.olazy;
			context.Typecore.init_class_done(ctx);
			switch (r) {
				case None:
				case Some(r):
					context.Typecore.delay(ctx, PTypeField, function () {
						core.Type.lazy_type(r);
					});
			}
		}
	}
}