package context;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
import ocaml.DynArray;
import ocaml.Hashtbl;
import ocaml.List;
import ocaml.PMap;
import ocaml.Ref;
using ocaml.Cloner;

using haxe.EnumTools.EnumValueTools;
// import haxe.EnumTools.EnumValueTools;

using equals.Equal;

class Forbid_package {
	public var a:{pack:String,m:core.Path, p:core.Globals.Pos};
	public var pl:ImmutableList<core.Globals.Pos>;
	public var pf:String;
	public function new (a:{pack:String,m:core.Path, p:core.Globals.Pos}, pl:ImmutableList<core.Globals.Pos>, pf:String) {
		this.a = a;
		this.pl = pl;
		this.pf = pf;
	}
}
class WithTypeError {
	public var m:core.Error.ErrorMsg;
	public var pos:core.Globals.Pos;
	public function new (m:core.Error.ErrorMsg, pos:core.Globals.Pos) {
		this.m = m;
		this.pos = pos;
	}
}

enum WithType {
	NoValue;
	Value;
	WithType (v:core.Type.T);
}

typedef TypePatch = {
	tp_type : Option<core.Ast.ComplexType>,
	tp_remove : Bool,
	tp_meta : core.Ast.Metadata
}

enum CurrentFun {
	FunMember;
	FunStatic;
	FunConstructor;
	FunMemberAbstract;
	FunMemberClassLocal;
	FunMemberAbstractLocal;
}

enum MacroMode {
	MExpr;
	MBuild;
	MMacroType;
	MDisplay;
}

enum TyperPass {
	PBuildModule; // build the module structure and setup module type parameters
	PBuildClass; // build the class structure
	PTypeField; // type the class field, allow access to types structures
	PCheckConstraint; // perform late constraint checks with inferred types
	PForce; // usually ensure that lazy have been evaluated
	PFinal; // not used, only mark for finalize
}

typedef TyperModule = {
	curmod : core.Type.ModuleDef,
	module_types : ImmutableList<{mt:core.Type.ModuleType, pos:core.Globals.Pos}>,
	module_using : ImmutableList<{tc:core.Type.TClass, pos:core.Globals.Pos}>,
	module_globals : PMap<String, {a:core.Type.ModuleType, b:String, pos:core.Globals.Pos}>,
	wildcard_packages : ImmutableList<{l:ImmutableList<String>, pos:core.Globals.Pos}>,
	module_imports : ImmutableList<core.Ast.Import>
}

@:structInit
class TyperGlobals {
	public var types_module : Hashtbl<core.Path, core.Path>;
	public var modules : Hashtbl<core.Path, core.Type.ModuleDef>;
	public var delayed : ImmutableList<{fst:TyperPass, snd:ImmutableList<Void->Void>}>;
	public var debug_delayed : ImmutableList<{fst:TyperPass, snd:ImmutableList<{f:Void->Void, s:String, t:context.Typecore.Typer}>}>;
	public var doinline : Bool;
	public var core_api : Option<Typer>;
	public var macros : Option<{f:Void->Void, t:Typer}>;
	public var std : core.Type.ModuleDef;
	public var hook_generate : ImmutableList<Void->Void>;
	public var type_patches : Hashtbl<core.Path, {map:Hashtbl<{s:String, b:Bool}, context.Typecore.TypePatch>, tp:context.Typecore.TypePatch}>;
	public var global_metadata : ImmutableList<{l:ImmutableList<String>, me:core.Ast.MetadataEntry, bs:{a:Bool, b:Bool, c:Bool}}>;
	public var module_check_policies : ImmutableList<{l:ImmutableList<String>, mcps:ImmutableList<core.Type.ModuleCheckPolicy>, b:Bool}>;
	public var get_build_infos : Void->Option<{mt:core.Type.ModuleType, l:core.Type.TParams, cfs: ImmutableList<core.Ast.ClassField>}>;
	public var delayed_macros : DynArray<Void->Void>;
	public var global_using : ImmutableList<{tc:core.Type.TClass, pos:core.Globals.Pos}>;
	// api
	public var do_inherit : Typer->core.Type.TClass->core.Globals.Pos->{is_extends:Bool, tp:core.Ast.PlacedTypePath}->Bool;
	public var do_create : context.Common.Context->Typer;
	public var do_macro : Typer->MacroMode->core.Path->String->ImmutableList<core.Ast.Expr>->core.Globals.Pos->Option<core.Ast.Expr>;
	public var do_load_module : Typer->core.Path->core.Globals.Pos->core.Type.ModuleDef;
	public var do_optimize : Typer->core.Type.TExpr->core.Type.TExpr;
	// do_build_instance : Typer->core.Type.ModuleType->core.Globals.Pos->{types:ImmutableList<{s:String, t:core.Type.T}>, path:core.Path, f:core.Type.TParams->core.Type.T};
	public var do_build_instance : Typer->core.Type.ModuleType->core.Globals.Pos->{types:core.Type.TypeParams, path:core.Path, f:core.Type.TParams->core.Type.T};
	public var do_format_string : Typer->String->core.Globals.Pos->core.Ast.Expr;
	public var do_finalize : Typer->Void;
	public var do_generate : Typer->{main:Option<core.Type.TExpr>, types:ImmutableList<core.Type.ModuleType>, modules:ImmutableList<core.Type.ModuleDef>};
}


typedef Typer = {
	// shared
	com : context.Common.Context,
	t : core.Type.BasicTypes,
	g : TyperGlobals,
	meta : core.Ast.Metadata,
	this_stack : ImmutableList<core.Type.TExpr>,
	with_type_stack : ImmutableList<WithType>,
	call_argument_stack : ImmutableList<ImmutableList<core.Ast.Expr>>,
	// variable
	pass : TyperPass,
	// per-module
	m : TyperModule,
	is_display_file : Bool,
	// per-class
	curclass : core.Type.TClass,
	tthis : core.Type.T,
	type_params : core.Type.TypeParams,
	// per-function
	curfield : core.Type.TClassField,
	untyped_ : Bool,
	in_loop : Bool,
	in_display : Bool,
	in_macro : Bool,
	macro_depth : Int,
	curfun : CurrentFun,
	ret : core.Type.T,
	locals : PMap<String, core.Type.TVar>,
	opened : ImmutableList<Ref<core.Type.AnonStatus>>,
	vthis : Option<core.Type.TVar>,
	in_call_args : Bool,
	// events
	on_error : Typer->String->core.Globals.Pos->Void
}

class Typecore {
	public static var make_call_ref = new Ref<Typer -> core.Type.TExpr -> ImmutableList<core.Type.TExpr> -> core.Type.T -> core.Globals.Pos -> core.Type.TExpr>(function (_,_,_,_,_) { trace("Shall not be seen"); throw false; });
	public static var type_expr_ref = new Ref<Typer -> core.Ast.Expr -> WithType -> core.Type.TExpr>(function (_,_,_) { trace("Shall not be seen"); throw false; });
	public static var type_module_expr_ref = new Ref<Typer -> core.Type.ModuleType -> Option<ImmutableList<core.Type.T>> -> core.Globals.Pos -> core.Type.TExpr>(function (_,_,_,_) { trace("Shall not be seen"); throw false; });
	public static var match_expr_ref = new Ref<(context.Typecore.Typer, core.Ast.Expr, ImmutableList<core.Ast.Case>, Option<{e:Option<core.Ast.Expr>, pos:core.Globals.Pos}>,context.Typecore.WithType,core.Globals.Pos)->core.Type.TExpr>(function(_,_,_,_,_,_) { trace("Shall not be seen"); throw false; });
	public static var cast_or_unify_ref = new Ref<Typer-> core.Type.T -> core.Type.TExpr -> core.Globals.Pos -> core.Type.TExpr>(context.typecore.AbstractCast.cast_or_unify_raise);
	public static var find_array_access_raise_ref = new Ref<(Typer, core.Type.TAbstract, core.Type.TParams, core.Type.TExpr, Option<core.Type.TExpr>, core.Globals.Pos)->{cf:core.Type.TClassField, tf:core.Type.T, r:core.Type.T, e1:core.Type.TExpr, e2o:Option<core.Type.TExpr>}>(function (_,_,_,_,_,_) { trace("Shall not be seen"); throw false; });
	public static var analyser_run_on_expr_ref = new Ref<context.Common.Context-> core.Type.TExpr -> core.Type.TExpr>(function (_, _) { trace("Shall not be seen"); throw false; });

	public static function display_error (ctx:Typer, msg:String, p:core.Globals.Pos) : Void {
		switch (ctx.com.display.dms_error_policy) {
			case EPShow, EPIgnore: ctx.on_error(ctx, msg, p);
			case EPCollect: context.Common.add_diagnostics_message(ctx.com, msg, p, Error);
		}
	}

	public static function make_call (ctx:Typer, e:core.Type.TExpr, el:ImmutableList<core.Type.TExpr>, t:core.Type.T, p:core.Globals.Pos) : core.Type.TExpr {
		return make_call_ref.get()(ctx, e, el, t, p);
	}

	public static function type_expr (ctx:Typer, e:core.Ast.Expr, with_type:WithType) : core.Type.TExpr {
		return type_expr_ref.get()(ctx, e, with_type);
	}

	public static function match_expr (ctx:Typer, e:core.Ast.Expr, cases:ImmutableList<core.Ast.Case>, def:Option<{e:Option<core.Ast.Expr>, pos:core.Globals.Pos}>, with_type:WithType, p:core.Globals.Pos) : core.Type.TExpr {
		return match_expr_ref.get()(ctx, e, cases, def, with_type, p);
	}

	public static function make_static_this (c:core.Type.TClass, p:core.Globals.Pos) : core.Type.TExpr {
		var ta:core.Type.T = TAnon({a_fields:c.cl_statics, a_status:new Ref<core.Type.AnonStatus>(Statics(c))});
		return core.Type.mk(TTypeExpr(TClassDecl(c)), ta, p);
	}

	public static function make_static_field_access (c:core.Type.TClass, cf:core.Type.TClassField, t:core.Type.T, p:core.Globals.Pos) : core.Type.TExpr {
		var ethis = make_static_this(c, p);
		return core.Type.mk(TField(ethis, FStatic(c, cf)), t, p);
	}

	public static function make_static_call(ctx:Typer, c:core.Type.TClass, cf:core.Type.TClassField, map:core.Type.T->core.Type.T, args:ImmutableList<core.Type.TExpr>, t:core.Type.T, p:core.Globals.Pos) : core.Type.TExpr {
		var monos = List.map(function (_) { return core.Type.mk_mono(); }, cf.cf_params);
		function map_ (t) {
			return map(core.Type.apply_params(cf.cf_params, monos, t));
		}
		var ef = make_static_field_access(c, cf, map(cf.cf_type), p);
		return make_call(ctx, ef, args, map_(t), p);
	}

	public static function raise_or_display (ctx:Typer, l:ImmutableList<core.Type.UnifyError>, p:core.Globals.Pos) {
		if (ctx.untyped_) {}
		else if (ctx.in_call_args) {
			throw new WithTypeError(Unify(l), p);
		}
		else {
			display_error(ctx, core.Error.error_msg(Unify(l)), p);
		}
	}

	public static function raise_or_display_message (ctx:Typer, msg:String, p:core.Globals.Pos) : Dynamic {
		if (ctx.in_call_args) {
			throw new WithTypeError(Custom(msg), p);
		}
		else {
			display_error(ctx, msg, p);
		}
		trace("Shall not be seen"); throw false; // for return type dynamic
	}


	public static function unify (ctx:Typer, t1:core.Type.T, t2:core.Type.T, p:core.Globals.Pos) : Void {
		try {
			core.Type.unify(t1, t2);
		}
		catch(err:core.Type.Unify_error) {
			raise_or_display(ctx, err.l, p);
		}
	}

	public static function unify_raise (ctx:Typer, t1:core.Type.T, t2:core.Type.T, p:core.Globals.Pos) : Void {
		try {
			core.Type.unify(t1, t2);
		}
		catch(err:core.Type.Unify_error) {
			// no untyped check
			throw new core.Error(Unify(err.l),p);
		}
	}

	public static function save_locals (ctx:Typer) : Void->Void {
		var locals = ctx.locals;
		return function() { ctx.locals = locals; }
	}

	public static function add_local (ctx:Typer, n:String, t:core.Type.T, p:core.Globals.Pos) : core.Type.TVar {
		var v = core.Type.alloc_var(n, t, p);
		if (core.Define.defined(ctx.com.defines, WarnVarShadowing)) {
			try {
				var v_ = ocaml.PMap.find(n, ctx.locals);
				ctx.com.warning("This variable shadows a previously declared variable", p);
				ctx.com.warning("Previous variable was here", v_.v_pos);
			}
			catch (_:ocaml.Not_found) {}
		}
		ctx.locals = PMap.add(n, v, ctx.locals);
		return v;
	}

	public static final gen_local_prefix = "`";

	public static function gen_local (ctx:Typer, t:core.Type.T, p:core.Globals.Pos) : core.Type.TVar {
		// ensure that our generated local does not mask an existing one
		function loop (n:Int) : String {
			var nv = (n == 0) ? gen_local_prefix : gen_local_prefix + n;
			if (PMap.mem(nv, ctx.locals)) {
				return loop(n+1);
			}
			else {
				return nv;
			}
		}
		return add_local(ctx, loop(0), t, p);
	}

	public static function delay (ctx:Typer, p:TyperPass, f:Void->Void) : Void {
		function loop (s:ImmutableList<{fst:TyperPass, snd:ImmutableList<Void->Void>}>) :ImmutableList<{fst:TyperPass, snd:ImmutableList<Void->Void>}>{
			return switch (s) {
				case []: [{fst:p, snd:[f]}];
				case {fst:p2, snd:l}::rest:
					var rest:ImmutableList<{fst:TyperPass, snd:ImmutableList<Void->Void>}> = rest;
					if (p2.equals(p)) {
						{fst:p, snd:(f::l)}::rest;
					}
					else if (typerPassToInt(p2) < typerPassToInt(p)) {
						{fst:p2, snd:l}::loop(rest);
					}
					else {
						var _tmp1:{fst:TyperPass, snd:ImmutableList<Void->Void>} = {fst:p, snd:[f]};
						var _tmp2:{fst:TyperPass, snd:ImmutableList<Void->Void>} = {fst:p2, snd:l};
						_tmp1 :: _tmp2 :: rest;
					}
			}
		}
		ctx.g.delayed = loop(ctx.g.delayed);
	}

	public static function delay_late (ctx:Typer, p:TyperPass, f:Void->Void) : Void {
		function loop (s:ImmutableList<{fst:TyperPass, snd:ImmutableList<Void->Void>}>) :ImmutableList<{fst:TyperPass, snd:ImmutableList<Void->Void>}>{
			return switch (s) {
				case []: [{fst:p, snd:[f]}];
				case {fst:p2, snd:l}::rest:
					var rest:ImmutableList<{fst:TyperPass, snd:ImmutableList<Void->Void>}> = rest;
					if (typerPassToInt(p2) <= typerPassToInt(p)) {
						{fst:p2, snd:l}::loop(rest);
					}
					else {
						var _tmp1:{fst:TyperPass, snd:ImmutableList<Void->Void>} = {fst:p, snd:[f]};
						var _tmp2:{fst:TyperPass, snd:ImmutableList<Void->Void>} = {fst:p2, snd:l};
						_tmp1 :: _tmp2 :: rest;
					}
			}
		}
		ctx.g.delayed = loop(ctx.g.delayed);
	}

	public static function flush_pass (ctx:Typer, p:TyperPass, where:String) {
		switch (ctx.g.delayed) {
			case {fst:p2, snd:l}::rest if (typerPassToInt(p2) <= typerPassToInt(p)):
				switch (l) {
					case []:
						ctx.g.delayed = rest;
					case f::l:
						ctx.g.delayed = {fst:p2, snd:l}::rest;
						f();
				}
				flush_pass(ctx, p, where);
			case _:
		}
	}

	public static function make_pass (ctx:Typer, f:Void->core.Type.BuildState) : Void->core.Type.BuildState {
		return f;
	}

	public static function init_class_done (ctx:Typer) : Void {
		ctx.pass = PTypeField;
	}


	public static function exc_protect (?force:Bool=true, ctx:Typer, f:Ref<core.Type.TLazy>->core.Type.T, where:String) : Ref<core.Type.TLazy> {
		var r = new Ref(core.Type.lazy_available(core.Type.t_dynamic));
		r.set(core.Type.lazy_wait(function () {
			return try {
				var t = f(r);
				r.set(core.Type.lazy_available(t));
				t;
			}
			catch (err:core.Error) {
				throw new core.Error.Fatal_error(core.Error.error_msg(err.msg), err.pos);
			}
		}));
		if (force) {
			delay(ctx, PForce, function () {core.Type.lazy_type(r);});
		}
		return r;
	}

	public static function push_this (ctx:Typer, e:core.Type.TExpr) : {fst:core.Ast.Expr, snd:Void->Void} {
		return switch (e.eexpr) {
			case TConst(ct=(TInt(_) | TFloat(_) | TString(_) | TBool(_))):
				{fst: {expr:EConst(core.Type.tconst_to_const(ct)), pos:e.epos}, snd: function () {}};
			case _:
				ctx.this_stack = e::ctx.this_stack;
				var er:core.Ast.Expr = {expr:EMeta({name:This, params:[], pos:e.epos}, {expr:EConst(CIdent("this")), pos:e.epos}), pos:e.epos};
				{fst:er, snd:function() { ctx.this_stack = List.tl(ctx.this_stack); }};
		}
	}

	public static function is_removable_field (ctx:Typer, f:core.Type.TClassField) : Bool {
		return core.Meta.has(Extern, f.cf_meta) || core.Meta.has(Generic, f.cf_meta) ||
			(switch (f.cf_kind) {
				case Var({v_read:AccRequire(s, _)}): true;
				case Method(MethMacro): !ctx.in_macro;
				case _: false;
			});
	}

	/**
	 * checks if we can access to a given class field using current context
	 */
	public static function can_access (ctx:Typer, ?in_overload:Bool=false, c:core.Type.TClass, cf:core.Type.TClassField, stat:Bool) : Bool {
		return
		if (cf.cf_public) {
			true;
		}
		else if (!in_overload && ctx.com.config.pf_overload && core.Meta.has(Overload, cf.cf_meta)) {
			true;
		}
		else {
			// TODO: should we add a c == ctx.curclass short check here?
			// has metadata path
			function make_path(c:core.Type.TClass, f:core.Type.TClassField) : ImmutableList<String> {
				return switch (c.cl_kind) {
					case KAbstractImpl(a):
						List.append(a.a_path.a, [a.a_path.b, f.cf_name]);
					case KGenericInstance(c, _):
						make_path(c, f);
					case _ if (c.cl_private):
						List.rev(f.cf_name::c.cl_path.b::List.tl(List.rev(c.cl_path.a)));
					case _:
						List.append(c.cl_path.a, [c.cl_path.b, f.cf_name]);
				}
			}
			function expr_path (acc:ImmutableList<String>, e:core.Ast.Expr) {
				return switch (e.expr) {
					case EField(e, f): expr_path(f::acc, e);
					case EConst(CIdent(n)): n :: acc;
					case _: Tl; // []
				}
			}
			function chk_path (psub:ImmutableList<String>, pfull:ImmutableList<String>) : Bool {
				return switch [psub, pfull] {
					case [[], _]: true;
					case [a::l1, b::l2] if (a == b): chk_path(l1, l2);
					case _: false;
				}
			}
			function has (m:core.Meta.StrictMeta, c:core.Type.TClass, f:core.Type.TClassField, path:ImmutableList<String>) : Bool {
				function loop (l:core.Ast.Metadata) : Bool {
					return switch (l) {
						case {name:m2, params:el} :: l if (m.equals(m2)):
							loop(l) || List.exists(function (e) {
								var p = expr_path([], e);
								return p != Tl && chk_path(p, path);
							}, el);
						case _::l: loop(l);
						case []: false;
					}
				}
				// return loop(c.cl_meta) || loop(f.cf_meta);
				return loop(f.cf_meta) || loop(c.cl_meta);
			}
			var cur_paths = new Ref([]);
			function loop (c:core.Type.TClass) : Void {
				cur_paths.set(make_path(c, ctx.curfield)::cur_paths.get());
				switch (c.cl_super) {
					case Some({c:csup}): loop(csup);
					case None:
				}
				List.iter(function (arg) { var c = arg.c; return loop(c); }, c.cl_implements);
			}
			loop(ctx.curclass);
			var is_constr = cf.cf_name == "new";
			function loop_(c:core.Type.TClass) : Bool {
				return has(Access, ctx.curclass, ctx.curfield, make_path(c, cf)) ||
					(switch (c.cl_super) {
						case Some({c:csup}): loop_(csup);
						case None: false;
					}) || (
						try {
							// if our common ancestor declare/override the field, then we can access it
							var f = if (is_constr) {
								switch (c.cl_constructor) {
									case None: throw ocaml.Not_found.instance;
									case Some(c): c;
								}
							}
							else {
								PMap.find(cf.cf_name, (stat) ? c.cl_statics : c.cl_fields);
							}
							List.exists(has.bind(Allow, c, f), cur_paths.get()) || core.Type.is_parent(c, ctx.curclass);
						}
						catch (_:ocaml.Not_found) {
							false;
						}
					);
			}
			var b = core.Meta.has(PrivateAccess, ctx.meta);
			// access is also allowed of we access a type parameter which is constrained to our (base) class
			b = b || (switch (c.cl_kind) {
				case KTypeParameter(tl):
					List.exists(function (t:core.Type.T) {
						return switch (core.Type.follow(t)) {
							case TInst(c, _): loop_(c);
							case _: false;
						}
					}, tl);
				case _: false;
			});
			b = b || loop_(c);
			// TODO: find out what this does and move it to genas3
			if (b && context.Common.defined(ctx.com, As3) && !core.Meta.has(Public, cf.cf_meta)) {
				cf.cf_meta = ({name:Public, params:[], pos:cf.cf_pos} : core.Ast.MetadataEntry) :: cf.cf_meta;
			}
			b;
		}
	}

	/*
	 * removes the first argument of the class field's function type and all its overloads
	 */
	public static function prepare_using_field (cf:core.Type.TClassField) : core.Type.TClassField {
		return switch (core.Type.follow(cf.cf_type)) {
			case TFun({args:{t:tf}::args, ret:ret}):
				function loop (acc, overloads:ImmutableList<core.Type.TClassField>) {
					return switch (overloads) {
						case (cfo={cf_type:TFun({args:{t:tfo}::args, ret:ret})})::l:
							var tfo = core.Type.apply_params(cfo.cf_params, List.map(function (p) {return p.t; }, cfo.cf_params), tfo);
							// ignore overloads which have a different first argument
							if (core.Type.type_iseq(tf, tfo)) {
								loop(cfo.with({cf_type:core.Type.T.TFun({args:args, ret:ret})})::acc, l);
							}
							else {
								loop(acc, l);
							}
						case _::l:
							loop(acc, l);
						case []: acc;
					}
				}
				cf.with({
					cf_overloads: loop([], cf.cf_overloads),
					cf_type: core.Type.T.TFun({args:args, ret:ret})
				});
			case _: cf;
		}
	}

	static function compareTyperPass (a:TyperPass, b:TyperPass) : Int {
		if (a.equals(b)) { return 0; }
		return (typerPassToInt(a) > typerPassToInt(b)) ? 1 : -1;
	}

	static function typerPassToInt (p:TyperPass) : Int {
		return switch (p) {
			case PBuildModule: 0;
			case PBuildClass: 1;
			case PTypeField: 2;
			case PCheckConstraint: 3;
			case PForce: 4;
			case PFinal: 5;
		};
	}
}