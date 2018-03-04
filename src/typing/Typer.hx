package typing;

import haxe.Utf8;
import haxe.ds.ImmutableList;
import haxe.ds.Option;
import ocaml.Hashtbl;
import ocaml.List;
import ocaml.PMap;
import ocaml.Ref;

using StringTools;
using equals.Equal;
using ocaml.Cloner;

enum SwitchMode {
	CMatch(ol:Option<ImmutableList<{ef:core.Type.TEnumField, s:String, t:core.Type.T}>>, pos:core.Globals.Pos);
	// of (tenum_field * (string * t) option list option * pos)
	CExpr(e:core.Type.TExpr);
}

enum AccessMode {
	MGet;
	MSet;
	MCall;
}

enum AccessKind {
	AKNo(s:String);
	AKExpr(e:core.Type.TExpr);
	AKSet(e:core.Type.TExpr, t:core.Type.T, cf:core.Type.TClassField);
	AKInline(e:core.Type.TExpr, cf:core.Type.TClassField, a:core.Type.TFieldAccess, t:core.Type.T);
	AKMacro(e:core.Type.TExpr, cf:core.Type.TClassField);
	AKUsing(e:core.Type.TExpr, c:core.Type.TClass, cf:core.Type.TClassField, e2:core.Type.TExpr);
	AKAccess(a:core.Type.TAbstract, p:core.Type.TParams, c:core.Type.TClass, e1:core.Type.TExpr, e2:core.Type.TExpr);
}

enum ObjectDeclKind {
	ODKWithStructure(a:core.Type.TAnon);
	ODKWithClass(c:core.Type.TClass, params:core.Type.TParams);
	ODKPlain;
}

enum Type_class {
	KInt;
	KFloat;
	KString;
	KUnk;
	KDyn;
	KOther;
	KParam(t:core.Type.T);
	KAbstract(a:core.Type.TAbstract, tl:core.Type.TParams);
}

enum State {
	Generating;
	Done;
	NotYet;
}

class Typer {
	public static var build_call_ref = new Ref(build_call);
	public static function relative_path (ctx:context.Typecore.Typer, file:String) : String {
		function slashes (path:String) {
			return path.split("\\").join("/");
		}
		var fpath = slashes(core.Path.get_full_path(file));
		var fpath_lower = fpath.toLowerCase();
		var flen = fpath_lower.length;
		function loop (l:ImmutableList<String>) {
			return switch (l) {
				case []: haxe.io.Path.withoutDirectory(file);
				case path::l:
					var spath = slashes(path).toLowerCase();
					var slen = spath.length;
					if (slen > 0 && slen < flen && fpath_lower.substr(0, slen) == spath) {
						fpath.substr(slen, (flen - slen));
					}
					else {
						loop(l);
					}
			}
		}
		return loop(ctx.com.class_path);
	}

	public static function mk_infos (ctx:context.Typecore.Typer, p:core.Globals.Pos, params:ImmutableList<core.Ast.ObjectField>) : core.Ast.Expr {
		var file = if (ctx.in_macro) {
			p.pfile;
		}
		else if (context.Common.defined(ctx.com, AbsolutePath)) {
			core.Path.get_full_path(p.pfile);
		}
		else {
			relative_path(ctx, p.pfile);
		}
		var fields = if (ctx.curfield.cf_name == "") {
			params;
		}
		else {
			({name:"methodName", pos:core.Globals.null_pos, quotes:NoQuotes, expr:{expr:EConst(CString(ctx.curfield.cf_name)), pos:p}}: core.Ast.ObjectField )::params;
		}
		fields = ({name:"className", pos:core.Globals.null_pos, quotes:NoQuotes, expr:{expr:EConst(CString(core.Globals.s_type_path(ctx.curclass.cl_path))), pos:p}}: core.Ast.ObjectField ) :: params;
		fields = ({name:"lineNumber", pos:core.Globals.null_pos, quotes:NoQuotes, expr:{expr:EConst(CInt(Std.string(syntax.Lexer.get_error_line(p)))), pos:p}}: core.Ast.ObjectField ) :: fields;
		fields = ({name:"fileName", pos:core.Globals.null_pos, quotes:NoQuotes, expr:{expr:EConst(CString(file)), pos:p}}: core.Ast.ObjectField ) :: fields;
		return { expr:EObjectDecl(fields), pos:p }
	}

	public static function check_assign (ctx:context.Typecore.Typer, e:core.Type.TExpr) : Void {
		switch (e.eexpr) {
			case TLocal({v_extra:None}), TArray(_), TField(_), TIdent(_):
			case TConst(TThis), TTypeExpr(_) if (ctx.untyped_):
			case _: core.Error.error("Invalid assign", e.epos);
		}
	}

	// type type_class
	public static function classify (t:core.Type.T) : Type_class {
		return switch (core.Type.follow(t)) {
			case TInst({cl_path:{a:[], b:"String"}}, []): KString;
			case TAbstract(a={a_impl:Some(_)}, tl): KAbstract(a, tl);
			case TAbstract({a_path:{a:[], b:"Int"}}, []): KInt;
			case TAbstract({a_path:{a:[], b:"Float"}}, []): KFloat;
			case TAbstract(a, []) if (List.exists(function (t) { return classify(t).match(KInt|KFloat); }, a.a_to)): KParam(t);
			case TInst({cl_kind:KTypeParameter(ctl)}, _) if (List.exists(function (t) { return classify(t).match(KInt|KFloat); }, ctl)): KParam(t);
			case TMono(r) if (r.get() == None): KUnk;
			case TDynamic(_): KDyn;
			case _: KOther;
		}
	}

	public static function get_iterator_param (t:core.Type.T) : core.Type.T {
		return switch (core.Type.follow(t)) {
			case TAnon(a):
				if (a.a_status.get() != Closed) { throw ocaml.Not_found.instance; }
				switch [core.Type.follow(PMap.find("hasNext", a.a_fields).cf_type), core.Type.follow(PMap.find("next", a.a_fields).cf_type)] {
					case [TFun({args:[], ret:tb}), TFun({args:[], ret:t})] if (core.Type.follow(tb).match(TAbstract({a_path:{a:[], b:"Bool"}}, _))):
						if (PMap.fold(function (_, acc:Int) { return acc + 1; }, a.a_fields, 0) != 2) { throw ocaml.Not_found.instance; }
						t;
					case _: throw ocaml.Not_found.instance;
				}
			case _: throw ocaml.Not_found.instance;
		}
	}

	public static function get_iterable_param (t:core.Type.T) : core.Type.T {
		return switch (core.Type.follow(t)) {
			case TAnon(a):
				if (a.a_status.get() != Closed) { throw ocaml.Not_found.instance; }
				switch (core.Type.follow(PMap.find("iterator", a.a_fields).cf_type)) {
					case TFun({args:[], ret:it}):
						var t = get_iterator_param(it);
						if (PMap.fold(function (_, acc:Int) { return acc + 1; }, a.a_fields, 0) != 1) { throw ocaml.Not_found.instance; }
						t;
					case _: throw ocaml.Not_found.instance;
				}
			case _: throw ocaml.Not_found.instance;
		}
	}

	public static function get_abstract_froms (a:core.Type.TAbstract, pl:core.Type.TParams) : core.Type.TParams {
		var l = List.map(core.Type.apply_params.bind(a.a_params, pl), a.a_from);
		return List.fold_left(function (acc:core.Type.TParams, other:{t:core.Type.T, cf:core.Type.TClassField}) {
			var t = other.t; var f = other.cf;
			return switch (core.Type.follow(core.Type.field_type(f))) {
				case TFun({args:[{t:v}], ret:t}):
					try {
						core.Type.type_eq(EqStrict, t, TAbstract(a, List.map(core.Type.dup, pl))); // unify fields monomorphs
						v::acc;
					}
					catch (_:core.Type.Unify_error) {
						acc;
					}
				case _:
					acc;
			}
		}, l, a.a_from_field);
	}

	/*
	 * temporally remove the constant flag from structures to allow larger unification
	 */
	public static function remove_constant_flag (t:core.Type.T, callb:Bool->AccessKind) : AccessKind {
		var tmp = new Ref<ImmutableList<core.Type.TAnon>>([]);
		function loop (t:core.Type.T) {
			switch (core.Type.follow(t)) {
				case TAnon(a):
					if (a.a_status.get() == Const) {
						a.a_status.set(Closed);
						tmp.set(a::tmp.get());
					}
					PMap.iter(function(_, f:core.Type.TClassField) { return loop(f.cf_type); }, a.a_fields);
				case _:
			}
		}
		function restore () {
			List.iter(function (a:core.Type.TAnon) { a.a_status.set(Const); }, tmp.get());
		}
		try {
			loop(t);
			var ret = callb(tmp.get() != Tl);
			restore();
			return ret;
		}
		catch (_:Bool) { trace("Shall not be seen"); throw false; }
		catch (e:Any) {
			restore();
			throw e;
		}
	}

	public static function is_pos_infos (t:core.Type.T) : Bool {
		return switch (t) {
			case TMono(r):
				switch (r.get()) {
					case Some(t): is_pos_infos(t);
					case _: false;
				}
			case TLazy(f):
				is_pos_infos(core.Type.lazy_type(f));
			case TType({t_path:{a:["haxe"], b:"PosInfos"}}, []):
				true;
			case TType(t, tl):
				is_pos_infos(core.Type.apply_params(t.t_params, tl, t.t_type));
			case TAbstract({a_path:{a:[], b:"Null"}}, [t]):
				is_pos_infos(t);
			case _:
				false;
		}
	}

	public static function check_constraints (ctx:context.Typecore.Typer, tname:String, tpl:core.Type.TypeParams, tl:core.Type.TParams, map:core.Type.T->core.Type.T, delayed:Bool, p:core.Globals.Pos) : Void {
		List.iter2(function (m, other) {
			var name = other.name; var t = other.t;
			switch (core.Type.follow(t)) {
				case TInst({cl_kind:KTypeParameter(constr)}, _) if (constr != Tl):
					var f = function () {
						List.iter(function(ct) {
							try {
								core.Type.unify(map(m), map(ct));
							}
							catch (ue:core.Type.Unify_error) {
								var l = ue.l;
								var l = core.Type.UnifyError.Constraint_failure(tname+"."+name)::l;
								throw new core.Type.Unify_error(l);
							}
						}, constr);
					}
					if (delayed) {
						context.Typecore.delay(ctx, PCheckConstraint, function () {
							try {
								f();
							}
							catch (ue:core.Type.Unify_error) {
								var l = ue.l;
								context.Typecore.display_error(ctx, core.Error.error_msg(Unify(l)), p);
							}
						});
					}
					else {
						f();
					}
				case _:
			}
		}, tl, tpl);
	}

	public static function enum_field_type (ctx:context.Typecore.Typer, en:core.Type.TEnum, ef:core.Type.TEnumField, tl_en:core.Type.TParams, tl_ef:core.Type.TParams, p:core.Globals.Pos) : core.Type.T {
		function map (t:core.Type.T) : core.Type.T {
			return core.Type.apply_params(en.e_params, tl_en, core.Type.apply_params(ef.ef_params, tl_ef, t));
		}
		try {
			check_constraints(ctx, core.Globals.s_type_path(en.e_path), en.e_params, tl_en, map, true, p);
			check_constraints(ctx, ef.ef_name, ef.ef_params, tl_ef, map, true, p);
		}
		catch (ue:core.Type.Unify_error) {
			var l = ue.l;
			context.Typecore.display_error(ctx, core.Error.error_msg(Unify(l)), p);
		}
		return map(ef.ef_type);
	}

	public static function add_constraint_check (ctx:context.Typecore.Typer, ctypes:core.Type.TypeParams, pl:core.Type.TParams, f:core.Type.TClassField, tl:core.Type.TParams, p:core.Globals.Pos) : Void{
		List.iter2(function (m:core.Type.T, tp:{name:String, t:core.Type.T}) {
			var name = tp.name; var t = tp.t;
			switch (core.Type.follow(t)) {
				case TInst({cl_kind:KTypeParameter(constr)},_) if (constr!=Tl):
					var constr = List.map(function (t) {
						var t = core.Type.apply_params(f.cf_params, tl, t);
						// only apply params if not static : in that case no param is passed
						var t = (pl == Tl) ? t : core.Type.apply_params(ctypes, pl, t);
						return t;
					}, constr);
					context.Typecore.delay(ctx, PCheckConstraint, function () {
						List.iter(function (ct) {
							try {
								// if has_mono m then raise (Unify_error [Unify_custom "Could not resolve full type for constraint checks"; Unify_custom ("Type was " ^ (s_type (print_context()) m))]);
								core.Type.unify(m, ct);
							}
							catch (ue:core.Type.Unify_error) {
								var l = ue.l;
								context.Typecore.display_error(ctx, core.Error.error_msg(Unify(core.Type.UnifyError.Constraint_failure(f.cf_name+"."+name)::l)), p);
							}
						}, constr);
					});
				case _:
			}
		}, tl, f.cf_params);
	}

	public static function field_type (ctx:context.Typecore.Typer, c:core.Type.TClass, pl:core.Type.TParams, f:core.Type.TClassField, p:core.Globals.Pos) : core.Type.T {
		return switch (f.cf_params) {
			case []: f.cf_type;
			case l:
				var monos = List.map(function (_) { return core.Type.mk_mono(); }, l);
				if (!core.Meta.has(Generic, f.cf_meta)) {
					add_constraint_check(ctx, c.cl_params, pl, f, monos, p);
				}
				core.Type.apply_params(l, monos, f.cf_type);
		}
	}

	public static function class_field (ctx:context.Typecore.Typer, c:core.Type.TClass, tl:core.Type.TParams, name:String, p:core.Globals.Pos) : {fst:Option<{c:core.Type.TClass, params:core.Type.TParams}>, snd:core.Type.T, trd:core.Type.TClassField} {
		return core.Type.raw_class_field(function (f) { return field_type(ctx, c, tl, f, p); }, c, tl, name);
	}

	public static function check_error (ctx:context.Typecore.Typer, err:core.Error.ErrorMsg, p:core.Globals.Pos) : Void {
		switch (err) {
			case Module_not_found({a:[], b:name}) if (context.display.Diagnostics.is_diagnostics_run(ctx)):
				context.DisplayToplevel.handle_unresolved_identifier(ctx, name, p, true);
			case _: context.Typecore.display_error(ctx, core.Error.error_msg(err), p);
		}
	}
	// ----------------------------------------------------------------------
	// PASS 3 : type expression & check structure

	public static function unify_min_raise (ctx:context.Typecore.Typer, el:ImmutableList<core.Type.TExpr>) : core.Type.T {
		function base_types (t:core.Type.T) {
			var tl = new Ref<ImmutableList<core.Type.T>>([]);
			function loop(t:core.Type.T) {
				switch (t) {
					case TInst(cl, params):
						switch (cl.cl_kind) {
							case KTypeParameter(tl): List.iter(loop, tl);
							case _:
						}
						List.iter(function (imp) {
							var ic = imp.c; var ip = imp.params;
							var t = core.Type.apply_params(cl.cl_params, params, TInst(ic, ip));
							loop(t);
						}, cl.cl_implements);
						switch (cl.cl_super) {
							case None:
							case Some({c:csup, params:pl}):
								var t = core.Type.apply_params(cl.cl_params, params, TInst(csup, pl));
								loop(t);
						}
						tl.set(t::tl.get());
					case TEnum(en, tl2=(_::_)):
						tl.set(core.Type.T.TEnum(en, List.map(function(_) { return core.Type.t_dynamic; }, tl2))::tl.get());
						tl.set(t::tl.get());
					case TType(td, pl):
						loop(core.Type.apply_params(td.t_params, pl, td.t_type));
						// prioritize the most generic definition
						tl.set(t::tl.get());
					case TLazy(f):
						loop(core.Type.lazy_type(f));
					case TMono(r):
						switch (r.get()) {
							case None:
							case Some(t): loop(t);
						}
					case _: tl.set(t::tl.get());
				}
			}
			loop(t);
			return tl.get();
		}
		return
		switch (el) {
			case []: core.Type.mk_mono();
			case [e]: e.etype;
			case _:
				function chk_null(e:core.Type.TExpr) {
					return (switch (e.eexpr) {
						case TConst(TNull): true;
						case TBlock(el):
							switch (List.rev(el)) {
								case []: false;
								case e::_: chk_null(e);
							}
						case TParenthesis(e), TMeta(_, e): chk_null(e);
						case _: false;
					}) || core.Type.is_null(e.etype);
				}
				// First pass: Try normal unification and find out if null is involved.
				function loop (t:core.Type.T, el:ImmutableList<core.Type.TExpr>) : {fst:Bool, snd:core.Type.T} {
					return switch (el) {
						case []: {fst:false, snd:t};
						case e::el:
							var t = (chk_null(e)) ? ctx.t.tnull(t) : t;
							try {
								context.Typecore.unify_raise(ctx, e.etype, t, e.epos);
								loop(t, el);
							}
							catch (err:core.Error) {
								switch (err.msg) {
									case Unify(_):
										try {
											context.Typecore.unify_raise(ctx, t, e.etype, e.epos);
											loop((core.Type.is_null(t)) ? ctx.t.tnull(e.etype) : e.etype, el);
										}
										catch (err2:core.Error) {
											switch (err2.msg) {
												case Unify(_):
													{fst:true, snd:t};
												case _: throw err2;
											}
										}
									case _: throw err;
								}
							}
					}
				}
				var _tmp = loop(core.Type.mk_mono(), el);
				var has_error = _tmp.fst; var t = _tmp.snd;
				if (!has_error) {
					t;
				}
				else {
					try {
						// specific case for const anon : we don't want to hide fields but restrict their common type
						var fcount = new Ref(-1);
						function field_count(a:core.Type.TAnon) : Int {
							return PMap.fold(function (_, acc:Int) {
								return acc + 1;
							}, a.a_fields, 0);
						}
						function expr (f:core.Type.TClassField) : core.Type.TExpr {
							return switch (f.cf_expr) {
								case None: core.Type.mk(TBlock([]), f.cf_type, f.cf_pos);
								case Some(e): e;
							}
						}
						var fields = List.fold_left(function (acc, e:core.Type.TExpr) : PMap<String, ImmutableList<core.Type.TExpr>> {
							return switch(core.Type.follow(e.etype)) {
								case TAnon(a) if (a.a_status.get() == Const):
									if (fcount.get() == -1) {
										fcount.set(field_count(a));
										PMap.map(function (f:core.Type.TClassField) : ImmutableList<core.Type.TExpr> {
											return [expr(f)];
										}, a.a_fields);
									}
									else {
										if (fcount.get() != field_count(a)) {
											throw ocaml.Not_found.instance;
										}
										PMap.mapi(function (n:String, el:ImmutableList<core.Type.TExpr>) : ImmutableList<core.Type.TExpr> {
											return expr(PMap.find(n, a.a_fields)) :: el;
										}, acc);
									}
								case _: throw ocaml.Not_found.instance;
							}
						}, PMap.empty(), el);
						var fields = PMap.foldi(function (n:String, el:ImmutableList<core.Type.TExpr>, acc:PMap<String, core.Type.TClassField>) {
							var t = try {
								unify_min_raise(ctx, el);
							}
							catch (err:core.Error) {
								switch (err.msg) {
									case Unify(_): throw ocaml.Not_found.instance;
									case _: throw err;
								}
							}
							return PMap.add(n, core.Type.mk_field(n, t, List.hd(el).epos, core.Globals.null_pos), acc);
						}, fields, PMap.empty());
						core.Type.T.TAnon({a_fields:fields, a_status:new Ref<core.Type.AnonStatus>(Closed)});
					}
					catch (_:ocaml.Not_found) {
					/* Second pass: Get all base types (interfaces, super classes and their interfaces) of most general type.
						Then for each additional type filter all types that do not unify. */
						var common_types = base_types(t);
						var dyn_types = List.fold_left(function (acc, t:core.Type.T) : ImmutableList<core.Type.T> {
							function loop(c:core.Type.TClass) : Bool {
								return ( switch (c.cl_super) {
									case None: false;
									case Some({c:c}): loop(c);
								}) || core.Meta.has(UnifyMinDynamic, c.cl_meta);
							}
							return switch (t) {
								case TInst(c, params) if (params != Tl && loop(c)):
									core.Type.T.TInst(c, List.map(function (_) { return core.Type.t_dynamic; }, params))::acc;
								case _: acc;
							}
						}, [], common_types);
						var common_types = new Ref<ImmutableList<core.Type.T>>(switch (List.rev(dyn_types)) {
							case []: common_types;
							case l: List.append(common_types, l);
						});
						function loop (e:core.Type.TExpr) {
							var first_error = new Ref<Option<core.Error>>(None);
							function filter(t:core.Type.T) : Bool {
								return
								try {
									context.Typecore.unify_raise(ctx, e.etype, t, e.epos);
									true;
								}
								catch (err:core.Error) {
									var p = err.pos;
									switch (err.msg) {
										case Unify(l):
											if (first_error.get() == None) {
												first_error.set(Some(err));
											}
											false;
										case _: throw err;
									}
								}
							}
							common_types.set(List.filter(filter, common_types.get()));
							switch [common_types.get(), first_error.get()] {
								case [[], Some(err)]: throw err;
								case _:
							}
						}
						switch (common_types.get()) {
							case []: core.Error.error("No common base type found", core.Ast.punion(List.hd(el).epos, List.hd(List.rev(el)).epos));
							case _:
								List.iter(loop, List.tl(el));
								List.hd(common_types.get());
						}
					}
				}
		}
	}
	public static function unify_min (ctx:context.Typecore.Typer, el:ImmutableList<core.Type.TExpr>) : core.Type.T {
		return try {
			unify_min_raise(ctx, el);
		}
		catch (err:core.Error) {
			switch (err.msg) {
				case Unify(l):
					if (!ctx.untyped_) {
						context.Typecore.display_error(ctx, core.Error.error_msg(err.msg), err.pos);
					}
					List.hd(el).etype;
				case _: throw err;
			}
		}
	}

	public static function is_forced_inline (c:Option<core.Type.TClass>, cf:core.Type.TClassField) : Bool {
		return switch (c) {
			case Some({cl_extern:true}): true;
			case Some({cl_kind:KAbstractImpl(_)}): true;
			case _ if (core.Meta.has(Extern, cf.cf_meta)): true;
			case _: false;
		}
	}

	public static function unify_call_args_ (ctx:context.Typecore.Typer, el:ImmutableList<core.Ast.Expr>, args:ImmutableList<core.Type.TSignatureArg>, r:core.Type.T, callp:core.Globals.Pos, inline_:Bool, force_inline:Bool) : {fst:ImmutableList<{fst:core.Type.TExpr, snd:Bool}>, snd:core.Type.T} {
		var in_call_args = ctx.in_call_args;
		ctx.in_call_args = true;
		function call_error (err:core.Error.CallError, p:core.Globals.Pos) : Dynamic {
			throw new core.Error(Call_error(err), p);
		}
		function arg_error (ul:core.Error.ErrorMsg, name:String, opt:Bool, p:core.Globals.Pos) : Dynamic {
			var err:core.Error.ErrorMsg = Stack(ul, Custom("For " + ((opt) ? "optional " : "") + "function argument '" + name + "'"));
			return call_error(Could_not_unify(err), p);
		}
		function mk_pos_infos (t:core.Type.T) : core.Type.TExpr {
			var infos = mk_infos(ctx, callp, []);
			return type_expr(ctx, infos, WithType(t));
		}
		function default_value (name:String, t:core.Type.T) : core.Type.TExpr {
			return (is_pos_infos(t)) ? mk_pos_infos(t) : core.Type.null_(ctx.t.tnull(t), callp);
		}
		var skipped = new Ref<ImmutableList<{fst:String, snd:core.Error.ErrorMsg, trd:core.Globals.Pos}>>([]);
		var invalid_skips = new Ref<ImmutableList<String>>([]);
		function skip (name:String, ul:core.Error.ErrorMsg, t:core.Type.T, p:core.Globals.Pos) : core.Type.TExpr {
			if (!ctx.com.config.pf_can_skip_non_nullable_argument && !core.Type.is_nullable(t)) {
				invalid_skips.set(name::invalid_skips.get());
			}
			skipped.set({fst:name, snd:ul, trd:p} :: skipped.get());
			return default_value(name, t);
		}
		// let force_inline, is_extern = match cf with Some(TInst(c,_),f) -> is_forced_inline (Some c) f, c.cl_extern | _ -> false, false in
		function type_against (t:core.Type.T, e:core.Ast.Expr) : core.Type.TExpr {
			try {
				var e = type_expr(ctx, e, WithType(t));
				return context.typecore.AbstractCast.cast_or_unify_raise(ctx, t, e, e.epos);
			}
			catch (exc:core.Error) {
				var l = exc.msg; var p = exc.pos;
				if (!l.match(Call_error(_)|Module_not_found(_))) {
					throw new context.Typecore.WithTypeError(l, p);
				}
				else {
					throw exc;
				}
			}
		}
		function loop (el:ImmutableList<core.Ast.Expr>, args:ImmutableList<core.Type.TSignatureArg>) :ImmutableList<{fst:core.Type.TExpr, snd:Bool}> {
			return switch [el, args] {
				case [[], []]:
					switch (List.rev(invalid_skips.get())) {
						case []:
						case name::_: call_error(Cannot_skip_non_nullable(name), callp);
					}
					[];
				case [_, [{name:name, opt:false, t:t}]] if (core.Type.follow(t).match(TAbstract({a_path:{a:["haxe", "extern"], b:"Rest"}}, _))):
					switch (core.Type.follow(t)) {
						case TAbstract({a_path:{a:["haxe", "extern"], b:"Rest"}}, [t]):
							try {
								List.map(function (e:core.Ast.Expr): {fst:core.Type.TExpr, snd:Bool} { return {fst:type_against(t, e), snd:false};}, el);
							}
							catch (exc:context.Typecore.WithTypeError) {
								var ul = exc.m; var p = exc.pos;
								arg_error(ul, name, false, p);
							}
						case _: trace("Shall not be seen"); throw false;
					}
				case [[], {opt:false}::_]:
					call_error(Not_enough_arguments(args), callp);
				case [[], {name:name, opt:true, t:t}::args]:
					switch (loop([], args)) {
						case [] if (!(inline_ && (ctx.g.doinline || force_inline)) && !ctx.com.config.pf_pad_nulls):
							(is_pos_infos(t)) ? [{fst:mk_pos_infos(t), snd:true}] : [];
						case args:
					var e_def = default_value(name, t);
							{fst:e_def, snd:true}::args;
					}
				case [{pos:p}::_, []]:
					switch (List.rev(skipped.get())) {
						case []: call_error(Too_many_arguments, p);
						case {fst:s, snd:ul, trd:p}::_: arg_error(ul, s, true, p);
					}
				case [e::el, {name:name, opt:opt, t:t}::args]:
					try {
						var e = type_against(t, e);
						{fst:e, snd:opt}::loop(el, args);
					}
					catch (exc:context.Typecore.WithTypeError) {
						var ul = exc.m; var p = exc.pos;
						if (opt) {
							var e_def = skip(name, ul, t, p);
							{fst:e_def, snd:true}:: loop (e::el, args);
						}
						else {
							arg_error(ul, name, false, p);
						}
					}

			}
		}
		var el = try {
			loop(el, args);
		}
		catch (_:Bool) { trace("Shall not be seen"); throw false; }
		catch (exc:Any) {
			ctx.in_call_args = in_call_args;
			throw exc;
		}
		ctx.in_call_args = in_call_args;
		return {fst:el, snd:TFun({args:args, ret:r})};
	}

	public static function unify_call_args (ctx:context.Typecore.Typer, el:ImmutableList<core.Ast.Expr>, args:ImmutableList<core.Type.TSignatureArg>, r:core.Type.T, p:core.Globals.Pos, inline_:Bool, force_inline:Bool) : {fst:ImmutableList<core.Type.TExpr>, snd:core.Type.T} {
		var _tmp = unify_call_args_(ctx, el, args, r, p, inline_, force_inline);
		var el = _tmp.fst; var tf = _tmp.snd;
		return {fst:List.map(function (e) { return e.fst; }, el), snd:tf};
	}

	public static function unify_field_call (ctx:context.Typecore.Typer, fa:core.Type.TFieldAccess, el:ImmutableList<core.Ast.Expr>, args:ImmutableList<core.Type.TSignatureArg>, ret:core.Type.T, p:core.Globals.Pos, inline_:Bool) : {fst:ImmutableList<core.Type.TExpr>, snd:core.Type.T, trd:core.Type.TExpr->core.Globals.Pos->core.Type.TExpr} {
		function map_cf (cf0:core.Type.TClassField, map:core.Type.T->core.Type.T, cf:core.Type.TClassField) : {t:core.Type.T, cf:core.Type.TClassField} {
			var t = map (core.Type.monomorphs(cf.cf_params, cf.cf_type));
			switch [cf.cf_expr, cf.cf_kind] {
				case [None, Method(MethInline)] if (!ctx.com.config.pf_overload):
					/* This is really awkward and shouldn't be here. We'll keep it for
						3.2 in order to not break code that relied on the quirky behavior
						in 3.1.3, but it should really be reviewed afterwards.
						Related issue: https://github.com/HaxeFoundation/haxe/issues/3846
					*/
					cf.cf_expr = cf0.cf_expr;
					cf.cf_kind = cf0.cf_kind;
				case _:
			}
			return {t:t, cf:cf};
		}
		function expand_overloads (map:core.Type.T->core.Type.T, cf:core.Type.TClassField) : ImmutableList<{t:core.Type.T, cf:core.Type.TClassField}> {
			return {t:(TFun({args:args, ret:ret}) : core.Type.T), cf:cf}::List.map(map_cf.bind(cf, map), cf.cf_overloads);
		}
		var _tmp = switch (fa) {
			case FStatic(c, cf):
				{fst:expand_overloads(function (t) {return t;}, cf), snd:Some(c), trd:cf, frth:function(cf:core.Type.TClassField) : core.Type.TFieldAccess { return FStatic(c, cf); }};
			case FAnon(cf):
				{fst:expand_overloads(function (t) {return t;}, cf), snd:None, trd:cf, frth:function(cf:core.Type.TClassField) : core.Type.TFieldAccess { return FAnon(cf); }};
			case FInstance(c, tl, cf):
				var map = core.Type.apply_params.bind(c.cl_params, tl);
				var cfl = if (cf.cf_name == "new" || !(core.Meta.has(Overload, cf.cf_meta) && ctx.com.config.pf_overload)) {
					List.map(map_cf.bind(cf, map), cf.cf_overloads);
				}
				else {
					List.map(function (a:{t:core.Type.T, cf:core.Type.TClassField}) { var t=a.t; var cf = a.cf; return {t:map(core.Type.monomorphs(cf.cf_params, t)), cf:cf}; }, codegen.Overloads.get_overloads(c, cf.cf_name));
				}
				{fst:{t:core.Type.T.TFun({args:args, ret:ret}), cf:cf}::cfl, snd:Some(c), trd:cf, frth:function (cf):core.Type.TFieldAccess { return FInstance(c, tl, cf); }};
			case FClosure(co, cf):
				var c = switch (co) { case None: None; case Some({c:c}): Some(c); };
				{fst:expand_overloads(function (t) { return t; }, cf), snd:c, trd:cf, frth:function (cf):core.Type.TFieldAccess { return switch (co) { case None: FAnon(cf); case Some({c:c, params:tl}): FInstance(c, tl, cf); }}};
			case _: core.Error.error("Invalid field call", p);
		}
		var candidates = _tmp.fst; var co = _tmp.snd; var cf = _tmp.trd; var mk_fa = _tmp.frth;
		var is_forced_inline = is_forced_inline(co, cf);
		var is_overload = core.Meta.has(Overload, cf.cf_meta);
		function attempt_call (t:core.Type.T, cf:core.Type.TClassField) : {fst:ImmutableList<{fst:core.Type.TExpr, snd:Bool}>, snd:core.Type.T, trd:core.Type.TExpr->core.Globals.Pos->core.Type.TExpr} {
			return switch (core.Type.follow(t)) {
				case TFun({args:args, ret:ret}):
					var _tmp = unify_call_args_(ctx, el, args, ret, p, inline_, is_forced_inline);
					var el = _tmp.fst; var tf = _tmp.snd;
					function mk_call (ethis, p_field) {
						var ef = core.Type.mk(TField(ethis, mk_fa(cf)), t, p_field);
						return make_call(ctx, ef, List.map(function (arg) { return arg.fst; }, el), ret, p);
					}
					{fst:el, snd:tf, trd:mk_call};
				case _:
					trace("Shall not be seen");
					throw false;
			}
		}
		function maybe_raise_unknown_ident (cerr:core.Error.CallError, p:core.Globals.Pos) : Void {
			function loop (err:core.Error.ErrorMsg) {
				switch (err) {
					case Unknown_ident(_): core.Error.error(core.Error.error_msg(err), p);
					case Stack(e1, e2): loop(e1); loop(e2);
					case _:
				}
			}
			switch (cerr) {
				case Could_not_unify(err): loop(err);
				case _:
			}
		}
		function loop (candidates:ImmutableList<{t:core.Type.T, cf:core.Type.TClassField}>) : {fst:ImmutableList<{fst:ImmutableList<{fst:core.Type.TExpr, snd:Bool}>, snd:core.Type.T, trd:core.Type.TExpr->core.Globals.Pos->core.Type.TExpr}>, snd:ImmutableList<{fst:core.Type.TClassField, snd:core.Error.ErrorMsg, trd:core.Globals.Pos}>} {
			return switch (candidates) {
				case []: {fst:[], snd:[]};
				case {t:t, cf:cf}::candidates:
					try {
						var candidate = attempt_call(t, cf);
						if (ctx.com.config.pf_overload && is_overload) {
							var _tmp = loop(candidates);
							var candidates = _tmp.fst; var failures = _tmp.snd;
							{fst:candidate::candidates, snd:failures};
						}
						else {
							{fst:[candidate], snd:[]};
						}
					}
					catch (exc:core.Error) {
						var err = exc.msg; var p = exc.pos;
						switch (err) {
							case Call_error(cerr):
								maybe_raise_unknown_ident(cerr, p);
								var _tmp = loop(candidates);
								var candidates = _tmp.fst; var failures = _tmp.snd;
								{fst:candidates, snd:{fst:cf, snd:err, trd:p}::failures};
							case _: throw err;
						}
					}
			}
		}
		function fail_fun () : {fst:ImmutableList<core.Type.TExpr>, snd:core.Type.T, trd:core.Type.TExpr->core.Globals.Pos->core.Type.TExpr} {
			var tf:core.Type.T = TFun({args:args, ret:ret});
			return {fst:[], snd:tf, trd:function (ethis:core.Type.TExpr, p_field:core.Globals.Pos) {
				var e1 = core.Type.mk(TField(ethis, mk_fa(cf)), tf, p_field);
				return core.Type.mk(TCall(e1, []), ret, p);
			}};
		}
		return switch (candidates) {
			case [{t:t, cf:cf}]:
				try {
					var _tmp = attempt_call(t, cf);
					var el = _tmp.fst; var tf = _tmp.snd; var mk_call = _tmp.trd;
					{fst:List.map(function (e) { return e.fst; }, el), snd:tf, trd:mk_call};
				}
				catch (exc:core.Error) {
					if (ctx.com.display.dms_error_policy == EPIgnore) {
						fail_fun();
					}
					else {
						throw exc;
					}
				}
			case _:
				var _tmp = loop(candidates);
				var candidates = _tmp.fst; var failures = _tmp.snd;
				function fail () : Dynamic {
					var failures = List.map(function (arg) : {fst:core.Type.TClassField, snd:String, trd:core.Globals.Pos} {
						var cf = arg.fst; var err = arg.snd; var p = arg.trd;
						return {fst:cf, snd:core.Error.error_msg(err), trd:p};
					}, failures);
					failures = core.Ast.remove_duplicates(function (a1:{fst:core.Type.TClassField, snd:String, trd:core.Globals.Pos}, a2:{fst:core.Type.TClassField, snd:String, trd:core.Globals.Pos}) {
						var msg1 = a1.snd; var msg2 = a2.snd;
						return msg1 != msg2;
					}, failures);
					return
					switch (failures) {
						case [{snd:msg, trd:p}]: core.Error.error(msg, p);
						case _:
							context.Typecore.display_error(ctx, "Could not find a suitable overload, reasons follow", p);
							List.iter(function (a) {
								var cf = a.fst; var msg = a.snd; var p2 = a.trd;
								context.Typecore.display_error(ctx, "Overload resolution failed for "+(core.Type.s_type(core.Type.print_context(), cf.cf_type)), p);
								context.Typecore.display_error(ctx, msg, p2);
							}, failures);
							core.Error.error("End of overload failure reasons", p);
					}
				}
				if (is_overload && ctx.com.config.pf_overload) {
					switch (codegen.overloads.Resolution.reduce_compatible(candidates)) {
						case []:
							fail();
						case [{fst:el, snd:tf, trd:mk_call}]:
							{fst:List.map(function (e) { return e.fst; }, el), snd:tf, trd:mk_call};
						case _: core.Error.error("Ambiguous overload", p);
					}
				}
				else {
					switch (List.rev(candidates)) {
						case []:
							fail();
						case {fst:el, snd:tf, trd:mk_call}::_:
							{fst:List.map(function (e) { return e.fst; }, el), snd:tf, trd:mk_call};
					}
				}
		}
	}

	public static function type_module_type (ctx:context.Typecore.Typer, t:core.Type.ModuleType, tparams:Option<Dynamic>, p:core.Globals.Pos) : core.Type.TExpr {
		return switch (t) {
			case TClassDecl({cl_kind:KGenericBuild(_)}):
				var f = typing.Typeload.build_instance(ctx, t, p).f;
				var t = f(switch (tparams) { case None: []; case Some(tl): tl; });
				var mt = try {
					core.Type.module_type_of_type(t);
				}
				catch (_:ocaml.Exit) {
					if (core.Type.follow(t) == core.Type.t_dynamic) {
						typing.Typeload.load_type_def(ctx, p, {tpackage:[], tname:"Dynamic", tparams:[], tsub:None});
					}
					else {
						core.Error.error("Invalid module type", p);
					}
				}
				type_module_type(ctx, mt, None, p);
			case TClassDecl(c):
				var t_tmp = core.Type.class_module_type(c);
				core.Type.mk(TTypeExpr(TClassDecl(c)), TType(t_tmp, []), p);
			case TEnumDecl(e):
				var types = switch (tparams) { case None: List.map(function (_) { return core.Type.mk_mono(); }, e.e_params); case Some(l): l;}
				core.Type.mk(TTypeExpr(TEnumDecl(e)), TType(e.e_type, types), p);
			case TTypeDecl(s):
				var t = core.Type.apply_params(s.t_params, List.map(function (_) { return core.Type.mk_mono(); }, s.t_params), s.t_type);
				if (!context.Common.defined(ctx.com, NoDeprecationWarnings)) {
					context.display.DeprecationCheck.check_typedef(ctx.com, s, p);
				}
				switch (core.Type.follow(t)) {
					case TEnum(e, params):
						type_module_type(ctx, TEnumDecl(e), Some(params), p);
					case TInst(c, params):
						type_module_type(ctx, TClassDecl(c), Some(params), p);
					case TAbstract(a, params):
						type_module_type(ctx, TAbstractDecl(a), Some(params), p);
					case _:
						core.Error.error(core.Globals.s_type_path(s.t_path) + " is not a value", p);
				}
			case TAbstractDecl({a_impl:Some(c)}):
				type_module_type(ctx, TClassDecl(c), tparams, p);
			case TAbstractDecl(a):
				if (!core.Meta.has(RuntimeValue, a.a_meta)) {
					core.Error.error(core.Globals.s_type_path(a.a_path) + " is not a value", p);
				}
				var t_tmp = core.Type.abstract_module_type(a, []);
				core.Type.mk(TTypeExpr(TAbstractDecl(a)), TType(t_tmp, []), p);
		}
	}

	public static function type_type (ctx:context.Typecore.Typer, tpath:core.Path, p:core.Globals.Pos) : core.Type.TExpr {
		return type_module_type(ctx, typing.Typeload.load_type_def(ctx, p, {tpackage:tpath.a, tname:tpath.b, tparams:[], tsub:None}), None, p);
	}

	public static function get_constructor (ctx:context.Typecore.Typer, c:core.Type.TClass, params:core.Type.TParams, p:core.Globals.Pos) : {t:core.Type.T, cf:core.Type.TClassField} {
		return switch (c.cl_kind) {
			case KAbstractImpl(a):
				var f = try {
					PMap.find("_new", c.cl_statics);
				}
				catch (_:ocaml.Not_found) {
					core.Error.raise_error(No_constructor(TAbstractDecl(a)), p);
				}
				var ct = field_type(ctx, c, params, f, p);
				{t:core.Type.apply_params(a.a_params, params, ct), cf:f};
			case _:
				var _tmp = try {
					core.Type.get_constructor(function (f) { return field_type(ctx, c, params, f, p); }, c);
				}
				catch (_:ocaml.Not_found) {
					core.Error.raise_error(No_constructor(TClassDecl(c)), p);
				}
				var ct = _tmp.t; var f = _tmp.cf;
				{t:core.Type.apply_params(c.cl_params, params, ct), cf:f};
		}
	}

	public static function make_call (ctx:context.Typecore.Typer, e:core.Type.TExpr, params:ImmutableList<core.Type.TExpr>, t:core.Type.T, p:core.Globals.Pos) : core.Type.TExpr {
		return
		try {
			var _tmp = switch (e.eexpr) {
				case TField(ethis, fa):
					var _tmp = switch (fa) {
						case FInstance(c, _, cf), FStatic(c, cf): {fst:Some(c), snd:cf};
						case FAnon(cf): {fst:None, snd:cf};
						case _: throw  ocaml.Exit.instance;
					}
					var co = _tmp.fst; var cf = _tmp.snd;
					{fst:ethis, snd:co, trd:cf};
				case _: throw ocaml.Exit.instance;
			}
			var ethis = _tmp.fst; var cl = _tmp.snd; var f = _tmp.trd;
			if (!f.cf_kind.match(Method(MethInline))) { throw ocaml.Exit.instance; }
			var config = switch (cl) {
				case Some({cl_kind:KAbstractImpl(_)}) if (core.Meta.has(Impl, f.cf_meta)):
					var t = if (f.cf_name == "_new") {
						t;
					}
					else if (params == Tl) {
						core.Error.error("Invalid abstract implementation function", f.cf_pos);
					}
					else {
						core.Type.follow(List.hd(params).etype);
					}
					switch (t) {
						case TAbstract(a, pl):
							var has_params = a.a_params != Tl && f.cf_params != Tl;
							var monos = List.map(function (_) { return core.Type.mk_mono(); }, f.cf_params);
							function map_type (t:core.Type.T) : core.Type.T {
								return core.Type.apply_params(a.a_params, pl, core.Type.apply_params(f.cf_params, monos, t));
							}
							Some({fst:has_params, snd:map_type});
						case _: None;
					}
				case _: None;
			}
			core.Type.follow(f.cf_type); // force evaluation
			var params = List.map(ctx.g.do_optimize.bind(ctx), params);
			var force_inline = is_forced_inline(cl, f);
			switch [f.cf_expr_unoptimized, f.cf_expr] {
				case [Some(fd), _], [None, Some({eexpr:TFunction(fd)})]:
					switch (optimization.Optimizer.type_inline(ctx, f, fd, ethis, params, t, config, p, false, force_inline)) {
						case None:
							if (force_inline) { core.Error.error("Inline could not be done", p); }
							throw ocaml.Exit.instance;
						case Some(e): e;
					}
				case _:
					/*
						we can't inline because there is most likely a loop in the typing.
						this can be caused by mutually recursive vars/functions, some of them
						being inlined or not. In that case simply ignore inlining.
					*/
					throw ocaml.Exit.instance;
			}
		}
		catch (_:ocaml.Exit) {
			core.Type.mk(TCall(e, params), t, p);
		}
	}

	public static function mk_array_get_call(ctx:context.Typecore.Typer, other: {cf:core.Type.TClassField, tf:core.Type.T, r:core.Type.T, e1:core.Type.TExpr, e2o:Option<core.Type.TExpr>}, c, ebase, p:core.Globals.Pos) : core.Type.TExpr {
		var cf = other.cf; var tf = other.tf; var r = other.r; var e1 = other.e1; var e2o = other.e2o;
		trace("TODO: typing.Typer.mk_array_get_call");
		throw false;
	}

	public static function mk_array_set_call(ctx:context.Typecore.Typer, other: {cf:core.Type.TClassField, tf:core.Type.T, r:core.Type.T, e1:core.Type.TExpr, e2o:Option<core.Type.TExpr>}, c, ebase, p:core.Globals.Pos) : core.Type.TExpr {
		var cf = other.cf; var tf = other.tf; var r = other.r; var e1 = other.e1; var e2o = other.e2o;
		trace("TODO: typing.Typer.mk_array_set_call");
		throw false;
	}

	public static function acc_get(ctx:context.Typecore.Typer, g:AccessKind, p:core.Globals.Pos) : core.Type.TExpr {
		return switch (g) {
			case AKNo(f): core.Error.error("Field "+f+" cannot be accessed for reading", p);
			case AKExpr(e): e;
			case AKSet(_), AKAccess(_): trace("Shall not be seen"); throw false;
			case AKUsing(et, c, cf, e) if (ctx.in_display):
				// Generate a TField node so we can easily match it for position/usage completion (issue #1968)
				var ec = type_module_type(ctx, TClassDecl(c), None, p);
				var t:core.Type.T = switch(core.Type.follow(et.etype)) {
					case TFun({args:_::args, ret:ret}): TFun({args:args, ret:ret});
					case _: et.etype;
				}
				core.Type.mk(TField(ec, FStatic(c, cf)), t, et.epos);
			case AKUsing(et, _, cf, e):
				// build a closure with first parameter applied
				switch (core.Type.follow(et.etype)) {
					case TFun({args:_::args, ret:ret}):
						var tcallb:core.Type.T = TFun({args:args, ret:ret});
						var twrap:core.Type.T = TFun({args:[{name:"_e", opt:false, t:e.etype}], ret:tcallb});
						// arguments might not have names in case of variable fields of function types, so we generate one (issue #2495)
						var args = List.map(function (arg:core.Type.TSignatureArg) {
							var n = arg.name; var o = arg.opt; var t = arg.t;
							t = (o) ? ctx.t.tnull(t) : t;
							return {fst:o, snd: (n == "") ? context.Typecore.gen_local(ctx, t, e.epos) : core.Type.alloc_var(n, t, e.epos)}; // TODO: var pos
						}, args);
						var ve = core.Type.alloc_var("_e", e.etype, e.epos);
						var ecall = make_call(ctx, et, List.map(function (v) { return core.Type.mk(TLocal(v), v.v_type, p); }, ve::List.map(function (a) { return a.snd; }, args)), ret, p);
						var ecallb = core.Type.mk(
							TFunction({
								tf_args:List.map(function (a) {
									var o = a.fst; var v = a.snd;
									return {v:v, c:(o) ? Some(core.Type.TConstant.TNull) : None};
								}, args),
								tf_type:ret,
								tf_expr: switch (core.Type.follow(ret)) {
									case TAbstract({a_path:{a:[], b:"Void"}}, _): ecall;
									case _: core.Type.mk(TReturn(Some(ecall)), core.Type.t_dynamic, p);
								}
							}),
							tcallb,
							p
						);
						var ewrap = core.Type.mk(TFunction({
							tf_args: [{v:ve, c:None}],
							tf_type: tcallb,
							tf_expr: core.Type.mk(TReturn(Some(ecallb)), core.Type.t_dynamic, p)
						}), twrap, p);
						make_call(ctx, ewrap, [e], tcallb, p);
					case _: trace("Shall not be seen"); throw false;
				}
			case AKInline(e, f, fmode, t):
				// do not create a closure for static calls
				var cmode:core.Type.TFieldAccess = switch (fmode) {
					case FStatic(_): fmode;
					case FInstance(c, tl, f): FClosure(Some({c:c, params:tl}), f);
					case _: trace("Shall not be seen"); throw false;
				}
				core.Type.follow(f.cf_type); // force computing
				switch (f.cf_expr) {
					case None if (ctx.com.display.dms_display):
						core.Type.mk(TField(e, cmode), t, p);
					case None:
						core.Error.error("Recursive inline is not supported", p);
					case Some({eexpr:TFunction(_)}):
						function chk_class (c:core.Type.TClass) : Bool {
							return (c.cl_extern || core.Meta.has(Extern, f.cf_meta)) && !core.Meta.has(Runtime, f.cf_meta);
						}
						function wrap_extern (c:core.Type.TClass) : core.Type.TExpr {
							var c2 = {
								var m = c.cl_module;
								var mpath = new core.Path(List.append(m.m_path.a, ["_"+m.m_path.b]), m.m_path.b+"_Impl_");
								try {
									function loop (mtl:ImmutableList<core.Type.ModuleType>) {
										return switch (mtl) {
											case TClassDecl(c) :: _ if (c.cl_path.equals(mpath)):
												c;
											case _::mtl: loop(mtl);
											case _: throw ocaml.Not_found.instance;
										}
									}
									loop(c.cl_module.m_types);
								}
								catch (_:ocaml.Not_found) {
									var c2 = core.Type.mk_class(c.cl_module, mpath, c.cl_pos, core.Globals.null_pos);
									c.cl_module.m_types = (TClassDecl(c2) : core.Type.ModuleType) :: c.cl_module.m_types;
									c2;
								}
							}
							var cf = try {
								PMap.find(f.cf_name, c2.cl_statics);
							}
							catch (_:ocaml.Not_found) {
								var cf = f.with({
									cf_kind: core.Type.FieldKind.Method(MethNormal)
								});
								c2.cl_statics = PMap.add(cf.cf_name, cf, c2.cl_statics);
								c2.cl_ordered_statics = cf :: c2.cl_ordered_statics;
								cf;
							}
							var e_t = type_module_type(ctx, TClassDecl(c2), None, p);
							return core.Type.mk(TField(e_t, FStatic(c2, cf)), t, p);
						}
						var e_def = core.Type.mk(TField(e, cmode), t, p);
						switch (core.Type.follow(e.etype)) {
							case TInst(c, _) if (chk_class(c)):
								context.Typecore.display_error(ctx, "Can't create closure on an extern inline member method", p);
								e_def;
							case TAnon(a):
								switch (a.a_status.get()) {
									case Statics({cl_extern:false}) if (core.Meta.has(Extern, f.cf_meta)):
										context.Typecore.display_error(ctx, "Cannot create closure on @:extern inline method", p);
										e_def;
									case Statics(c) if (chk_class(c)):
										wrap_extern(c);
									case _: e_def;
								}
							case _: e_def;
						}
					case Some(e):
						function loop (e:core.Type.TExpr) {
							return core.Type.map_expr(loop, e.with({epos:p}));
						}
						loop(e);
				}
			case AKMacro(_):
				trace("Shall not be seen"); throw false;
		}
	}

	public static function error_require (r:String, p:core.Globals.Pos) : Dynamic {
		return if (r == "") {
			core.Error.error("This field is not available with the current compilation flags", p);
		}
		else {
			var r = if (r == "sys") {
				"a system platform (php,neko,cpp,etc.)";
			}
			else {
				try {
					if (r.substr(0, 5) == "flash") { throw ocaml.Exit.instance; }
					var v = r.substr(5).replace("_", ".");
					"flash version "+v+" (use -swf-version "+v+")";
				}
				catch (b:Bool) { throw b; }
				catch (_:Any) {
					"'"+r+"' to be enabled";
				}
			}
			core.Error.error("Accessing this field requires "+r, p);
		}
	}

	public static function get_this (ctx:context.Typecore.Typer, p:core.Globals.Pos) : core.Type.TExpr {
		return switch (ctx.curfun) {
			case FunStatic:
				core.Error.error("Cannot acces this from a static function", p);
			case FunMemberClassLocal, FunMemberAbstractLocal:
				var v = switch (ctx.vthis) {
					case None:
						var v = if (ctx.curfun == FunMemberAbstractLocal) {
							PMap.find("this", ctx.locals);
						}
						else {
							context.Typecore.add_local(ctx, "`this", ctx.tthis, p);
						}
						ctx.vthis = Some(v);
						v;
					case Some(v):
						ctx.locals = PMap.add(v.v_name, v, ctx.locals);
						v;
				}
				core.Type.mk(TLocal(v), ctx.tthis, p);
			case FunMemberAbstract:
				var v = try {
					PMap.find("this", ctx.locals);
				}
				catch (_:ocaml.Not_found) { trace("Shall not be seen"); throw false; }
				core.Type.mk(TLocal(v), v.v_type, p);
			case FunConstructor, FunMember:
				core.Type.mk(TConst(TThis), ctx.tthis, p);
		}
	}

	public static function field_access (ctx:context.Typecore.Typer, mode:AccessMode, f:core.Type.TClassField, fmode:core.Type.TFieldAccess, t:core.Type.T, e:core.Type.TExpr, p:core.Globals.Pos) : AccessKind {
		function fnormal () : AccessKind {
			return AKExpr(core.Type.mk(TField(e, fmode), t, p));
		}
		function normal () : AccessKind {
			return switch (core.Type.follow(e.etype)) {
				case TAnon(a):
					switch (a.a_status.get()) {
						case EnumStatics(en):
							var c = try {
								PMap.find(f.cf_name, en.e_constrs);
							}
							catch (_:ocaml.Not_found) {
								trace("Shall not be seen"); throw false;
							}
							var fmode:core.Type.TFieldAccess = FEnum(en, c);
							AKExpr(core.Type.mk(TField(e, fmode), t, p));
						case _: fnormal();
					}
				case _: fnormal();
			}
		}
		return switch (f.cf_kind) {
			case Method(m):
				if (mode == MSet && m != MethDynamic && !ctx.untyped_) {
					core.Error.error("Cannot rebind this method : please use 'dynamic' before method declaration", p);
				}
				switch [ctx.curfun, e.eexpr] {
					case [(FunMemberAbstract | FunMemberAbstractLocal), TTypeExpr(TClassDecl(c={cl_kind:KAbstractImpl(a)}))] if (c.equals(ctx.curclass) && core.Meta.has(Impl, f.cf_meta)):
						var e = core.Type.mk(TField(e, fmode), t, p);
						var ethis = get_this(ctx, p);
						ethis.etype = TAbstract(a, List.map(function (param) { return param.t; }, a.a_params));
						AKUsing(e, ctx.curclass, f, ethis);
					case _:
						switch [m, mode] {
							case [MethInline, _]: AKInline(e, f, fmode, t);
							case [MethMacro, MGet]:
								context.Typecore.display_error(ctx, "Macro functions must be called immediately", p);
								normal();
							case [MethMacro, MCall]:
								AKMacro(e, f);
							case [_, MGet]:
								var cmode:core.Type.TFieldAccess = switch (fmode) {
									case FInstance(_, _, cf), FStatic(_, cf) if (core.Meta.has(Generic, cf.cf_meta)):
										context.Typecore.display_error(ctx, "Cannot create closure on generic function", p);
										fmode;
									case FInstance(c, tl, cf): FClosure(Some({c:c, params:tl}), cf);
									case FStatic(_), FEnum(_): fmode;
									case FAnon(f): FClosure(None, f);
									case FDynamic(_), FClosure(_): trace("Shall not be seen"); throw false;
								}
								AKExpr(core.Type.mk(TField(e, cmode), t, p));
							case _:
								normal();
						}
				}
			case Var(v):
				switch ( switch (mode) { case MGet, MCall: v.v_read; case MSet: v.v_write; }) {
					case AccNo if (!core.Meta.has(PrivateAccess, ctx.meta)):
						switch (core.Type.follow(e.etype)) {
							case TInst(c, _) if (core.Type.is_parent(c, ctx.curclass) || context.Typecore.can_access(ctx, c, f.with({cf_public:false}), false)): normal();
							case TAnon(a):
								switch (a.a_status.get()) {
									case Opened if (mode == MSet):
										f.cf_kind = Var({v_read:v.v_read, v_write:AccNormal});
										normal();
									case Statics(c2) if (ctx.curclass.equals(c2) || context.Typecore.can_access(ctx, c2, f.with({cf_public:false}), true)): normal();
									case _: (ctx.untyped_) ? normal() : AKNo(f.cf_name);
								}
							case _:
								(ctx.untyped_) ? normal() : AKNo(f.cf_name);
						}
					case AccNormal, AccNo:
						// if we are reading from a read-only variable on an anonymous object, it might actually be a method, so make sure to create a closure
						function is_maybe_method() : Bool {
							return switch [v.v_write, core.Type.follow(t), core.Type.follow(e.etype)] {
								case [(AccNo | AccNever), TFun(_), TAnon(a)]:
									switch (a.a_status.get()) {
										case Statics(_), EnumStatics(_): false;
										case _: true;
									}
								case _: false;
							}
						}
						if (mode == MGet && is_maybe_method()) {
							AKExpr(core.Type.mk(TField(e, FClosure(None, f)), t, p));
						}
						else {
							normal();
						}
					case AccCall if (ctx.in_display):
						normal();
					case AccCall:
						var m = switch (mode) {
							case MSet: "set_";
							case _: "get_";
						};
						m = m + f.cf_name;
						function is_abstract_this_access() : Bool {
							return switch [e.eexpr, ctx.curfun] {
								case [TTypeExpr(TClassDecl(c={cl_kind:KAbstractImpl(_)})), (FunMemberAbstract | FunMemberAbstractLocal)]:
									c.equals(ctx.curclass);
								case _: false;
							}
						}
						if (m.equals(ctx.curfield.cf_name) && switch (e.eexpr) { case TConst(TThis): true; case TLocal(v): ocaml.Option.map_default(function (vthis) { return v.equals(vthis); }, false, ctx.vthis); case TTypeExpr(TClassDecl(c)) if (c == ctx.curclass) : true; case _:false;}) {
							var prefix = switch (ctx.com.platform) { case Flash if (context.Common.defined(ctx.com, As3)): "$"; case _: ""; };
							switch (e.eexpr) {
								case TLocal(_) if (context.Common.defined(ctx.com, Haxe3Compat)):
									ctx.com.warning("Field set has changed here in Haxe 4: call setter explicitly to keep Haxe 3.x behaviour", p);
								case _:
							}
							if (!core.Type.is_physical_field(f)) {
								context.Typecore.display_error(ctx, "This field cannot be accessed because it is not a real variable", p);
								context.Typecore.display_error(ctx, "Add @:isVar here to enable it", f.cf_pos);
							}
							AKExpr(core.Type.mk(TField(e, (prefix == "") ? fmode: FDynamic(prefix+f.cf_name)), t, p));
						}
						else if (is_abstract_this_access()) {
							var this_ = get_this(ctx, p);
							if (mode == MSet) {
								var _tmp = switch (ctx.curclass) { case c={cl_kind:KAbstractImpl(a)}: {fst:c, snd:a}; case _: trace("Shall not be seen"); throw false;}
								var c = _tmp.fst; var a = _tmp.snd;
								var f = PMap.find(m, c.cl_statics);
								// we don't have access to the type parameters here, right ?
								// let t = apply_params a.a_params pl (field_type ctx c [] f p) i
								var t = field_type(ctx, c, [], f, p);
								var ef = core.Type.mk(TField(e, FStatic(c, f)), t, p);
								AKUsing(ef, c, f, this_);
							}
							else {
								AKExpr(make_call(ctx, core.Type.mk(TField(e, core.Type.quick_field_dynamic(e.etype, m)), core.Type.tfun([this_.etype], t), p), [this_], t, p));
							}
						}
						else if (mode == MSet) {
							AKSet(e, t, f);
						}
						else {
							AKExpr(make_call(ctx, core.Type.mk(TField(e, core.Type.quick_field_dynamic(e.etype, m)), core.Type.tfun([], t), p), [], t, p));
						}
					case AccResolve:
						var fstring = core.Type.mk(TConst(TString(f.cf_name)), ctx.t.tstring, p);
						var tresolve = core.Type.tfun([ctx.t.tstring], t);
						AKExpr(make_call(ctx, core.Type.mk(TField(e, FDynamic("resolve")), tresolve, p), [fstring], t, p));
					case AccNever:
						(ctx.untyped_) ? normal() : AKNo(f.cf_name);
					case AccInline:
						AKInline(e, f, fmode, t);
					case AccCtor:
						(ctx.curfun == FunConstructor) ? normal() : AKNo(f.cf_name);
					case AccRequire(r, msg):
						switch (msg) {
							case None: error_require(r, p);
							case Some(msg): core.Error.error(msg, p);
						}
				}
		}
	}

	public static function using_field (ctx:context.Typecore.Typer, mode:AccessMode, e:core.Type.TExpr, i:String, p:core.Globals.Pos) : AccessKind {
		if (mode == MSet) {
			throw ocaml.Not_found.instance;
		}
		// do not try to find using fields if the type is a monomorph, which could lead to side-effects
		var is_dynamic = switch (core.Type.follow(e.etype)) {
			case TMono(_): throw ocaml.Not_found.instance;
			case t: (t == core.Type.t_dynamic);
		}
		var check_constant_struct = new Ref(false);
		function loop (l:ImmutableList<{tc:core.Type.TClass, pos:core.Globals.Pos}>) {
			return
			switch (l) {
				case []: throw ocaml.Not_found.instance;
				case {tc:c, pos:pc}::l:
					try {
						var cf = PMap.find(i, c.cl_statics);
						if (core.Meta.has(NoUsing, cf.cf_meta) || !(context.Typecore.can_access(ctx, c, cf, true)) || core.Meta.has(Impl, cf.cf_meta)) {
							throw ocaml.Not_found.instance;
						}
						var monos = List.map(function (_) { return core.Type.mk_mono(); }, cf.cf_params);
						var map = core.Type.apply_params.bind(cf.cf_params, monos);
						var t = map(cf.cf_type);
						switch (core.Type.follow(t)) {
							case TFun({args:{t:(TType({t_path:{a:["haxe", "macro"], b:"ExprOf"}}, [t0])|t0)}::args, ret:r}):
								if (is_dynamic && core.Type.follow(t0) != core.Type.t_dynamic) {
									throw ocaml.Not_found.instance;
								}
								var e = context.typecore.AbstractCast.cast_or_unify_raise(ctx, t0, e, p);
								// early constraints check is possible because e.etype has no monomorphs
								List.iter2(function (m:core.Type.T, pn:{name:String, t:core.Type.T}) {
									var name = pn.name; var t = pn.t;
									switch (core.Type.follow(t)) {
										case TInst({cl_kind:KTypeParameter(constr)}, _) if (constr!=Tl && !core.Type.has_mono(m)):
											List.iter(function (tc) { core.Type.unify(m, map(tc)); }, constr);
										case _:
									}

								}, monos, cf.cf_params);
								var et = type_module_type(ctx, TClassDecl(c), None, p);
								context.display.ImportHandling.maybe_mark_import_position(ctx, pc);
								AKUsing(core.Type.mk(TField(et, FStatic(c, cf)), t, p), c, cf, e);
							case _: throw ocaml.Not_found.instance;
						}
					}
					catch (_:ocaml.Not_found) { loop(l); }
					catch (ue:core.Type.Unify_error) {
						var el = ue.l;
						if (List.exists(function (u:core.Type.UnifyError) { return u.match(Has_extra_field(_)); }, el)) {
							check_constant_struct.set(true);
						}
						loop(l);
					}
					catch (err:core.Error) {
						switch (err.msg) {
							case Unify(el):
								if (List.exists(function (u:core.Type.UnifyError) { return u.match(Has_extra_field(_)); }, el)) {
									check_constant_struct.set(true);
								}
								loop(l);
							case _: throw err;
						}
					}
			}
		}
		return
		try {
			loop(ctx.m.module_using);
		}
		catch (_:ocaml.Not_found) {
			try {
				var acc = loop(ctx.g.global_using);
				switch (acc) {
					case AKUsing(_, c,_,_): core.Type.add_dependency(ctx.m.curmod, c.cl_module);
					case _: trace("Shall not be seen"); throw false;
				}
				acc;
			}
			catch (_:ocaml.Not_found) {
				if (!check_constant_struct.get()) {
					 throw ocaml.Not_found.instance;
				}
				remove_constant_flag(e.etype, function (ok:Bool) {
					return
					if (ok) {
						using_field(ctx, mode, e, i, p);
					}
					else {
						throw ocaml.Not_found.instance;
					}
				});
			}
		}
	}

	public static function type_ident_raise (ctx:context.Typecore.Typer, i:String, p:core.Globals.Pos, mode:AccessMode) : AccessKind {
		return switch (i) {
			case "true":
				(mode == MGet) ? AKExpr(core.Type.mk(TConst(TBool(true)), ctx.t.tbool, p)) : AKNo(i);
			case "false":
				(mode == MGet) ? AKExpr(core.Type.mk(TConst(TBool(false)), ctx.t.tbool, p)) : AKNo(i);
			case "this":
				switch [mode, ctx.curclass.cl_kind] {
					case [MSet, KAbstractImpl(_)]:
						switch (ctx.curfield.cf_kind) {
							case Method(MethInline):
							case Method(_) if (ctx.curfield.cf_name == "_new"):
							case _: core.Error.error("You can only modify 'this' inside an inline function", p);
						}
						AKExpr(get_this(ctx, p));
					case [MCall, KAbstractImpl(_)], [MGet, _]:
						AKExpr(get_this(ctx, p));
					case _: AKNo(i);
				}
			case "super":
				var t:core.Type.T = switch (ctx.curclass.cl_super) {
					case None: core.Error.error("Current class does not have a superclass", p);
					case Some({c:c, params:params}): TInst(c, params);
				}
				switch (ctx.curfun) {
					case FunMember, FunConstructor:
					case FunMemberAbstract: core.Error.error("Cannot access super inside an abstract function", p);
					case FunStatic: core.Error.error("Cannot access super inside a static function", p);
					case FunMemberClassLocal, FunMemberAbstractLocal: core.Error.error("Cannot access super inside a local function", p);
				}
				AKExpr(core.Type.mk(TConst(TSuper), t, p));
			case "null":
				(mode == MGet) ? AKExpr(core.Type.null_(core.Type.mk_mono(), p)) : AKNo(i);
			case _:
				try {
					var v = PMap.find(i, ctx.locals);
					switch (v.v_extra) {
						case Some({params:params, expr:e}):
							var t = core.Type.monomorphs(params, v.v_type);
							switch (e) {
								case Some(e={eexpr:TFunction(f)}) if (ctx.com.display.dms_full_typing):
									switch (mode) {
										case MSet: core.Error.error("Cannot set inline closure", p);
										case MGet: core.Error.error("Cannot create closure on inline closure", p);
										case MCall:
											// create a fake class with a fake field to emulate inlining
											var c = core.Type.mk_class(ctx.m.curmod, new core.Path(["local"], v.v_name), e.epos, core.Globals.null_pos);
											var cf = core.Type.mk_field(v.v_name, v.v_type, e.epos, core.Globals.null_pos);
											cf.cf_params = params; cf.cf_expr = Some(e); cf.cf_kind = Method(MethInline);
											c.cl_extern = true;
											c.cl_fields = PMap.add(cf.cf_name, cf, PMap.empty()); // PMap<String, core.Type.TClassField>
											AKInline(core.Type.mk(TConst(TNull), TInst(c, []), p), cf, FInstance(c, [], cf), t);
									}
								case _:
									AKExpr(core.Type.mk(TLocal(v), t, p));
							}
						case _:
							AKExpr(core.Type.mk(TLocal(v), v.v_type, p));
					}
				}
				catch (_:ocaml.Not_found) {
					try {
						// member variable lookup
						if (ctx.curfun == FunStatic) {
							throw ocaml.Not_found.instance;
						}
						var _tmp = class_field(ctx, ctx.curclass, List.map(function (arg) { return arg.t; }, ctx.curclass.cl_params), i, p);
						var c = _tmp.fst; var t = _tmp.snd; var f = _tmp.trd;
						field_access(ctx, mode, f, switch (c) { case None: FAnon(f); case Some({c:c, params:tl}): FInstance(c, tl, f);}, t, get_this(ctx, p), p);
					}
					catch (_:ocaml.Not_found) {
						try {
							// lookup using on 'this'
							if (ctx.curfun == FunStatic) { throw ocaml.Not_found.instance; }
							switch (using_field(ctx, mode, core.Type.mk(TConst(TThis), ctx.tthis, p), i, p)) {
								case AKUsing(et, c, f, _): AKUsing(et, c, f, get_this(ctx, p));
								case _: trace("Shall not be seen"); throw false;
							}
						}
						catch (_:ocaml.Not_found) {
							try {
								// static variable lookup
								var f = PMap.find(i, ctx.curclass.cl_statics);
								if (core.Meta.has(Impl, f.cf_meta) && !core.Meta.has(Impl, ctx.curfield.cf_meta) && !core.Meta.has(Enum, f.cf_meta)) {
									core.Error.error('Cannot access non-static field ${f.cf_name} from static method', p);
								}
								var e = type_type(ctx, ctx.curclass.cl_path, p);
								// check_locals_masking already done in type_type
								field_access(ctx, mode, f, FStatic(ctx.curclass, f), field_type(ctx, ctx.curclass, [], f, p), e, p);
							}
							catch (_:ocaml.Not_found) {
								try {
									function wrap(e:core.Type.TExpr) : AccessKind {
										return (mode == MSet) ? AKNo(i) : AKExpr(e);
									}
									// lookup imported enums
									function loop (l:ImmutableList<{mt:core.Type.ModuleType, pos:core.Globals.Pos}>) {
										return switch (l) {
											case []: throw ocaml.Not_found.instance;
											case {mt:t, pos:pt}::l:
												switch (t) {
													case TAbstractDecl(a={a_impl:Some(c)}) if (core.Meta.has(Enum, a.a_meta)):
														try {
															var cf = PMap.find(i, c.cl_statics);
															if (!core.Meta.has(Enum, cf.cf_meta)) {
																loop(l);
															}
															else {
																var et = type_module_type(ctx, TClassDecl(c), None, p);
																var fa:core.Type.TFieldAccess = FStatic(c, cf);
																var t = core.Type.monomorphs(cf.cf_params, cf.cf_type);
																context.display.ImportHandling.maybe_mark_import_position(ctx, pt);
																switch (cf.cf_kind) {
																	case Var({v_read:AccInline}): AKInline(et, cf, fa, t);
																	case _: AKExpr(core.Type.mk(TField(et, fa), t, p));
																}
															}
														}
														catch (_:ocaml.Not_found) {
															loop(l);
														}
													case TClassDecl(_), TAbstractDecl(_):
														loop(l);
													case TTypeDecl(t):
														switch (core.Type.follow(t.t_type)) {
															case TEnum(e, _): loop({mt:core.Type.ModuleType.TEnumDecl(e), pos:pt}::l);
															case _: loop(l);
														}
													case TEnumDecl(e):
														try {
															var ef = PMap.find(i,e.e_constrs);
															var et = type_module_type(ctx, t, None, p);
															var monos = List.map(function (_) { return core.Type.mk_mono(); }, e.e_params);
															var monos2 = List.map(function (_) { return core.Type.mk_mono(); }, ef.ef_params);
															context.display.ImportHandling.maybe_mark_import_position(ctx, pt);
															wrap(core.Type.mk(TField(et, FEnum(e, ef)), enum_field_type(ctx, e, ef, monos, monos2, p), p));
														}
														catch (_:ocaml.Not_found) {
															loop(l);
														}
												}
										}
									}
									try {
										loop(List.rev_map(function (t:core.Type.ModuleType) { return {mt:t, pos:core.Globals.null_pos}; }, ctx.m.curmod.m_types));
									}
									catch (_:ocaml.Not_found) {
										loop(ctx.m.module_types);
									}
								}
								catch (_:ocaml.Not_found) {
									// lookup imported globals
									var _tmp = PMap.find(i, ctx.m.module_globals);
									var t = _tmp.a; var name = _tmp.b; var pi = _tmp.pos;
									context.display.ImportHandling.maybe_mark_import_position(ctx, pi);
									var e = type_module_type(ctx, t, None, p);
									type_field(ctx, e, name, p, mode);
								}
							}
						}
					}
				}
		}
	}

	public static function type_field (?resume:Bool=false, ctx:context.Typecore.Typer, e:core.Type.TExpr, i:String, p:core.Globals.Pos, mode:AccessMode) : AccessKind {
		function no_field() : AccessKind {
			if (resume) { throw ocaml.Not_found.instance; }
			var t:core.Type.T = switch (core.Type.follow(e.etype)) {
				case TAnon(a):
					switch (a.a_status.get()) {
						case Statics({cl_kind:KAbstractImpl(a)}): TAbstract(a, []);
						case _: e.etype;
					}
				case TInst({cl_kind:KAbstractImpl(a)}, _): TAbstract(a, []);
				case _: e.etype;
			}
			function has_special_field (a:core.Type.TAbstract) : Bool {
				return List.exists(function (o) { var cf = o.cf; return cf.cf_name == i; }, a.a_ops)
				|| List.exists(function (u) { var cf = u.cf; return cf.cf_name == i; }, a.a_unops)
				|| List.exists(function (cf) { return cf.cf_name == i; }, a.a_array);
			}
			if (!ctx.untyped_) {
				switch (t) {
					case TAbstract(a, _) if (has_special_field(a)):
						// the abstract field is not part of the field list, which is only true when it has no expression (issue #2344)
						context.Typecore.display_error(ctx, "Field " + i + " cannot be called directly because it has no expression", p);
					case _:
						context.Typecore.display_error(ctx, core.type.StringError.string_error(i, core.Error.string_source(t), core.Type.s_type(core.Type.print_context(), t))+" has no field "+i, p);
				}
			}
			return AKExpr(core.Type.mk(TField(e, FDynamic(i)), core.Type.mk_mono(), p));
		}
		function does_forward (a:core.Type.TAbstract, stat:Bool) : Bool {
			return try {
				var el = core.Meta.get((stat) ? ForwardStatics : Forward, a.a_meta).params;
				switch (el) {
					case []: true;
					case _:
						List.exists(function (e:core.Ast.Expr) {
							return switch (e.expr) {
								case EConst((CIdent(s) | CString(s))): s == i;
								case _: core.Error.error("Identifier or string expected as argument to @:forward", e.pos);
							}
						}, el);
				}
			}
			catch (_:ocaml.Not_found) {
				false;
			}
		}
		return switch (core.Type.follow(e.etype)) {
			case TInst(c, params):
				function loop_dyn(c:core.Type.TClass, params:core.Type.TParams) : AccessKind {
					return switch (c.cl_dynamic) {
						case Some(t):
							var t = core.Type.apply_params(c.cl_params, params, t);
							if ((mode == MGet || mode == MCall) && PMap.mem("resolve", c.cl_fields)) {
								var f = PMap.find("resolve", c.cl_fields);
								switch (f.cf_kind) {
									case Method(MethMacro): context.Typecore.display_error(ctx, "The macro accessor is not allowed for field resolve", f.cf_pos);
									case _:
								}
								var texpect = core.Type.tfun([ctx.t.tstring], t);
								var tfield = core.Type.apply_params(c.cl_params, params, core.Type.monomorphs(f.cf_params, f.cf_type));
								try {
									core.Type.unify(tfield, texpect);
								}
								catch (u:core.Type.Unify_error) {
									context.Typecore.display_error(ctx, "Field resolve has an invalid type", f.cf_pos);
									context.Typecore.display_error(ctx, core.Error.error_msg(Unify([Cannot_unify(tfield, texpect)])), f.cf_pos);
								}
								AKExpr(make_call(ctx, core.Type.mk(TField(e, FInstance(c, params, f)), tfield, p), [core.Texpr.type_constant(ctx.com.basic, CString(i), p)], t, p));
							}
							else {
								AKExpr(core.Type.mk(TField(e, FDynamic(i)), t, p));
							}
						case None:
							switch (c.cl_super) {
								case None: throw ocaml.Not_found.instance;
								case Some({c:c, params:params}): loop_dyn(c, params);
							}
					}
				}
				try {
					var _tmp = class_field(ctx, c, params, i, p);
					var c2 = _tmp.fst; var t = _tmp.snd; var f = _tmp.trd;
					if (e.eexpr.equals(TConst(TSuper))) {
						switch [mode, f.cf_kind] {
							case [MGet, Var({v_read:AccCall})], [MSet, Var({v_write:AccCall})], [MCall, Var({v_read:AccCall})]:
							case [MCall, Var(_)]:
								context.Typecore.display_error(ctx, "Cannot access superclass variable for calling: needs to be a proper method", p);
							case [MCall, _]:
							case [MGet, Var(_)], [MSet, Var(_)] if (switch (c2) { case Some({c:{cl_extern:true, cl_path:{a:"flash"::_}}}): true; case _: false; }):
							case [_, Method(_)]:
								context.Typecore.display_error(ctx, "Cannot create closure on super method", p);
							case _:
								context.Typecore.display_error(ctx, "Normal variables cannot be accessed with 'super', use 'this' instead", p);
						}
					}
					if (!context.Typecore.can_access(ctx, c, f, false) && !ctx.untyped_) {
						context.Typecore.display_error(ctx, "Cannot access private field "+i, p);
					}
					field_access(ctx, mode, f, switch (c2) { case None: FAnon(f); case Some({c:c, params:tl}): FInstance(c, tl, f); }, core.Type.apply_params(c.cl_params, params, t), e, p);
				}
				catch (_:ocaml.Not_found) {
					try {
						switch (e.eexpr) {
							case TConst(TSuper): throw ocaml.Not_found.instance;
							case _: using_field(ctx, mode, e, i, p);
						}
					}
					catch (_:ocaml.Not_found) {
						try {
							loop_dyn(c, params);
						}
						catch (_:ocaml.Not_found) {
							try {
								// if we have an abstract constraint we have to check its static fields and recurse (issue #2343)
								switch (c.cl_kind) {
									case KTypeParameter(tl):
										function loop (tl:core.Type.TParams) {
											return switch (tl) {
												case t::tl:
													switch (core.Type.follow(t)) {
														case TAbstract({a_impl:Some(c)}, tl) if (PMap.mem(i, c.cl_statics)):
															var e = core.Type.mk_cast(e, t, p);
															type_field(ctx, e, i, p, mode);
														case _: loop(tl);
													}
												case []: throw ocaml.Not_found.instance;
											}
										}
										loop(tl);
									case _: throw ocaml.Not_found.instance;
								}
							}
							catch (_:ocaml.Not_found) {
								if (PMap.mem(i, c.cl_statics)) {
									core.Error.error("Cannot access static field " + i + " from a class instance", p);
								}
								no_field();
							}
						}
					}
				}
			case TDynamic(_.get()=>t):
				try {
					using_field(ctx, mode, e, i, p);
				}
				catch (_:ocaml.Not_found) {
					AKExpr(core.Type.mk(TField(e, FDynamic(i)), t, p));
				}
			case TAnon(a):
				try {
					var f = PMap.find(i, a.a_fields);
					if (core.Meta.has(Impl, f.cf_meta) && !core.Meta.has(Enum, f.cf_meta)) {
						context.Typecore.display_error(ctx, "Cannot access non-static abstract field statically", p);
					}
					if (!f.cf_public && !ctx.untyped_) {
						switch (a.a_status.get()) {
							case Closed, Extend(_): // always allow anon private fields access
							case Statics(c) if (context.Typecore.can_access(ctx, c, f, true)):
							case _: context.Typecore.display_error(ctx, "Cannot access private field "+i, p);
						}
					}
					var _tmp:{fst:core.Type.TFieldAccess, snd:core.Type.T} = switch (a.a_status.get()) {
						case Statics(c): {fst:FStatic(c, f), snd:field_type(ctx, c, [], f, p)};
						case EnumStatics(e): {fst:FEnum(e, try {PMap.find(f.cf_name, e.e_constrs);} catch (_:ocaml.Not_found) { trace("Shall not be seen"); throw false; }), snd:core.Type.field_type(f)};
						case _:
							switch (f.cf_params) {
								case []: {fst:FAnon(f), snd:core.Type.field_type(f)};
								case l:
									// handle possible constraints
									var monos = List.map(function (_) { return core.Type.mk_mono(); }, l);
									var t = core.Type.apply_params(f.cf_params, monos, f.cf_type);
									add_constraint_check(ctx, [], [], f, monos, p);
									{fst:FAnon(f), snd:t};
							}
					}
					var fmode = _tmp.fst; var ft = _tmp.snd;
					field_access(ctx, mode, f, fmode, ft, e, p);
				}
				catch (_:ocaml.Not_found) {
					try {
						switch (a.a_status.get()) {
							case Statics({cl_kind:KAbstractImpl(a)}) if (does_forward(a, true)):
								var mt = try { core.Type.module_type_of_type(a.a_this); } catch (_:ocaml.Exit) { throw ocaml.Not_found.instance; }
								var et = type_module_type(ctx, mt, None, p);
								type_field(ctx, et, i, p, mode);
							case _: throw ocaml.Not_found.instance;
						}
					}
					catch (_:ocaml.Not_found) {
						if (core.Type.is_closed(a)) {
							try { using_field(ctx, mode, e, i, p); } catch (_:ocaml.Not_found) { no_field(); }
						}
						else {
							var f = core.Type.mk_field(i, core.Type.mk_mono(), p, core.Globals.null_pos);
							f.cf_kind = Var({v_read:AccNormal, v_write:switch (mode) { case MSet:AccNormal; case MGet, MCall: AccNo; }});
							a.a_fields = PMap.add(i, f, a.a_fields);
							field_access(ctx, mode, f, FAnon(f), core.Type.field_type(f), e, p);
						}
					}
				}
			case TMono(r):
				var f = core.Type.mk_field(i, core.Type.mk_mono(), p, core.Globals.null_pos);
				f.cf_kind = Var({v_read:AccNormal, v_write:switch (mode) {case MSet:AccNormal; case MGet, MCall:AccNo;}});
				var x = new Ref<core.Type.AnonStatus>(Opened);
				var t:core.Type.T = TAnon({a_fields:PMap.add(i, f, PMap.empty()), a_status:x}); // <String, core.Type.TClassField>
				ctx.opened = x :: ctx.opened;
				r.set(Some(t));
				field_access(ctx, mode, f, FAnon(f), core.Type.field_type(f), e, p);
			case TAbstract(a, pl):
				var static_abstract_access_through_instance = new Ref(false);
				try {
					var c = switch (a.a_impl) { case None: throw ocaml.Not_found.instance; case Some(c): c;}
					var f = PMap.find(i, c.cl_statics);
					if (!context.Typecore.can_access(ctx, c, f, true) && ! ctx.untyped_) {
						context.Typecore.display_error(ctx, "Cannot access private field " + i, p);
					}
					function field_type (f:core.Type.TClassField) : core.Type.T {
						if (!core.Meta.has(Impl, f.cf_meta)) {
							static_abstract_access_through_instance.set(true);
							throw ocaml.Not_found.instance;
						}
						var t = typing.Typer.field_type(ctx, c, [], f, p);
						return core.Type.apply_params(a.a_params, pl, t);
					}
					var et = type_module_type(ctx, TClassDecl(c), None, p);
					function field_expr (f:core.Type.TClassField, t:core.Type.T) : core.Type.TExpr {
						return core.Type.mk(TField(et, FStatic(c, f)), t, p);
					}
					switch [mode, f.cf_kind] {
						case [(MGet | MCall), Var({v_read:AccCall})]:
							// getter call
							var f = PMap.find("get_"+f.cf_name, c.cl_statics);
							var t = field_type(f);
							var r = switch (core.Type.follow(t)) { case TFun({ret:r}):r; case _: throw ocaml.Not_found.instance; }
							var ef = field_expr(f, t);
							AKExpr(make_call(ctx, ef, [e], r, p));
						case [MSet, Var({v_write:AccCall})]:
							var f = PMap.find("set_"+f.cf_name, c.cl_statics);
							var t = field_type(f);
							var ef = field_expr(f, t);
							AKUsing(ef, c, f, e);
						case [(MGet | MCall), Var({v_read:AccNever})]:
							AKNo(f.cf_name);
						case [(MGet | MCall), _]:
							function loop (cfl:ImmutableList<core.Type.TClassField>) : core.Type.TClassField{
								return switch (cfl) {
									case []: core.Error.error('Field ${f.cf_name} cannot be called on ${core.Type.s_type(core.Type.print_context(), e.etype)}', p);
									case cf::cfl:
										switch (core.Type.follow(core.Type.apply_params(a.a_params, pl, core.Type.monomorphs(cf.cf_params, cf.cf_type)))) {
											case TFun({args:{t:t1}::_}) if (core.Type.type_iseq(t1, core.Abstract.get_underlying_type(a, pl))):
												cf;
											case _:
												loop(cfl);
										}
								}
							}
							var f = switch (f.cf_overloads) {
								case []: f;
								case cfl: loop(f::cfl);
							}
							var t = field_type(f);
							switch (core.Type.follow(t)) {
								case TFun({args:{t:t1}::_}):
								case _: core.Error.error("Invalid call to static function " + i + " through abstract instance", p);
							}
							var ef = field_expr(f, t);
							AKUsing(ef, c, f, e);
						case [MSet, _]:
							core.Error.error("This operation is unsupported", p);
					}
				}
				catch (_:ocaml.Not_found) {
					try {
						if (does_forward(a, false)) {
							type_field(true, ctx, e.with({etype:core.Type.apply_params(a.a_params, pl, a.a_this)}), i, p, mode);
						}
						else {
							throw ocaml.Not_found.instance;
						}
					}
					catch (_:ocaml.Not_found) {
						try {
							using_field(ctx, mode, e, i, p);
						}
						catch (_:ocaml.Not_found) {
							try {
								switch [ctx.curfun, e.eexpr] {
									case [FunMemberAbstract, TConst(TThis)]:
										type_field(ctx, e.with({etype:core.Type.apply_params(a.a_params, pl, a.a_this)}), i, p, mode);
									case _: throw ocaml.Not_found.instance;
								}
							}
							catch (_:ocaml.Not_found) {
								try {
									var _tmp = switch [a.a_impl, a.a_resolve] {
										case [Some(c), Some(cf)]: {fst:c, snd:cf};
										case _: throw ocaml.Not_found.instance;
									}
									var c = _tmp.fst; var cf = _tmp.snd;
									var et = type_module_type(ctx, TClassDecl(c), None, p);
									var t = core.Type.apply_params(a.a_params, pl, field_type(ctx, c, [], cf, p));
									var ef = core.Type.mk(TField(et, FStatic(c, cf)), t, p);
									AKExpr(build_call_ref.get()(ctx, AKUsing(ef, c, cf, e), [{expr:EConst(CString(i)), pos:p}], NoValue, p));
								}
								catch (_:ocaml.Not_found) {
									if (static_abstract_access_through_instance.get()) {
										core.Error.error("Invalid call to static function " + i + " through abstract instance", p);
									}
									else {
										no_field();
									}
								}
							}
						}
					}
				}
			case _:
				try {
					using_field(ctx, mode, e, i, p);
				}
				catch (_:ocaml.Not_found) {
					no_field();
				}
		}
	}

	public static function type_bind (ctx:context.Typecore.Typer, e:core.Type.TExpr, sig:core.Type.TSignature, params, p:core.Globals.Pos) : core.Type.TExpr {
		var args = sig.args; var ret = sig.ret;
		trace("TODO: type_bind");
		throw false;
	}

	public static function unify_int (ctx:context.Typecore.Typer, e:core.Type.TExpr, k:Type_class) : Bool {
		function is_dynamic(t:core.Type.T) : Bool {
			return core.Type.follow(t).match(TDynamic(_));
		}
		function is_dynamic_array(t:core.Type.T) : Bool {
			return switch (core.Type.follow(t)) {
				case TInst(_, [p]): is_dynamic(p);
				case _: true;
			}
		}
		function is_dynamic_field(t:core.Type.T, f:String) : Bool {
			return switch (core.Type.follow(t)) {
				case TAnon(a):
					try {
						is_dynamic(PMap.find(f, a.a_fields).cf_type);
					}
					catch (_:ocaml.Not_found) { false; }
				case TInst(c, tl):
					try {
						is_dynamic(core.Type.apply_params(c.cl_params, tl, core.Type.class_field(c, tl, f).snd));
					}
					catch (_:ocaml.Not_found) { false; }
				case _: true;
			}
		}
		function is_dynamic_return(t:core.Type.T) : Bool {
			return switch(core.Type.follow(t)) {
				case TFun({ret:r}): is_dynamic(r);
				case _: true;
			}
		}

		// This is some quick analysis that matches the most common cases of dynamic-to-mono convertions
		function maybe_dynamic_mono(e:core.Type.TExpr) : Bool {
			function maybe_dynamic_rec(e:core.Type.TExpr, t:core.Type.T) : Bool {
				return switch (core.Type.follow(t)) {
					case TMono(_), TDynamic(_): maybe_dynamic_mono(e);
					// we might have inferenced a tmono into a single field
					case TAnon(a) if (a.a_status.get() == Opened):
						maybe_dynamic_mono(e);
					case _: false;
				}
			}
			return switch (e.eexpr) {
				case TLocal(_): is_dynamic(e.etype);
				case TArray(e={etype:t}, _): is_dynamic_array(t) || maybe_dynamic_rec(e, t);
				case TField(e={etype:t}, f): is_dynamic_field(t, core.Type.field_name(f)) || maybe_dynamic_rec(e, t);
				case TCall(e={etype:t}, _): is_dynamic_return(t) || maybe_dynamic_rec(e, t);
				case TParenthesis(e), TMeta(_, e): maybe_dynamic_mono(e);
				case TIf(_, a, Some(b)): maybe_dynamic_mono(a) || maybe_dynamic_mono(b);
				case _: false;
			}
		}
		return switch(k) {
			case KUnk, KDyn if (maybe_dynamic_mono(e)):
				context.Typecore.unify(ctx, e.etype, ctx.t.tfloat, e.epos);
				false;
			case _:
				context.Typecore.unify(ctx, e.etype, ctx.t.tint, e.epos);
				true;
		}
	}

	public static function type_generic_function (ctx:context.Typecore.Typer, _efa:{fst:core.Type.TExpr, snd:core.Type.TFieldAccess}, el:ImmutableList<core.Ast.Expr>, ?using_param:Option<core.Type.TExpr>=null, with_type:context.Typecore.WithType, p:core.Globals.Pos) : core.Type.TExpr {
		if (using_param == null) { using_param = None; }
		var e = _efa.fst; var fa = _efa.snd;
		trace("TODO: typing.Typer.type_generic_function");
		throw false;
	}

	public static function call_to_string (ctx:context.Typecore.Typer, ?resume:Bool=false, e:core.Type.TExpr) : core.Type.TExpr {
		// Ignore visibility of the toString field.
		ctx.meta = ({name:PrivateAccess, params:[], pos:e.epos} : core.Ast.MetadataEntry) :: ctx.meta;
		var acc = type_field(resume, ctx, e, "toString", e.epos, MCall);
		ctx.meta = List.tl(ctx.meta);
		return build_call_ref.get()(ctx, acc, [], WithType(ctx.t.tstring), e.epos);
	}

	public static function type_binop (ctx:context.Typecore.Typer, op:core.Ast.Binop, e1:core.Ast.Expr, e2:core.Ast.Expr, is_assign_op:Bool, with_type:context.Typecore.WithType, p:core.Globals.Pos) : core.Type.TExpr {
		return switch (op) {
			case OpAssign:
				var e1 = type_access(ctx, e1.expr, e1.pos, MSet);
				var tt:context.Typecore.WithType = switch (e1) {
					case AKNo(_), AKInline(_), AKUsing(_), AKMacro(_), AKAccess(_): Value;
					case AKSet(_, t, _): WithType(t);
					case AKExpr(e): WithType(e.etype);
				}
				var e2 = type_expr(ctx, e2, tt);
				switch (e1) {
					case AKNo(s): core.Error.error("Cannot access field or identifier "+s+" for writing", p);
					case AKExpr(e1):
						var e2 = context.typecore.AbstractCast.cast_or_unify(ctx, e1.etype, e2, p);
						check_assign(ctx, e1);
						switch [e1.eexpr, e2.eexpr] {
							case [TLocal(i1), TLocal(i2)] if (i1.equals(i2)): core.Error.error("Assigning a value to itself", p);
							case [TField({eexpr:TConst(TThis)}, FInstance(_,_,f1)), TField({eexpr:TConst(TThis)}, FInstance(_,_,f2))] if (f1.equals(f2)):
								core.Error.error("Assigning a value to itself", p);
							case [_, _]:
						}
						core.Type.mk(TBinop(op, e1, e2), e1.etype, p);
					case AKSet(e, t, cf):
						var e2 = context.typecore.AbstractCast.cast_or_unify(ctx, t, e2, p);
						make_call(ctx, core.Type.mk(TField(e, core.Type.quick_field_dynamic(e.etype, "set_"+cf.cf_name)), core.Type.tfun([t], t), p), [e2], t, p);
					case AKAccess(a, tl, c, ebase, ekey):
						mk_array_get_call(ctx, context.typecore.AbstractCast.find_array_access(ctx, a, tl, ekey, Some(e2), p), c, ebase, p);
					case AKUsing(ef, _, _, et):
						// this must be an abstract setter
						var _tmp = switch (core.Type.follow(ef.etype)) {
							case TFun({args:[_, {t:t}], ret:ret}):
								{fst:context.typecore.AbstractCast.cast_or_unify(ctx, t, e2, p), snd:ret};
							case _:
								core.Error.error("Invalid field type for abstract setter", p);
						}
						var e2 = _tmp.fst; var ret = _tmp.snd;
						make_call(ctx, ef, [et, e2], ret, p);
					case AKInline(_), AKMacro(_): trace("Shall not be seen"); throw false;
				}
			case OpAssignOp((OpBoolAnd | OpBoolOr)):
				core.Error.error("The operators ||= and &&= are not supported", p);
			case OpAssignOp(op):
				switch (type_access(ctx, e1.expr, e1.pos, MSet)) {
					case AKNo(s): core.Error.error("Cannot access field or identifier "+s+" for writing", p);
					case AKExpr(e):
						var save = context.Typecore.save_locals(ctx);
						var v = context.Typecore.gen_local(ctx, e.etype, e.epos);
						var has_side_effect = optimization.OptimizerTexpr.has_side_effect(e);
						var e1:core.Ast.Expr = (has_side_effect) ? {expr:EConst(CIdent(v.v_name)), pos:e.epos} : e1;
						var eop = type_binop(ctx, op, e1, e2, true, with_type, p);
						save();
						switch (eop.eexpr) {
							case TBinop(_, _, e2):
								context.Typecore.unify(ctx, eop.etype, e.etype, p);
								check_assign(ctx, e);
								core.Type.mk(TBinop(OpAssignOp(op), e, e2), e.etype, p);
							case TMeta({name:RequiresAssign}, e2):
								context.Typecore.unify(ctx, e2.etype, e.etype, p);
								check_assign(ctx, e);
								switch (e.eexpr) {
									case TArray(ea1, ea2) if (has_side_effect):
										var v1 = context.Typecore.gen_local(ctx, ea1.etype, ea1.epos);
										var ev1 = core.Type.mk(TLocal(v1), v1.v_type, p);
										var v2 = context.Typecore.gen_local(ctx, ea2.etype, ea2.epos);
										var ev2 = core.Type.mk(TLocal(v2), v2.v_type, p);
										var e = e.with({eexpr: core.Type.TExprExpr.TArray(ev1, ev2)});
										core.Type.mk(TBlock([
											core.Type.mk(TVar(v1, Some(ea1)), ctx.t.tvoid, p),
											core.Type.mk(TVar(v2, Some(ea2)), ctx.t.tvoid, p),
											core.Type.mk(TVar(v, Some(e)), ctx.t.tvoid, p),
											core.Type.mk(TBinop(OpAssign, e, e2), e.etype, p)
										]), e.etype, p);
									case TField(ea1, fa) if (has_side_effect):
										var v1 = context.Typecore.gen_local(ctx, ea1.etype, ea1.epos);
										var ev1 = core.Type.mk(TLocal(v1), v1.v_type, p);
										var e = e.with({eexpr: core.Type.TExprExpr.TField(ev1, fa)});
										core.Type.mk(TBlock([
											core.Type.mk(TVar(v1, Some(ea1)), ctx.t.tvoid, p),
											core.Type.mk(TVar(v, Some(e)), ctx.t.tvoid, p),
											core.Type.mk(TBinop(OpAssign, e, e2), e.etype, p)
										]), e.etype, p);
									case _:
										core.Type.mk(TBinop(OpAssign, e, e2), e.etype, p);

								}
							case _:
								// this must be an abstract cast
								check_assign(ctx, e);
								if (has_side_effect) {
									core.Type.mk(TBlock([
										core.Type.mk(TVar(v, Some(e)), ctx.t.tvoid, eop.epos),
										eop
									]), eop.etype, eop.epos);
								}
								else {
									eop;
								}
						}
					case AKSet(e, t, cf):
						var l = context.Typecore.save_locals(ctx);
						var v = context.Typecore.gen_local(ctx, e.etype, e.epos);
						var ev = core.Type.mk(TLocal(v), e.etype, p);
						var get = type_binop(ctx, op, {expr:EField({expr:EConst(CIdent(v.v_name)), pos:p}, cf.cf_name), pos:p}, e2, true, with_type, p);
						var e_ = switch (get.eexpr) {
							case TBinop(_), TMeta({name:RequiresAssign}, _):
								context.Typecore.unify(ctx, get.etype, t, p);
								make_call(ctx, core.Type.mk(TField(ev, core.Type.quick_field_dynamic(ev.etype, "set_"+cf.cf_name)), core.Type.tfun([t], t), p), [get], t, p);
							case _:
								// abstract setter
								get;
						}
						l();
						core.Type.mk(TBlock([
							core.Type.mk(TVar(v, Some(e)), ctx.t.tvoid, p),
							e_
						]), t, p);
					case AKUsing(ef, c, cf, et):
						// abstract setter + getter
						var ta:core.Type.T = switch (c.cl_kind) {
							case KAbstractImpl(a): TAbstract(a, List.map(function (_) { return core.Type.mk_mono(); }, a.a_params));
							case _: trace("Shall not be seen"); throw false;
						}
						var ret = switch (core.Type.follow(ef.etype)) {
							case TFun({args:[_, _], ret:ret}): ret;
							case _: core.Error.error("Invalid field type for abstract setter", p);
						}
						var l = context.Typecore.save_locals(ctx);
						var _tmp = switch (et.eexpr) {
							case TLocal(v) if (v.v_name != "this"):
								{fst:v, snd:false};
							case _:
								{fst:context.Typecore.gen_local(ctx, ta, ef.epos), snd:true};
						}
						var v = _tmp.fst; var is_temp = _tmp.snd;
						var ev = core.Type.mk(TLocal(v), ta, p);
						// this relies on the fact that cf_name is set_name
						var getter_name = cf.cf_name.substr(4);
						var get = type_binop(ctx, op, {expr:EField({expr:EConst(CIdent(v.v_name)), pos:p}, getter_name), pos:p}, e2, true, with_type, p);
						context.Typecore.unify(ctx, get.etype, ret, p);
						l();
						var e_call = make_call(ctx, ef, [ev, get], ret, p);
						if (is_temp) {
							core.Type.mk(TBlock([
								core.Type.mk(TVar(v, Some(et)), ctx.t.tvoid, p),
								e_call
							]), ret, p);
						}
						else {
							e_call;
						}
					case AKAccess(a, tl, c, ebase, ekey):
						var _tmp = context.typecore.AbstractCast.find_array_access(ctx, a, tl, ekey, None, p);
						var cf_get = _tmp.cf; var tf_get = _tmp.tf; var r_get = _tmp.r; var ekey = _tmp.e1;
						// bind complex keys to a variable so they do not make it into the output twice
						var save = context.Typecore.save_locals(ctx);
						function maybe_bind_to_temp(e:core.Type.TExpr) : {fst:core.Type.TExpr, snd:Option<core.Type.TExpr>} {
							return switch (optimization.Optimizer.make_constant_expression(ctx, e)) {
								case Some(e): {fst:e, snd:None};
								case None:
									var v = context.Typecore.gen_local(ctx, e.etype, p);
									var e_ = core.Type.mk(TLocal(v), e.etype, p);
									{fst:e_, snd:Some(core.Type.mk(TVar(v, Some(e)), ctx.t.tvoid, p))};
							}
						}
						var _tmp = maybe_bind_to_temp(ekey);
						var ekey = _tmp.fst; var ekey_ = _tmp.snd;
						var _tmp2 = maybe_bind_to_temp(ebase);
						var ebase = _tmp2.fst; var ebase_ = _tmp2.snd;
						var eget = mk_array_get_call(ctx, {cf:cf_get, tf:tf_get, r:r_get, e1:ekey, e2o:None}, c, ebase, p);
						var eget = type_binop2(ctx, op, eget, e2, true, WithType(eget.etype), p);
						context.Typecore.unify(ctx, eget.etype, r_get, p);
						var _tmp3 = context.typecore.AbstractCast.find_array_access(ctx, a, tl, ekey, Some(eget), p);
						var cf_set = _tmp3.cf; var tf_set = _tmp3.tf; var r_set = _tmp3.r; var ekey = _tmp3.e1; var eget = _tmp3.e2o;
						var eget = switch (eget) { case None: trace("Shall not be seen"); throw false; case Some(e): e; }
						var et = type_module_type(ctx, TClassDecl(c), None, p);
						var e = switch [cf_set.cf_expr, cf_get.cf_expr] {
							case [None, None]:
								var ea = core.Type.mk(TArray(ebase, ekey), r_get, p);
								core.Type.mk(TBinop(OpAssignOp(op), ea, type_expr(ctx, e2, WithType(r_get))), r_set, p);
							case [Some(_), Some(_)]:
								var ef_set = core.Type.mk(TField(et, FStatic(c, cf_set)), tf_set, p);
								var el = [make_call(ctx, ef_set, [ebase, ekey, eget], r_set, p)];
								el = switch(ebase_) { case None: el; case Some(ebase): ebase::el; };
								el = switch (ekey_) { case None: el; case Some(ekey): ekey::el; };
								switch (el) {
									case [e]: e;
									case el: core.Type.mk(TBlock(el), r_set, p);
								}
							case _:
								core.Error.error("Invalid array access getter/setter combination", p);
						}
						save();
						e;
					case AKInline(_), AKMacro(_): trace("Shall not be seen"); throw false;
				}
			case _:
				/* If the with_type is an abstract which has exactly one applicable @:op method, we can promote it
					to the individual arguments (issue #2786). */
				var wt:context.Typecore.WithType = switch (with_type) {
					case WithType(t):
						switch (core.Type.follow(t)) {
							case TAbstract(a, _):
								switch (List.filter(function (ao:{op:core.Ast.Binop, cf:core.Type.TClassField}) { var o = ao.op; return o.equals(OpAssignOp(op)) || o.equals(op); }, a.a_ops)) {
									case [_]: with_type;
									case _: Value;
								}
							case _: Value;
						}
					case _: Value;
				}
				var e1 = type_expr(ctx, e1, wt);
				type_binop2(ctx, op, e1, e2, is_assign_op, wt, p);
		}
	}
	public static function type_binop2 (ctx:context.Typecore.Typer, op:core.Ast.Binop, e1:core.Type.TExpr, e2:core.Ast.Expr, is_assign_op:Bool, wt:context.Typecore.WithType, p:core.Globals.Pos) : core.Type.TExpr {
		var e2 = type_expr(ctx, e2, (op.match(OpEq|OpNotEq)) ? WithType(e1.etype) : wt);
		var tint = ctx.t.tint;
		var tfloat = ctx.t.tfloat;
		var tstring = ctx.t.tstring;
		function to_string (e:core.Type.TExpr): core.Type.TExpr {
			function loop(t:core.Type.T) : core.Type.TExpr {
				return switch(classify(t)) {
					case KAbstract({a_impl:Some(c)}, _) if (PMap.mem("toString", c.cl_statics)):
						call_to_string(ctx, e);
					case KInt, KFloat, KString: e;
					case KUnk, KDyn, KParam(_), KOther:
						var std = type_type(ctx, new core.Path([], "std"), e.epos);
						var acc = acc_get(ctx, type_field(ctx, std, "string", e.epos, MCall), e.epos);
						core.Type.follow(acc.etype);
						var acc:core.Type.TExpr = switch (acc.eexpr) {
							case TField(e, FClosure(Some({c:c, params:tl}), f)):
								acc.with({eexpr: core.Type.TExprExpr.TField(e, FInstance(c, tl, f))});
							case _: acc;
						}
						make_call(ctx, acc, [e], ctx.t.tstring, e.epos);
					case KAbstract(a, tl):
						try {
							context.typecore.AbstractCast.cast_or_unify(ctx, tstring, e, p);
						}
						catch (err:core.Error) {
							switch (err.msg) {
								case Unify(_):
									loop(core.Abstract.get_underlying_type(a, tl));
								case _: throw err;
							}
						}
				}
			}
			return loop(e.etype);
		}
		function mk_op(e1:core.Type.TExpr, e2:core.Type.TExpr, t:core.Type.T) : core.Type.TExpr {
			return
			if (op.match(OpAdd) && classify(t)==KString) {
				var e1 = to_string(e1);
				var e2 = to_string(e2);
				core.Type.mk(TBinop(op, e1, e2), t, p);
			}
			else {
				core.Type.mk(TBinop(op, e1, e2), t, p);
			}
		}
		function make (e1:core.Type.TExpr, e2:core.Type.TExpr) : core.Type.TExpr {
			return switch (op) {
				case OpAdd:
					mk_op(e1, e2, switch [classify(e1.etype), classify(e2.etype)] {
						case [KInt, KInt]: tint;
						case [KFloat, KInt], [KInt, KFloat], [KFloat, KFloat]: tfloat;
						case [KUnk, KInt]: (unify_int(ctx, e1, KUnk)) ? tint : tfloat;
						case [KUnk, KFloat], [KUnk, KString]:
							context.Typecore.unify(ctx, e1.etype, e2.etype, e1.epos);
							e1.etype;
						case [KInt, KUnk]:
							(unify_int(ctx, e2, KUnk)) ? tint : tfloat;
						case [KFloat, KUnk], [KString, KUnk]:
							context.Typecore.unify(ctx, e2.etype, e1.etype, e2.epos);
							e2.etype;
						case [_, KString], [KString, _]: tstring;
						case [_, KDyn]: e2.etype;
						case [KDyn, _]: e1.etype;
						case [KUnk, KUnk]:
							var ok1 = unify_int(ctx, e1, KUnk);
							var ok2 = unify_int(ctx, e2, KUnk);
							(ok1 && ok2) ? tint : tfloat;
						case [KParam(t1), KParam(t2)] if (core.Type.type_iseq(t1, t2)):
							t1;
						case [KParam(t), KInt], [KInt, KParam(t)]:
							t;
						case [KParam(_), KFloat], [KFloat, KParam(_)], [KParam(_), KParam(_)]:
							tfloat;
						case [KParam(t), KUnk]:
							context.Typecore.unify(ctx, e2.etype, tfloat, e2.epos);
							tfloat;
						case [KUnk, KParam(t)]:
							context.Typecore.unify(ctx, e1.etype, tfloat, e1.epos);
							tfloat;
						case [KAbstract(_), KFloat]:
							context.Typecore.unify(ctx, e1.etype, tfloat, e1.epos);
							tfloat;
						case [KFloat, KAbstract(_)]:
							context.Typecore.unify(ctx, e2.etype, tfloat, e2.epos);
							tfloat;
						case [KAbstract(_), KInt]:
							context.Typecore.unify(ctx, e1.etype, tint, e1.epos);
							tint;
						case [KInt, KAbstract(_)]:
							context.Typecore.unify(ctx, e2.etype, tint, e2.epos);
							tint;
						case [KAbstract(_), _], [_, KAbstract(_)], [KParam(_), _], [_, KParam(_)], [KOther, _], [_, KOther]:
							var pr = core.Type.print_context();
							core.Error.error("Cannot add "+core.Type.s_type(pr, e1.etype)+ " and "+ core.Type.s_type(pr, e2.etype), p);
					});
				case OpAnd, OpOr, OpXor, OpShl, OpShr, OpUShr:
					var i = tint;
					context.Typecore.unify(ctx, e1.etype, i, e1.epos);
					context.Typecore.unify(ctx, e2.etype, i, e2.epos);
					mk_op(e1, e2, i);
				case OpMod, OpMult, OpDiv, OpSub:
					var result = new Ref((op == OpDiv) ? tfloat : tint);
					switch [classify(e1.etype), classify(e2.etype)] {
						case [KFloat, KFloat]: result.set(tfloat);
						case [KParam(t1), KParam(t2)] if (core.Type.type_iseq(t1, t2)):
							if (op != OpDiv) {
								result.set(t1);
							}
						case [KParam(_), KParam(_)]:
							result.set(tfloat);
						case [KParam(t), KInt], [KInt, KParam(t)]:
							if (op != OpDiv) {
								result.set(t);
							}
						case [KParam(t), KFloat], [KFloat, KParam(t)]:
							result.set(tfloat);
						case [KFloat, k]:
							unify_int(ctx, e2, k);
							result.set(tfloat);
						case [k, KFloat]:
							unify_int(ctx, e1, k);
							result.set(tfloat);
						case [k1, k2]:
							var ok1 = unify_int(ctx, e1, k1);
							var ok2 = unify_int(ctx, e2, k2);
							if (!ok1 || !ok2) {
								result.set(tfloat);
							}
					}
					mk_op(e1, e2, result.get());
				case OpEq, OpNotEq:
					var _tmp = try {
						// we only have to check one type here, because unification fails if one is Void and the other is not
						switch (core.Type.follow(e2.etype)) {
							case TAbstract({a_path:{a:[], b:"Void"}}, _):
								core.Error.error("Cannot compare Void", p);
							case _:
						}
						{fst:context.typecore.AbstractCast.cast_or_unify_raise(ctx, e2.etype, e1, p), snd:e2};
					}
					catch (err:core.Error) {
						switch (err.msg) {
							case Unify(_):
								{fst:e1, snd:context.typecore.AbstractCast.cast_or_unify(ctx, e1.etype, e2, p)};
							case _: throw err;
						}
					}
					var e1 = _tmp.fst; var e2 = _tmp.snd;
					mk_op(e1, e2, ctx.t.tbool);
				case OpGt, OpGte, OpLt, OpLte:
					switch [classify(e1.etype), classify(e2.etype)] {
						case [KInt, KInt], [KInt, KFloat], [KFloat, KInt], [KFloat, KFloat], [KString, KString]:
						case [KInt, KUnk]: unify_int(ctx, e2, KUnk);
						case [KFloat, KUnk], [KString, KUnk]: context.Typecore.unify(ctx, e2.etype, e1.etype, e2.epos);
						case [KUnk, KInt]: unify_int(ctx, e1, KUnk);
						case [KUnk, KFloat], [KUnk, KString]: context.Typecore.unify(ctx, e1.etype, e2.etype, e1.epos);
						case [KUnk, KUnk]:
							unify_int(ctx, e1, KUnk);
							unify_int(ctx, e2, KUnk);
						case [KDyn, KInt], [KDyn, KFloat], [KDyn, KString]:
						case [KInt, KDyn], [KFloat, KDyn], [KString, KDyn]:
						case [KDyn, KDyn]:
						case [KParam(_), x] if (x != KString && x != KOther):
						case [x, KParam(_)] if (x != KString && x != KOther):
						case [KAbstract(_), _], [_, KAbstract(_)],
							 [KDyn, KUnk], [KUnk, KDyn], [KString, KInt],
							 [KString, KFloat], [KInt, KString],
							 [KFloat, KString], [KParam(_), _],
							 [_, KParam(_)], [KOther, _],
							 [_, KOther]:
							var pr = core.Type.print_context();
							core.Error.error("Cannot compare "+core.Type.s_type(pr, e1.etype) + " and " + core.Type.s_type(pr, e2.etype), p);
					}
					mk_op(e1, e2, ctx.t.tbool);
				case OpBoolAnd, OpBoolOr:
					var b = ctx.t.tbool;
					context.Typecore.unify(ctx, e1.etype, b, p);
					context.Typecore.unify(ctx, e2.etype, b, p);
					mk_op(e1, e2, b);
				case OpInterval:
					var t = typing.Typeload.load_core_type(ctx, "IntIterator");
					context.Typecore.unify(ctx, e1.etype, tint, e1.epos);
					context.Typecore.unify(ctx, e2.etype, tint, e2.epos);
					core.Type.mk(TNew(switch (t) {case TInst(c,[]): c; case _: trace("Shall not be seen"); throw false;}, [], [e1, e2]), t, p);
				case OpArrow: core.Error.error("Unexpected =>", p);
				case OpIn: core.Error.error("Unexpected in", p);
				case OpAssign, OpAssignOp(_): trace("Shall not be seen"); throw false;
			}
		}
		function find_overload(a:core.Type.TAbstract, c, tl:core.Type.TParams, left) {
			var map = core.Type.apply_params.bind(a.a_params, tl);
			function make_(op_cf, cf:core.Type.TClassField, e1:core.Type.TExpr, e2:core.Type.TExpr, tret:core.Type.T) {
				return if (cf.cf_expr == None) {
					if (!core.Meta.has(NoExpr, cf.cf_meta)) {
						context.Typecore.display_error(ctx, "Recursive operator method", p);
					}
					if (!core.Meta.has(CoreType, a.a_meta)) {
						// for non core-types we require that the return type is compatible to the native result type
						var _e1 = e1.with({etype:core.Abstract.follow_with_abstracts(e1.etype)});
						var _e2 = e2.with({etype: core.Abstract.follow_with_abstracts(e2.etype)});
						var e_ = make(_e1, _e2);
						var t_expected = e_.etype;
						try {
							context.Typecore.unify_raise(ctx, tret, t_expected, p);
						}
						catch (err:core.Error) {
							switch (err.msg) {
								case Unify(_):
									switch (core.Type.follow(tret)) {
										case TAbstract(a, tl) if (core.Type.type_iseq(core.Abstract.get_underlying_type(a, tl), t_expected)):
										case _:
											var st = core.Type.s_type.bind(core.Type.print_context());
											core.Error.error('The result of this operation (${st(t_expected)}) is not compatible with declared return type ${st(tret)}', p);
									}
								case _: throw err;
							}
						}
					}
					var e = core.Texpr.Builder.binop(op, e1, e2, tret, p);
					core.Type.mk_cast(e, tret, p);
				}
				else {
					var e = context.Typecore.make_static_call(ctx, c, cf, map, [e1, e2], tret, p);
					e;
				}
			}
			/* special case for == and !=: if the second type is a monomorph, assume that we want to unify
				it with the first type to preserve comparison semantics. */
			var is_eq_op = op.match(OpEq | OpNotEq);
			if (is_eq_op) {
				switch [core.Type.follow(e1.etype), core.Type.follow(e2.etype)] {
					case [TMono(_), _], [_, TMono(_)]:
						core.Type.unify(e1.etype, e2.etype);
					case _:
				}
			}
			function loop (ol:ImmutableList<{op:core.Ast.Binop, cf:core.Type.TClassField}>) {
				return switch (ol) {
					case {op:op_cf, cf:cf}::ol if (!op_cf.equals(op) && (!is_assign_op || !op_cf.equals(OpAssignOp(op))) ):
						loop(ol);
					case {op:op_cf, cf:cf}::ol:
						var is_impl = core.Meta.has(Impl, cf.cf_meta);
						switch (core.Type.follow(cf.cf_type)) {
							case TFun({args:[{t:t1}, {t:t2}], ret:tret}):
								function check(e1, e2, swapped) {
									function map_arguments() {
										var monos = List.map(function (_) { return core.Type.mk_mono(); }, cf.cf_params);
										function map_(t) {
											return map(core.Type.apply_params(cf.cf_params, monos, t));
										}
										var t1 = map_(t1);
										var t2 = map_(t2);
										var tret = map_(tret);
										return {fst:monos, snd:t1, trd:t2, frth:tret};
									}
									var _tmp = map_arguments();
									var monos = _tmp.fst; var t1 = _tmp.snd; var t2 = _tmp.trd; var tret = _tmp.frth;
									function make__ (e1, e2) {
										return make_(op_cf, cf, e1, e2, tret);
									}
									var t1 = (is_impl) ? core.Abstract.follow_with_abstracts(t1) : t1;
									var _tmp = if (left || (!left && swapped)) {
										core.Type.type_eq(EqStrict, (is_impl) ? core.Abstract.follow_with_abstracts(e1.etype) : e1.etype, t1);
										{fst:e1, snd:context.typecore.AbstractCast.cast_or_unify_raise(ctx, t2, e2, p)};
									}
									else {
										core.Type.type_eq(EqStrict, e2.etype, t2);
										{fst:context.typecore.AbstractCast.cast_or_unify_raise(ctx, t1, e1, p), snd:e2};
									}
									var e1 = _tmp.fst; var e2 = _tmp.snd;
									check_constraints(ctx, "", cf.cf_params, monos, core.Type.apply_params.bind(a.a_params, tl), false, cf.cf_pos);
									function check_null (e:core.Type.TExpr, t:core.Type.T) {
										if (is_eq_op) {
											switch (e.eexpr) {
												case TConst(TNull) if (!core.Type.is_explicit_null(t)):
													throw new core.Type.Unify_error([]);
												case _:
											}
										}
									}
									/* If either expression is `null` we only allow operator resolving if the argument type
										is explicitly Null<T> (issue #3376) */
									if (is_eq_op) {
										check_null(e2, t2);
										check_null(e1, t2);
									}
									var e = if (!swapped) {
										make__(e1, e2);
									}
									else if (!optimization.OptimizerTexpr.has_side_effect(e1) && !optimization.OptimizerTexpr.has_side_effect(e2)) {
										make__(e1, e2);
									}
									else {
										var v1 = context.Typecore.gen_local(ctx, t1, e1.epos); var v2 = context.Typecore.gen_local(ctx, t2, e2.epos);
										var ev1 = core.Type.mk(TVar(v1, Some(e1)), ctx.t.tvoid, p);
										var ev2 = core.Type.mk(TVar(v2, Some(e2)), ctx.t.tvoid, p);
										var eloc1 = core.Type.mk(TLocal(v1), v1.v_type, p);
										var eloc2 = core.Type.mk(TLocal(v2), v2.v_type, p);
										var e = make__(eloc1, eloc2);
										var e = core.Type.mk(TBlock([
											ev2,
											ev1,
											e
										]), e.etype, e.epos);
										e;
									}
									if (is_assign_op && op_cf.equals(op)) {
										return core.Type.mk(TMeta({name:RequiresAssign, params:[], pos:p}, e), e.etype, e.epos);
									}
									else {
										return e;
									}
								}
								function __catch () {
									return
									try {
										if (!core.Meta.has(Commutative, cf.cf_meta)) {
											throw ocaml.Not_found.instance;
										}
										check(e1, e2, false);
									}
									catch (_:ocaml.Not_found) { loop(ol); }
									catch (err:core.Error) {
										switch (err.msg) {
											case Unify(_): loop(ol);
											case _: throw err;
										}
									}
									catch (_:core.Type.Unify_error) {
										loop(ol);
									}
								}

								try {
									check(e1, e2, false);
								}
								catch (err:core.Error) {
									switch (err.msg) {
										case Unify(_):
											__catch();
										case _: throw err;
									}
								}
								catch (_:core.Type.Unify_error) {
									__catch();
								}
							case _: trace("Shall not be seen"); throw false;
						}
					case []: throw ocaml.Not_found.instance;
				}
			}
			return loop ((left) ? a.a_ops : List.filter(function (ops:{op:core.Ast.Binop, cf:core.Type.TClassField}) { var cf = ops.cf; return !core.Meta.has(Impl, cf.cf_meta); }, a.a_ops));
		}
		return
		try {
			switch (core.Type.follow(e1.etype)) {
				case TAbstract(a={a_impl:Some(c)}, tl): find_overload(a, c, tl, true);
				case _: throw ocaml.Not_found.instance;
			}
		}
		catch (_:ocaml.Not_found) {
			try {
				switch (core.Type.follow(e2.etype)) {
					case TAbstract(a={a_impl:Some(c)}, tl): find_overload(a, c, tl, false);
					case _: throw ocaml.Not_found.instance;
				}
			}
			catch (_:ocaml.Not_found) {
				make(e1, e2);
			}
		}
	}

	public static function type_unop (ctx:context.Typecore.Typer, op:core.Ast.Unop, flag:core.Ast.UnopFlag, e:core.Ast.Expr, p:core.Globals.Pos) : core.Type.TExpr {
		var set = op.match(OpIncrement | OpDecrement); // (op == OpIncrement || op == OpDecrement);
		var acc = type_access(ctx, e.expr, e.pos, (set) ? MSet : MGet);
		function access (e:core.Type.TExpr) : core.Type.TExpr {
			function make (e:core.Type.TExpr) : core.Type.TExpr {
				var t = switch (op) {
					case OpNot:
						if (flag == Postfix) {
							core.Error.error("Postfix ! is not supported", p);
						}
						context.Typecore.unify(ctx, e.etype, ctx.t.tbool, e.epos);
						ctx.t.tbool;
					case OpNegBits:
						context.Typecore.unify(ctx, e.etype, ctx.t.tint, e.epos);
						ctx.t.tint;
					case OpIncrement, OpDecrement, OpNeg:
						if (set) { check_assign(ctx, e); }
						switch (classify(e.etype)) {
							case KFloat: ctx.t.tfloat;
							case KParam(t):
								context.Typecore.unify(ctx, e.etype, ctx.t.tfloat, e.epos);
								t;
							case k:
								(unify_int(ctx, e, k)) ? ctx.t.tint : ctx.t.tfloat;
						}
				}
				return core.Type.mk(TUnop(op, flag, e), t, p);
			}
			return
			try {
				switch (core.Type.follow(e.etype)) {
					case TAbstract(a={a_impl:Some(c)}, pl):
						function loop (opl:ImmutableList<{op:core.Ast.Unop, flag:core.Ast.UnopFlag, cf:core.Type.TClassField}>) : {fst:core.Type.TClassField, snd:core.Type.T, trd:core.Type.T} {
							return switch (opl) {
								case []: throw ocaml.Not_found.instance;
								case {op:op2, flag:flag2, cf:cf}::opl if (op.equals(op2) && flag.equals(flag2)):
									var m = core.Type.mk_mono();
									var tcf = core.Type.apply_params(a.a_params, pl, core.Type.monomorphs(cf.cf_params, cf.cf_type));
									if (core.Meta.has(Impl, cf.cf_meta)) {
										(core.Type.type_iseq(core.Type.tfun([core.Type.apply_params(a.a_params, pl, a.a_this)], m), tcf)) ? {fst:cf, snd:tcf, trd:m} : loop(opl);
									}
									else {
										(core.Type.type_iseq(core.Type.tfun([e.etype], m), tcf)) ? {fst:cf, snd:tcf, trd:m} : loop(opl);
									}
								case _::opl: loop(opl);
							}
						}
						var _tmp = try { loop(a.a_unops); } catch (_:ocaml.Not_found) { throw ocaml.Not_found.instance; }
						var cf = _tmp.fst; var t = _tmp.snd; var r = _tmp.trd;
						switch (cf.cf_expr) {
							case None:
								var e = e.with({etype: core.Type.apply_params(a.a_params, pl, a.a_this)});
								var e = core.Type.mk(TUnop(op, flag, e), r, p);
								// unify ctx r e.etype p; *) (* TODO: I'm not sure why this was here (related to #2295)
								e;
							case Some(_):
								var et = type_module_type(ctx, TClassDecl(c), None, p);
								var ef = core.Type.mk(TField(et, FStatic(c, cf)), t, p);
								make_call(ctx, ef, [e], r, p);
						}
					case _: throw ocaml.Not_found.instance;
				}
			}
			catch (_:ocaml.Not_found) {
				make(e);
			}
		}
		function loop(acc:AccessKind) {
			return switch (acc) {
				case AKExpr(e): access(e);
				case AKInline(_), AKUsing(_) if (!set):
					access(acc_get(ctx, acc, p));
				case AKNo(s):
					core.Error.error("The field or identifier " + s + " is not accessible for " + ((set) ? "writing" : "reading"), p);
				case AKAccess(a, tl, c, ebase, ekey):
					try {
						switch (op) { case OpIncrement, OpDecrement: case _: throw ocaml.Not_found.instance; }
						var v_key = core.Type.alloc_var("tmp", ekey.etype, ekey.epos);
						var evar_key = core.Type.mk(TVar(v_key, Some(ekey)), ctx.com.basic.tvoid, ekey.epos);
						var ekey = core.Type.mk(TLocal(v_key), ekey.etype, ekey.epos);
						// get
						var e_get = mk_array_get_call(ctx, context.typecore.AbstractCast.find_array_access_raise(ctx, a, tl, ekey, None, p), c, ebase, p);
						var v_get = core.Type.alloc_var("tmp", e_get.etype, e_get.epos);
						var ev_get = core.Type.mk(TLocal(v_get), v_get.v_type, p);
						var evar_get = core.Type.mk(TVar(v_get, Some(e_get)), ctx.com.basic.tvoid, p);
						// op
						var e_one = core.Type.mk(TConst(TInt(1)), ctx.com.basic.tint, p);
						var e_op = core.Type.mk(TBinop((op==OpIncrement) ? OpAdd : OpSub, ev_get, e_one), ev_get.etype, p);
						// set
						var e_set = mk_array_set_call(ctx, context.typecore.AbstractCast.find_array_access_raise(ctx, a, tl, ekey, Some(e_op), p), c, ebase, p);
						var el = evar_key :: evar_get :: e_set :: ((flag == Postfix) ? [ev_get] : []);
						core.Type.mk(TBlock(el), e_set.etype, p);
					}
					catch (_:ocaml.Not_found) {
						var e = mk_array_get_call(ctx, context.typecore.AbstractCast.find_array_access(ctx, a, tl, ekey, None, p), c, ebase, p);
						loop(AKExpr(e));
					}
				case AKInline(_), AKUsing(_), AKMacro(_):
					core.Error.error("This kind of operation is not supported", p);
				case AKSet(e, t, cf):
					var l = context.Typecore.save_locals(ctx);
					var v = context.Typecore.gen_local(ctx, e.etype, p);
					var ev = core.Type.mk(TLocal(v), e.etype, p);
					var op:core.Ast.Binop = switch (op) {
						case OpIncrement: OpAdd;
						case OpDecrement: OpSub;
						case _: trace("Shall not be seen"); throw false;
					}
					var one:core.Ast.Expr = {expr:EConst(CInt("1")), pos:p};
					var eget:core.Ast.Expr = {expr:EField({expr:EConst(CIdent(v.v_name)), pos:p}, cf.cf_name), pos:p};
					switch (flag) {
						case Prefix:
							var get = type_binop(ctx, op, eget, one, false, Value, p);
							context.Typecore.unify(ctx, get.etype, t, p);
							l();
							core.Type.mk(TBlock([
								core.Type.mk(TVar(v, Some(e)), ctx.t.tvoid, p),
								make_call(ctx, core.Type.mk(TField(ev, core.Type.quick_field_dynamic(ev.etype, "set_"+cf.cf_name)), core.Type.tfun([t], t), p), [get], t, p)
							]), t, p);
						case Postfix:
							var v2 = context.Typecore.gen_local(ctx, t, p);
							var ev2 = core.Type.mk(TLocal(v2), t, p);
							var get = type_expr(ctx, eget, Value);
							var plusone = type_binop(ctx, op, {expr:EConst(CIdent(v2.v_name)), pos:p}, one, false, Value, p);
							context.Typecore.unify(ctx, get.etype, t, p);
							l();
							core.Type.mk(TBlock([
								core.Type.mk(TVar(v, Some(e)), ctx.t.tvoid, p),
								core.Type.mk(TVar(v2, Some(get)), ctx.t.tvoid, p),
								make_call(ctx, core.Type.mk(TField(ev, core.Type.quick_field_dynamic(ev.etype, "set_"+cf.cf_name)), core.Type.tfun([plusone.etype], t), p), [plusone], t, p),
								ev2
							]), t, p);
					}
			}
		}
		return  loop(acc);
	}

	public static function type_ident(ctx:context.Typecore.Typer, i:String, p:core.Globals.Pos, mode:AccessMode) : AccessKind {
		return try {
			type_ident_raise(ctx, i, p, mode);
		}
		catch (_:ocaml.Not_found) {
			try {
				// lookup type
				if (core.Ast.is_lower_ident(i)) {
					throw ocaml.Not_found.instance;
				}
				var e = try {
					type_type(ctx, new core.Path([], i), p);
				}
				catch (exc:core.Error) {
					switch (exc.msg) {
						case Module_not_found({a:[], b:name}) if (name == i): throw ocaml.Not_found.instance;
						case _: throw exc;
					}
				}
				AKExpr(e);
			}
			catch (_:ocaml.Not_found) {
				if (ctx.untyped_) {
					if (i == "__this__") {
						AKExpr(core.Type.mk(TConst(TThis), ctx.tthis, p));
					}
					else {
						var t = core.Type.mk_mono();
						AKExpr(core.Type.mk(TIdent(i), t, p));
					}
				}
				else {
					if (ctx.curfun == FunStatic && PMap.mem(i, ctx.curclass.cl_fields)) {
						core.Error.error("Cannot access "+i+" in static function", p);
					}
					try {
						var t = List.find(function (tp:{name:String, t:core.Type.T}) {
							var i2 = tp.name; return i2 == i;
						}, ctx.type_params);
						var c = switch (core.Type.follow(t.t)) { case TInst(c, _): c; case _: trace("Shall not be seen"); throw false;};
						if (typing.Typeload.is_generic_parameter(ctx, c) && core.Meta.has(Const, c.cl_meta)) {
							AKExpr(type_module_type(ctx, TClassDecl(c), None, p));
						}
						else {
							context.Typecore.display_error(ctx, "Type parameter "+i+" is only available at compilation and is not a runtime value", p);
							AKExpr(core.Type.mk(TConst(TNull), core.Type.t_dynamic, p));
						}
					}
					catch (_:ocaml.Not_found) {
						var err:core.Error.ErrorMsg = Unknown_ident(i);
						if (ctx.in_display) {
							throw new core.Error(err, p);
						}
						switch (ctx.com.display.dms_kind) {
							case DMNone: throw new core.Error(err, p);
							case DMDiagnostics(b) if (b || ctx.is_display_file):
								context.DisplayToplevel.handle_unresolved_identifier(ctx, i, p, false);
								var t = core.Type.mk_mono();
								AKExpr(core.Type.mk(TIdent(i), t, p));
							case _:
								context.Typecore.display_error(ctx, core.Error.error_msg(err), p);
								var t = core.Type.mk_mono();
								AKExpr(core.Type.mk(TIdent(i), t, p));
						}
					}
				}
			}
		}
	}

	// MORDOR
	public static function handle_efield (ctx:context.Typecore.Typer, e:core.Ast.ExprDef, p:core.Globals.Pos, mode:AccessMode) : AccessKind {
		/*
			given chain of fields as the `path` argument and an `access_mode->access_kind` getter for some starting expression as `e`,
			return a new `access_mode->access_kind` getter for the whole field access chain.

			if `resume` is true, `Not_found` will be raised if the first field in chain fails to resolve, in all other
			cases, normal type errors will be raised if a field can't be accessed.
		*/
		function fields (?resume:Bool=false, path:ImmutableList<{fst:String, snd:Bool, trd:core.Globals.Pos}>, e:AccessMode->AccessKind) : AccessMode->AccessKind {
			var resume = new Ref(resume);
			var force = new Ref(false);
			var e = List.fold_left(function (e:AccessMode->AccessKind, path:{fst:String, snd:Bool, trd:core.Globals.Pos}) {
				var f = path.fst; var p = path.trd;
				var e = acc_get(ctx, e(MGet), p);
				var f = type_field.bind(resume.get(), ctx, e, f, p);
				force.set(resume.get());
				resume.set(false);
				return f;
			}, e, path);
			if (force.get()) {
				e(MCall); // not necessarily a call, but prevent #2602 among others
			}
			return e;
		}
		/*
			given a chain of identifiers (dot-path) represented as a list of (ident,starts_uppercase,pos) tuples,
			resolve it into an `access_mode->access_kind` getter for the resolved expression
		*/
		function type_path (path:ImmutableList<{fst:String, snd:Bool, trd:core.Globals.Pos}>) : AccessMode->AccessKind {
			/*
				this is an actual loop for processing a fully-qualified dot-path.
				it relies on the fact that packages start with a lowercase letter, while modules and types
				start with upper-case letters, so it processes path parts, accumulating lowercase package parts in `acc`,
				until it encounters an upper-case part, which can mean either a module access or module's primary type access,
				so it tries to figure out the type and and calls `fields` on it to resolve the rest of field access chain.
			*/
			function loop (acc:ImmutableList<{fst:String, snd:Bool, trd:core.Globals.Pos}>, path:ImmutableList<{fst:String, snd:Bool, trd:core.Globals.Pos}>) : AccessMode->AccessKind {
				return switch (path) {
					case (x={snd:false})::path:
						// part starts with lowercase - it's a package part, add it the accumulator and proceed
						loop(x::acc, path);
					case (x={fst:name, snd:true, trd:p})::path:
						// part starts with uppercase - it either points to a module or its main type

						// acc is contains all the package parts now, so extract package from them
						var pack = List.rev_map(function (p) { var x = p.fst; return x; }, acc);
						/* default behaviour: try loading module's primary type (with the same name as module)
							and resolve the rest of the field chain against its statics, or the type itself
							if the rest of chain is empty */
						function def() {
							return try {
								var e = type_type(ctx, new core.Path(pack, name), p);
								fields(path, function(_) { return AKExpr(e); });
							}
							catch (err:core.Error) {
								switch (err.msg) {
									case Module_not_found(m) if (m.equals(new core.Path(pack, name))):
										/* if it's not a module path after all, it could be an untyped field access that looks like
											a dot-path, e.g. `untyped __global__.String`, add the whole path to the accumulator and
											proceed to the untyped identifier resolution */
										loop(List.append(List.rev(path), x::acc), []);
									case _: throw err;
								}
							}
						}
						switch (path) {
							case {fst:sname, snd:true, trd:p}::path:
								/* next part starts with uppercase, meaning it can be either a module sub-type access
									or static field access for the primary module type, so we have to do some guessing here

									In this block, `name` is the first first-uppercase part (possibly a module name),
									and `sname` is the second first-uppsercase part (possibly a subtype name). */

								// get static field by `sname` from a given type `t`, if `resume` is true - raise Not_found
								function get_static (resume:Bool, t:core.Type.ModuleType) : AccessMode->AccessKind {
									return fields(resume, {fst:sname, snd:true, trd:p}::path, function (_) { return AKExpr(type_module_type(ctx, t, None, p)); });
								}
								// try accessing subtype or main class static field by `sname` in given module with path `m`
								function check_module (m:core.Path) : Option<AccessMode->AccessKind> {
									return try {
										var md = typing.Typeload.load_module(ctx, m, p);
										// first look for existing subtype
										try {
											var t = List.find(function (t) { return !(core.Type.t_infos(t).mt_private) && core.Type.t_path(t).equals(new core.Path(m.a, sname)); }, md.m_types);
											Some(fields(path, function(_) { return AKExpr(type_module_type(ctx, t, None, p)); }));
										}
										catch (_:ocaml.Not_found) {
											try {
												// then look for main type statics
												if (m.a == Tl) {
													throw ocaml.Not_found.instance; // ensure that we use def() to resolve local types first
												}
												var t = List.find(function (t) { return !(core.Type.t_infos(t).mt_private) && core.Type.t_path(t).equals(m); }, md.m_types);
												Some(get_static(false, t));
											}
											catch (_:ocaml.Not_found) {
												None;
											}
										}
									}
									catch (exc:core.Error) {
										switch (exc.msg) {
											case Module_not_found(m2) if (m.equals(m2)):
												None;
											case _: throw exc;
										}
									}
								}
								switch (pack) {
									case []:
										// if there's no package specified...
										try {
											/* first try getting a type by `name` in current module types and current imports
												and try accessing its static field by `sname` */
											function path_match (t:core.Type.ModuleType) { return core.Type.t_infos(t).mt_path.b == name; }
											var t = try {
												List.find(path_match, ctx.m.curmod.m_types); // types in this modules
											}
											catch (_:ocaml.Not_found) {
												var _tmp = List.find(function (arg:{mt:core.Type.ModuleType, pos:core.Globals.Pos}) { var t = arg.mt; return path_match(t); }, ctx.m.module_types);
												var t = _tmp.mt; var p = _tmp.pos;
												context.display.ImportHandling.maybe_mark_import_position(ctx, p);
												t;
											}
											get_static(true, t);
										}
										catch (_:ocaml.Not_found) {
											/* if the static field (or the type) wasn't not found, look for a subtype instead - #1916
												look for subtypes/main-class-statics in modules of current package and its parent packages */
											function loop (pack:ImmutableList<String>) : AccessMode->AccessKind {
												return switch (check_module(new core.Path(pack, name))) {
													case Some(r): r;
													case None:
														switch (List.rev(pack)) {
															case []: def();
															case _::l: loop(List.rev(l));
														}
												}
											}
											loop(ctx.m.curmod.m_path.a);
										}
									case _:
										/* if package was specified, treat it as fully-qualified access to either
											a module subtype or a static field of module's primary type*/
										switch (check_module(new core.Path(pack, name))) {
											case Some(r): r;
											case None: def();
										}
								}
							case _:
								/* no more parts or next part starts with lowercase - it's surely not a type name,
									so do the default thing: resolve fields against primary module type */
								def();
						}
					case []:
						/* If we get to here, it means that either there were no uppercase-first-letter parts,
							or we couldn't find the specified module, so it's not a qualified dot-path after all.
							And it's not a known identifier too, because otherwise `loop` wouldn't be called at all.
							So this must be an untyped access (or a typo). Try resolving the first identifier with support
							for untyped and resolve the rest of field chain against it.

							TODO: extract this into a separate function
						*/
						switch (List.rev(acc)) {
							case []: trace("Shall not be seen"); throw false;
							case {fst:name, snd:flag, trd:p}::path:
								try {
									fields(path, type_ident.bind(ctx, name, p));
								}
								catch (e:core.Error) {
									var p2 = e.pos;
									switch (e.msg) {
										case Unknown_ident(_) if (p.equals(p2)):
											try {
												// try raising a more sensible error if there was an uppercase-first (module name) part
												var path = new Ref<ImmutableList<String>>([]);
												var name = List.find(function (arg) {
													var name = arg.fst; var flag = arg.snd;
													if (flag) {
														return true;
													}
													else {
														path.set(name :: path.get());
														return false;
													}
												}, List.rev(acc)).fst;
												throw new core.Error(Module_not_found(new core.Path(List.rev(path.get()), name)), p);
											}
											catch (_:ocaml.Not_found) {
												// if there was no module name part, last guess is that we're trying to get package completion
												if (ctx.in_display) {
													throw new syntax.parser.TypePath(List.map(function (path) {
															var n = path.fst;
															return n;
														}, List.rev(acc)), None,false);
												}
												throw e;
											}
										case _: throw e;
									}
								}
						}
				}
			}
			return switch (path) {
				case []: trace("Shall not be seen"); throw false;
				case {fst:name, trd:p}::pnext:
					try {
						/*
							first, try to resolve the first ident in the chain and access its fields.
							this doesn't support untyped identifiers yet, because we want to check
							fully-qualified dot paths first even in an untyped block.
						*/
						fields(pnext, function(_) { return type_ident_raise(ctx, name, p, MGet); });
					}
					catch (_:ocaml.Not_found) {
						// first ident couldn't be resolved, it's probably a fully qualified path - resolve it
						loop([], path);
					}
			}
		}
		/*
			loop through the given EField expression and behave differently depending on whether it's a simple dot-path
			or a more complex expression, accumulating field access parts in form of (ident,starts_uppercase,pos) tuples.

			if it's a dot-path, then it might be either fully-qualified access (pack.Class.field) or normal field access of
			a local/global/field identifier. we pass the accumulated path to `type_path` and let it figure out what it is.

			if it's NOT a dot-path (anything other than indentifiers appears in EField chain), then we can be sure it's
			normal field access, not fully-qualified access, so we pass the non-ident expr along with the accumulated
			fields chain to the `fields` function and let it type the field access.
		*/
		function loop (acc:ImmutableList<{fst:String, snd:Bool, trd:core.Globals.Pos}>, expr:core.Ast.Expr) : AccessMode->AccessKind {
			var e = expr.expr; var p = expr.pos;
			return switch (e) {
				case EField(e, s):
					loop({fst:s, snd:!(core.Ast.is_lower_ident(s)), trd:p}::acc, e);
				case EConst(CIdent(i)):
					type_path({fst:i, snd:!(core.Ast.is_lower_ident(i)), trd:p}::acc);
				case _:
					fields(acc, type_access.bind(ctx, e, p));
			}
		}
		return loop([], {expr:e, pos:p})(mode);
	}

	public static function type_access (ctx:context.Typecore.Typer, e:core.Ast.ExprDef, p:core.Globals.Pos, mode:AccessMode) : AccessKind {
		return switch (e) {
			case EConst(CIdent(s)):
				type_ident(ctx, s, p, mode);
			case EField(e1, "new"):
				var e1 = type_expr(ctx, e1, Value);
				switch (e1.eexpr) {
					case TTypeExpr(TClassDecl(c)):
						if (mode == MSet) { core.Error.error("Cannot set constructor", p); }
						if (mode == MCall) { core.Error.error("Cannot call constructor like this, use 'new " + core.Globals.s_type_path(c.cl_path) + "()' instead", p); }
						var monos = List.map(function (_) { return core.Type.mk_mono(); }, c.cl_params);
						var _tmp = get_constructor(ctx, c, monos, p);
						var ct = _tmp.t; var cf = _tmp.cf;
						var args = switch (core.Type.follow(ct)) {
							case TFun({args:args, ret:ret}): args;
							case _: trace("Shall not be seen"); throw false;
						}
						var vl = List.map(function (a:core.Type.TSignatureArg) {
							var n = a.name; var t = a.t;
							return core.Type.alloc_var(n, t, c.cl_pos);
						}, args);
						function vexpr(v:core.Type.TVar) {
							return core.Type.mk(TLocal(v), v.v_type, p);
						}
						var el = List.map(vexpr, vl);
						var _tmp = switch (c.cl_kind) {
							case KAbstractImpl(a):
								var e = type_module_type(ctx, TClassDecl(c), None, p);
								e = core.Type.mk(TField(e, FStatic(c, cf)), ct, p);
								var t:core.Type.T = TAbstract(a, monos);
								{fst:make_call(ctx, e, el, t, p), snd:t};
							case _:
								var t:core.Type.T = TInst(c, monos);
								{fst:core.Type.mk(TNew(c, monos, el), t, p), snd:t};
						}
						var ec = _tmp.fst; var t = _tmp.snd;
						AccessKind.AKExpr(core.Type.mk(TFunction(
							{
								tf_args: List.map(function (v:core.Type.TVar) { return {v:v, c:None}; }, vl),
								tf_type: t,
								tf_expr: core.Type.mk(TReturn(Some(ec)), t, p)
							}
						), TFun(
							{args:List.map(function (v:core.Type.TVar) {
								return {name:v.v_name, opt:false, t:v.v_type};
							}, vl), ret:t}
						), p));
					case _:
						core.Error.error("Binding new is only allowed on class types", p);
				}
			case EField(_, _):
				handle_efield(ctx, e, p, mode);
			case EArray(e1, e2):
				var e1 = type_expr(ctx, e1, Value);
				var e2 = type_expr(ctx, e2, Value);
				var has_abstract_array_access = new Ref(false);
				try {
					switch (core.Type.follow(e1.etype)) {
						case TAbstract(a={a_impl:Some(c)}, pl) if (a.a_array != Tl):
							switch (mode) {
								case MSet:
									// resolve later
									AKAccess(a, pl, c, e1, e2);
								case _:
									has_abstract_array_access.set(true);
									var e = mk_array_get_call(ctx, context.typecore.AbstractCast.find_array_access(ctx, a, pl, e2, None, p), c, e1, p);
									AKExpr(e);
							}
						case _: throw ocaml.Not_found.instance;
					}
				}
				catch (_:ocaml.Not_found) {
					context.Typecore.unify(ctx, e2.etype, ctx.t.tint, e2.epos);
					function loop (et:core.Type.T) : core.Type.T {
						return switch (core.Type.follow(et)) {
							case TInst({cl_array_access:Some(t), cl_params:pl}, tl):
								core.Type.apply_params(pl, tl, t);
							case TInst({cl_super:Some({c:c, params:stl}), cl_params:pl}, tl):
								core.Type.apply_params(pl, tl, loop(TInst(c, stl)));
							case TInst({cl_path:{a:[], b:"ArrayAccess"}}, [t]):
								t;
							case TInst({cl_path:{a:[], b:"Array"}}, [t]) if ( t == core.Type.t_dynamic ):
								core.Type.t_dynamic;
							case TAbstract(a, tl) if (core.Meta.has(ArrayAccess, a.a_meta)):
								loop(core.Type.apply_params(a.a_params, tl, a.a_this));
							case _:
								var pt = core.Type.mk_mono();
								var t = ctx.t.tarray(pt);
								try {
									context.Typecore.unify_raise(ctx, et, t, p);
								}
								catch (err:core.Error) {
									switch (err.msg) {
										case Unify(_):
											if (!ctx.untyped_) {
												if (has_abstract_array_access.get()) {
													core.Error.error("No @:arrayAccess function accepts an argument of " + core.Type.s_type(core.Type.print_context(), e2.etype), e1.epos);
												}
												else {
													core.Error.error("Array access is not allowed on " + core.Type.s_type(core.Type.print_context(), e1.etype), e1.epos);
												}
											}
										case _: throw err;
									}
								}
								pt;
						}
					}
					var pt = loop(e1.etype);
					AKExpr(core.Type.mk(TArray(e1, e2), pt, p));
				}
			case _:
				AKExpr(type_expr(ctx, {expr:e, pos:p}, Value));
		};
	}

	public static function type_vars (ctx:context.Typecore.Typer, vl:ImmutableList<core.Ast.Var>, p:core.Globals.Pos) : core.Type.TExpr {
		var vl = List.map(function (variable:core.Ast.Var) {
			var v = variable.name.pack; var pv = variable.name.pos; var t = variable.type; var e = variable.expr;
			return
			try {
				var t = typing.Typeload.load_type_hint(ctx, p, t);
				var e = switch (e) {
					case None: None;
					case Some(e):
						var e = type_expr(ctx, e, WithType(t));
						Some(context.typecore.AbstractCast.cast_or_unify(ctx, t, e, p));
				}
				if (v.charAt(0) == "$") {
					context.Typecore.display_error(ctx, "Variables names starting with a dollar are not allowed", p);
				}
				var v = context.Typecore.add_local(ctx, v, t, pv);
				v.v_meta = ({name:UserVariable, params:[], pos:pv} : core.Ast.MetadataEntry) :: v.v_meta;
				if (ctx.in_display && context.Display.is_display_position(pv)) {
					context.display.DisplayEmitter.display_variable(ctx.com.display, v, pv);
				}
				{fst:v, snd:e};
			}
			catch (err:core.Error) {
				var e = err.msg; var p = err.pos;
				check_error(ctx, e, p);
				{fst:context.Typecore.add_local(ctx, v, core.Type.t_dynamic, pv), snd:None} // TODO: What to do with this...
			}
		}, vl);
		return switch (vl) {
			case [{fst:v, snd:eo}]:
				core.Type.mk(TVar(v, eo), ctx.t.tvoid, p);
			case _:
				var e = core.Type.mk(TBlock(List.map(function(_tmp) {
					var v = _tmp.fst; var e = _tmp.snd;
					return core.Type.mk(TVar(v, e), ctx.t.tvoid, p);
				}, vl)), ctx.t.tvoid, p);
				core.Type.mk(TMeta({name:MergeBlock, params:[], pos:p}, e), e.etype, e.epos);
		}
	}

	public static function type_block (ctx:context.Typecore.Typer, el:ImmutableList<core.Ast.Expr>, with_type:context.Typecore.WithType, p:core.Globals.Pos) : core.Type.TExpr {
		function merge (e:core.Type.TExpr) : ImmutableList<core.Type.TExpr> {
			return switch (e.eexpr) {
				case TMeta({name:MergeBlock}, {eexpr:TBlock(el)}): el;
				case _: [e];
			}
		}
		function loop (l:ImmutableList<core.Ast.Expr>) : ImmutableList<core.Type.TExpr> {
			return switch (l) {
				case []: [];
				case [e]:
					try {
						merge(type_expr(ctx, e, with_type));
					}
					catch (err:core.Error) {
						var e = err.msg; var p = err.pos;
						check_error(ctx, e, p);
						Tl;
					}
				case e::l:
					try {
						var e = type_expr(ctx, e, NoValue);
						List.append(merge(e), loop(l));
					}
					catch (err:core.Error) {
						var e = err.msg; var p = err.pos;
						check_error(ctx, e, p);
						loop(l);
					}
			}
		}
		var l = loop(el);
		function loop_(l:ImmutableList<core.Type.TExpr>) : core.Type.T {
			return switch (l) {
				case []: ctx.t.tvoid;
				case [e]: e.etype;
				case _::l: loop_(l);
			}
		}
		return core.Type.mk(TBlock(l), loop_(l), p);
	}

	public static function type_object_decl (ctx:context.Typecore.Typer, fl:ImmutableList<core.Ast.ObjectField>, with_type:context.Typecore.WithType, p:core.Globals.Pos) : core.Type.TExpr {
		trace("TODO: typing.Typer.type_object_decl");
		throw false;
	}

	public static function type_map_declaration (ctx:context.Typecore.Typer, e1:core.Ast.Expr, el:ImmutableList<core.Ast.Expr>, with_type:context.Typecore.WithType, p:core.Globals.Pos) : core.Type.TExpr {
		trace("TODO: typing.Typer.type_map_declaration");
		throw false;
	}

	public static function type_new (ctx:context.Typecore.Typer, path:core.Ast.PlacedTypePath, el:ImmutableList<core.Ast.Expr>, with_type:context.Typecore.WithType, p:core.Globals.Pos) : core.Type.TExpr {
		function unify_constructor_call (c:core.Type.TClass, params:core.Type.TParams, f:core.Type.TClassField, ct:core.Type.T) : ImmutableList<core.Type.TExpr> {
			return switch (core.Type.follow(ct)) {
				case TFun({args:args, ret:r}):
					try {
						var el = unify_field_call(ctx, FInstance(c, params, f), el, args, r, p, false).fst;
						el;
					}
					catch (exc:core.Error) {
						var e = exc.msg; var p = exc.pos;
						context.Typecore.display_error(ctx, core.Error.error_msg(e), p);
						Tl; // []
					}
				case _: core.Error.error("Constructor is not a function", p);
			}
		}
		var t = if (path.tp.tparams != Tl) {
			core.Type.follow(typing.Typeload.load_instance(ctx, path, false, p));
		}
		else {
			try {
				ctx.call_argument_stack = el :: ctx.call_argument_stack;
				var t = core.Type.follow(typing.Typeload.load_instance(ctx, path, true, p));
				ctx.call_argument_stack = List.tl(ctx.call_argument_stack);
				// Try to properly build @:generic classes here (issue #2016)
				switch (t) {
					case TInst(c={cl_kind:KGeneric}, tl):
						core.Type.follow(typing.Typeload.build_generic(ctx, c, p, tl));
					case _: t;
				}
			}
			catch (_:typing.Typeload.GenericException) {
				// Try to infer generic parameters from the argument list (issue #2044)
				switch (core.Type.resolve_typedef(typing.Typeload.load_type_def(ctx, p, path.tp))) {
					case TClassDecl(c={cl_constructor:Some(cf)}):
						var monos = List.map(function (_) { return core.Type.mk_mono(); }, c.cl_params);
						var _tmp = get_constructor(ctx, c, monos, p);
						var ct = _tmp.t; var f = _tmp.cf;
						unify_constructor_call(c, monos, f, ct);
						try {
							var t = typing.Typeload.build_generic(ctx, c, p, monos);
							var map = core.Type.apply_params.bind(c.cl_params, monos);
							check_constraints(ctx, core.Globals.s_type_path(c.cl_path), c.cl_params, monos, map, true, p);
							t;
						}
						catch (exc:typing.Typeload.GenericException) {
							switch (with_type) {
								case WithType(t):
									switch (core.Type.follow(t)) {
										case TMono(_): throw exc;
										case t: t;
									}
								case _: throw exc;
							}
						}
					case mt:
						core.Error.error(core.Globals.s_type_path(core.Type.t_infos(mt).mt_path) + " cannot be constructed", p);
				}
			}
		}
		context.display.DisplayEmitter.check_display_type(ctx, t, path.pos);
		function build_constructor_call(c:core.Type.TClass, tl:core.Type.TParams) {
			var _tmp = get_constructor(ctx, c, tl, p);
			var ct = _tmp.t; var f = _tmp.cf;
			if (core.Meta.has(CompilerGenerated, f.cf_meta)) {
				context.Typecore.display_error(ctx, core.Error.error_msg(No_constructor(TClassDecl(c))), p);
			}
			if (!(context.Typecore.can_access(ctx, c, f, true) || core.Type.is_parent(c, ctx.curclass)) && !ctx.untyped_) {
				context.Typecore.display_error(ctx, "Cannot access private constructor", p);
			}
			switch (f.cf_kind) {
				case Var({v_read:AccRequire(r, msg)}):
					switch (msg) {
						case Some(msg): core.Error.error(msg, p);
						case None: error_require(r, p);
					}
				case _:
			}
			var el = unify_constructor_call(c, tl, f, ct);
			return {fst:el, snd:f, trd:ct};
		}
		return
		try {
			switch (t) {
				case TInst(c={cl_kind:KTypeParameter(tl)}, params):
					if (!typing.Typeload.is_generic_parameter(ctx, c)) {
						core.Error.error("Only generic type parameters can be constructed", p);
					}
					var el = List.map(function (e) { return type_expr(ctx, e, Value); }, el);
					var ct = core.Type.tfun(List.map(function (e) { return e.etype; }, el), ctx.t.tvoid);
					function loop(t) {
						return switch (core.Type.follow(t)) {
							case TAnon(a):
								try {
									context.Typecore.unify(ctx, PMap.find("new", a.a_fields).cf_type, ct, p);
									true;
								}
								catch (_:ocaml.Not_found) {
									false;
								}
							case TAbstract({a_path:{a:["haxe"], b:"Constructible"}}, _): true;
							case TInst({cl_kind:KTypeParameter(tl)}, _): List.exists(loop, tl);
							case _: false;
						}
					}
					if (!List.exists(loop, tl)) {
						core.Error.raise_error(No_constructor(TClassDecl(c)), p);
					}
					core.Type.mk(TNew(c, params, el), t, p);
				case TAbstract(a={a_impl:Some(c)}, tl) if (!core.Meta.has(MultiType, a.a_meta)):
					var _tmp = build_constructor_call(c, tl);
					var el = _tmp.fst; var cf = _tmp.snd; var ct = _tmp.trd;
					var ta:core.Type.T = TAnon({a_fields:c.cl_statics, a_status:new Ref<core.Type.AnonStatus>(Statics(c))});
					var e = core.Type.mk(TTypeExpr(TClassDecl(c)), ta, p);
					var e = core.Type.mk(TField(e, FStatic(c, cf)), ct, p);
					make_call(ctx, e, el, t, p);
				case TInst(c, params), TAbstract({a_impl:Some(c)}, params):
					var el = build_constructor_call(c, params).fst;
					core.Type.mk(TNew(c, params, el), t, p);
				case _:
					core.Error.error(core.Type.s_type(core.Type.print_context(), t)+ " cannot be constructed", p);
			}
		}
		catch (exc:core.Error) {
			var err = exc.msg; var p = exc.pos;
			switch (err) {
				case No_constructor(_) if (ctx.com.display.dms_display):
					context.Typecore.display_error(ctx, core.Error.error_msg(err), p);
					context.display.Diagnostics.secure_generated_code(ctx, core.Type.mk(TConst(TNull), t, p));
				case _: throw exc;
			}
		}
	}

	public static function type_try (ctx:context.Typecore.Typer, e1:core.Ast.Expr, catches:ImmutableList<core.Ast.Catch>, with_type:context.Typecore.WithType, p:core.Globals.Pos) : core.Type.TExpr {
		var e1 = type_expr(ctx, core.ast.Expr.ensure_block(e1), with_type);
		function check_unreachable(cases:ImmutableList<{v:core.Type.TVar, e:core.Type.TExpr}>, t:core.Type.T, p:core.Globals.Pos) {
			return switch (cases) {
				case {v:v, e:e}::cases:
					function unreachable () : Void {
						context.Typecore.display_error(ctx, "This block is unreachable", p);
						var st = core.Type.s_type.bind(core.Type.print_context());
						context.Typecore.display_error(ctx, '${st(t)} can be assigned to ${st(v.v_type)}, which is handled here', e.epos);
					}
					try {
						switch [core.Type.follow(t), core.Type.follow(v.v_type)] {
							case [TDynamic(_), TDynamic(_)]: unreachable();
							case [TDynamic(_), _]:
							case _:
								core.Type.unify(t, v.v_type);
								unreachable();
						}
					}
					catch (_:core.Type.Unify_error) {
						check_unreachable(cases, t, p);
					}
				case []:
			}
		}
		function check_catch_type (path:core.Path, params:core.Type.TParams) : String {
			List.iter(function (pt:core.Type.T) {
				if (pt != core.Type.t_dynamic) {
					core.Error.error("Catch class parameter must be Dynamic", p);
				}
			}, params);
			return switch (path) {
				case {a:x::_}: x;
				case {a:[], b:name}: name;
			}
		}
		var catches = List.fold_left(function (acc:ImmutableList<{v:core.Type.TVar, e:core.Type.TExpr}>, _catch:core.Ast.Catch) {
			var v = _catch.name.pack; var pv = _catch.name.pos; var t = _catch.type; var e_ast = _catch.expr; var pc = _catch.pos;
			var t = typing.Typeload.load_complex_type(ctx, true, p, t);
			function loop (t:core.Type.T) : {fst:String, snd:core.Type.T} {
				return switch (core.Type.follow(t)) {
					case TInst(c={cl_kind:KTypeParameter(_)}, _) if (!typing.Typeload.is_generic_parameter(ctx, c)):
						core.Error.error("Cannot catch non-generic type parameter", p);
					case TInst({cl_path:path}, params), TEnum({e_path:path}, params):
						{fst:check_catch_type(path, params), snd:t};
					case TAbstract(a, params) if (core.Meta.has(RuntimeValue, a.a_meta)):
						{fst:check_catch_type(a.a_path, params), snd:t};
					case TAbstract(a, tl) if (!core.Meta.has(CoreType, a.a_meta)):
						loop(core.Abstract.get_underlying_type(a, tl));
					case TDynamic(_):
						{fst:"", snd:t};
					case _:
						core.Error.error("Catch type must be a class, an enum or Dynamic", e_ast.pos);
				}
			}
			var _tmp = loop(t);
			var name = _tmp.fst; var t2 = _tmp.snd;
			if (v.charAt(0) == "$") {
				context.Typecore.display_error(ctx, "Catch variable names starting with a dollar are not allowed", p);
			}
			check_unreachable(acc, t2, e_ast.pos);
			var locals = context.Typecore.save_locals(ctx);
			var v = context.Typecore.add_local(ctx, v, t, pv);
			if (ctx.is_display_file && context.Display.is_display_position(pv)) {
				context.display.DisplayEmitter.display_variable(ctx.com.display, v, pv);
			}
			var e = type_expr(ctx, e_ast, with_type);
			/* If the catch position is the display position it means we get completion on the catch keyword or some
				punctuation. Otherwise we wouldn't reach this point. */
			if (ctx.is_display_file && context.Display.is_display_position(pc)) {
				display_expr(ctx, e_ast, e, with_type, pc);
			}
			v.v_type = t2;
			locals();
			if (with_type != NoValue) {
				context.Typecore.unify(ctx, e.etype, e1.etype, e.epos);
			}
			if (PMap.mem(name, ctx.locals)) {
				core.Error.error("Local variable "+name+" is preventing usage of this type here", e.epos);
			}
			return {v:v, e:e}::acc;
		}, [], catches);
		return core.Type.mk(TTry(e1, List.rev(catches)), (with_type == NoValue) ? ctx.t.tvoid : e1.etype ,p);
	}

	public static function type_local_function (ctx:context.Typecore.Typer, name:Option<String>, f:core.Ast.Func, with_type:context.Typecore.WithType, p:core.Globals.Pos) : core.Type.TExpr {
		var params = typing.Typeload.type_function_params(ctx, f, switch (name) { case None: "localfun"; case Some(n): n;}, p);
		if (params != Tl) {
			if (name == None) {
				context.Typecore.display_error(ctx, "Type parameters not supported in unnamed local functions", p);
			}
			if (with_type != NoValue) {
				core.Error.error("Type parameters are not supported for rvalue functions", p);
			}
		}
		List.iter (function (tp:core.Ast.TypeParam) {
			if (tp.tp_constraints != Tl) {
				context.Typecore.display_error(ctx, "Type parameter constraints are not supported for local functions", p);
			}
		}, f.f_params);
		var _tmp = switch(name) {
			case None: {fst:false, snd:None};
			case Some(v) if (StringTools.startsWith(v, "inline_")): {fst:true, snd:Some(v.substr(7))};
			case Some(v): {fst:false, snd:Some(v)};
		}
		var inline_ = _tmp.fst; var v = _tmp.snd;
		var old_tp = ctx.type_params; var old_in_loop = ctx.in_loop;
		ctx.type_params = List.append(params, ctx.type_params);
		if (!inline_) { ctx.in_loop = false; }
		var rt = typing.Typeload.load_type_hint(ctx, p, f.f_type);
		var args = List.map(function (arg:core.Ast.FunArg) : {name:String, opt:Option<core.Ast.Expr>, t:core.Type.T} {
			var s = arg.name.pack; var opt = arg.opt; var t = arg.type; var c = arg.value;
			var t = typing.Typeload.load_type_hint(ctx, p, t);
			var _tmp = typing.Typeload.type_function_arg(ctx, t, c, opt, p);
			var t = _tmp.fst; var c = _tmp.snd;
			return {name:s, opt:c, t:t};
		}, f.f_args);
		switch (with_type) {
			case WithType(t):
				function loop (t:core.Type.T) {
					switch (core.Type.follow(t)) {
						case TFun({args:args2, ret:tr}) if (List.length(args2) == List.length(args)):
							List.iter2(function (a1:{name:String, opt:Option<core.Ast.Expr>, t:core.Type.T}, a2:core.Type.TSignatureArg) {
								var t1 = a1.t; var t2 = a2.t;
								switch (core.Type.follow(t1)) {
									case TMono(_): context.Typecore.unify(ctx, t2, t1, p);
									case _:
								}
							}, args, args2);
							// unify for top-down inference unless we are expecting Void
							switch [core.Type.follow(tr), core.Type.follow(rt)] {
								case [TAbstract({a_path:{a:[], b:"Void"}}, _), _]:
								case [_, TMono(_)]: context.Typecore.unify(ctx, rt, tr, p);
								case _:
							}
						case TAbstract(a, tl):
							loop(core.Abstract.get_underlying_type(a, tl));
						case _:
					}
				}
				loop(t);
			case NoValue:
				if (name == None) {
					context.Typecore.display_error(ctx, "Unnamed lvalue functions are not supported", p);
				}
			case _:
		}
		var ft:core.Type.T = TFun({args:core.Type.fun_args(args), ret:rt});
		var v = switch (v) {
			case None: None;
			case Some(v):
				if (v.charAt(0) == "$") {
					context.Typecore.display_error(ctx, "Variable names starting with a dollar are not allowed", p);
				}
				Some(context.Typecore.add_local(ctx, v, ft, p)); // TODO: var pos;
		}
		var curfun:context.Typecore.CurrentFun = switch (ctx.curfun) {
			case FunStatic: FunStatic;
			case FunMemberAbstract: FunMemberAbstractLocal;
			case _: FunMemberClassLocal;
		}
		var _tmp = typing.Typeload.type_function(ctx, args, rt, curfun, f, ctx.in_display, p);
		var e = _tmp.fst; var fargs = _tmp.snd;
		ctx.type_params = old_tp;
		ctx.in_loop = old_in_loop;
		var f = {
			tf_args: fargs,
			tf_type: rt,
			tf_expr: e
		};
		var e = core.Type.mk(TFunction(f), ft, p);
		return switch (v) {
			case None: e;
			case Some(v):
				if (params != Tl || inline_) {
					v.v_extra = Some({params:params, expr:(inline_) ? Some(e) : None});
				}
				function loop (u:filters.LocalUsage.Usage) {
					switch (u) {
						case Block(f), Loop(f), Function(f): f(loop);
						case Use(v2), Assign(v2) if (v.equals(v2)): throw ocaml.Exit.instance;
						case Use(_), Assign(_), Declare(_):
					}
				}
				var is_rec = try { filters.LocalUsage.local_usage(loop, e); false; } catch (_:ocaml.Exit) { true; }
				var decl = if (is_rec) {
					if (inline_) {
						context.Typecore.display_error(ctx, "Inline function cannot be recursive", e.epos);
					}
					var vnew = context.Typecore.add_local(ctx, v.v_name, ft, v.v_pos);
					core.Type.mk(TVar(vnew, Some(core.Type.mk(TBlock([
						core.Type.mk(TVar(v, Some(core.Type.mk(TConst(TNull), ft, p))), ctx.t.tvoid, p),
						core.Type.mk(TBinop(OpAssign, core.Type.mk(TLocal(v), ft, p), e), ft, p),
						core.Type.mk(TLocal(v), ft, p)
					]), ft, p))), ctx.t.tvoid, p);
				}
				else if (inline_) {
					core.Type.mk(TBlock([]), ctx.t.tvoid, p); // do not add variable since it will be inlined
				}
				else {
					core.Type.mk(TVar(v, Some(e)), ctx.t.tvoid, p);
				}
				if (with_type != NoValue && !inline_) {
					core.Type.mk(TBlock([decl, core.Type.mk(TLocal(v), v.v_type, p)]), v.v_type, p);
				}
				else {
					decl;
				}
		}
	}

	public static function type_array_decl (ctx:context.Typecore.Typer, el:ImmutableList<core.Ast.Expr>, with_type:context.Typecore.WithType, p:core.Globals.Pos) : core.Type.TExpr {
		var tp = switch (with_type) {
			case WithType(t):
				function loop(t:core.Type.T) {
					return switch(core.Type.follow(t)) {
						case TInst({cl_path:{a:[], b:"Array"}}, [tp]):
							switch (core.Type.follow(tp)) {
								case TMono(_): None;
								case _: Some(tp);
							}
						case TAnon(_):
							try {
								Some(get_iterable_param(t));
							}
							catch (_:ocaml.Not_found) {
								None;
							}
						case TAbstract(a, pl):
							switch (List.fold_left(function (acc:ImmutableList<core.Type.T>, t:core.Type.T) {
								return switch (loop(t)) {
									case None: acc;
									case Some(t): t :: acc;
								}
							}, [], get_abstract_froms(a, pl))) {
								case [t]: Some(t);
								case _: None;
							}
						case t:
							(t == core.Type.t_dynamic) ? Some(t) : None;
					}
				}
				loop(t);
			case _: None;
		}
		return switch (tp) {
			case None:
				var el = List.map(function (e:core.Ast.Expr) { return type_expr(ctx, e, Value); }, el);
				var t = try {
					unify_min_raise(ctx, el);
				}
				catch (err:core.Error) {
					var p = err.pos;
					switch (err.msg) {
						case Unify(l):
							if (ctx.com.display.dms_error_policy == EPIgnore || ctx.untyped_) {
								core.Type.t_dynamic;
							}
							else {
								context.Typecore.display_error(ctx, "Arrays of mixed types are only allowed if the type is forced to Array<Dynamic>", p);
								throw new core.Error(Unify(l), p);
							}
						case _: throw err;
					}
				}
				core.Type.mk(TArrayDecl(el), ctx.t.tarray(t), p);
			case Some(t):
				var el = List.map(function (e:core.Ast.Expr) {
					var e = type_expr(ctx, e, WithType(t));
					return context.typecore.AbstractCast.cast_or_unify(ctx, t, e, p);
				}, el);
				core.Type.mk(TArrayDecl(el), ctx.t.tarray(t), p);
		}
	}

	public static function type_expr (ctx:context.Typecore.Typer, expr:core.Ast.Expr, with_type:context.Typecore.WithType) : core.Type.TExpr {
		var e = expr.expr; var p = expr.pos;
		return switch (e) {
			case EField({expr:EConst(CString(s)), pos:ps}, "code"):
				if (Utf8.length(s) != 1) {
					core.Error.error("String must be a single UTF8 char", ps);
				}
				core.Type.mk(TConst(TInt(Utf8.charCodeAt(s, 0))), ctx.t.tint, p);
			case EField(_, n) if (n.charAt(0) == "$"):
				core.Error.error("Field names starting with $ are not allowed", p);
			case EConst(CIdent(s)):
				if (s == "super" && with_type != NoValue && !ctx.in_display) {
					core.Error.error("Cannot use super as value", p);
				}
				var e = maybe_type_against_enum(ctx, function () {
					return type_ident(ctx, s, p, MGet);
				}, with_type, false, p);
				acc_get(ctx, e, p);
			case EField(_), EArray(_):
				acc_get(ctx, type_access(ctx, e, p, MGet), p);
			case EConst(CRegexp(r, opt)):
				var str = core.Type.mk(TConst(TString(r)), ctx.t.tstring, p);
				var opt = core.Type.mk(TConst(TString(opt)), ctx.t.tstring, p);
				var t = typing.Typeload.load_core_type(ctx, "EReg");
				core.Type.mk(TNew(switch (t) { case TInst(c, []): c; case _: trace("Shall not be seen"); throw false; }, [], [str, opt]), t, p);
			case EConst(CString(s)) if (s != "" && syntax.Lexer.is_fmt_string(p)):
				type_expr(ctx, format_string(ctx, s, p), with_type);
			case EConst(c):
				core.Texpr.type_constant(ctx.com.basic, c, p);
			case EBinop(op, e1, e2):
				type_binop(ctx, op, e1, e2, false, with_type, p);
			case EBlock([]) if (with_type != NoValue):
				type_expr(ctx, {expr:EObjectDecl([]), pos:p}, with_type);
			case EBlock(l):
				var locals = context.Typecore.save_locals(ctx);
				var e = type_block(ctx, l, with_type, p);
				locals();
				e;
			case EParenthesis(e):
				var e = type_expr(ctx, e, with_type);
				core.Type.mk(TParenthesis(e), e.etype, p);
			case EObjectDecl(fl):
				type_object_decl(ctx, fl, with_type, p);
			case EArrayDecl([e=({expr:EFor(_)} | {expr:EWhile(_)} )]):
				var v = context.Typecore.gen_local(ctx, core.Type.mk_mono(), p);
				var et = new Ref<core.Ast.Expr>({expr:EConst(CIdent("null")), pos:p});
				function map_compr (expr:core.Ast.Expr) : core.Ast.Expr {
					var e = expr.expr; var p = expr.pos;
					return switch (e) {
						case EFor(it, e2): {expr:EFor(it, map_compr(e2)), pos:p};
						case EWhile(cond, e2, flag): {expr:EWhile(cond, map_compr(e2), flag), pos:p};
						case EIf(cond, e2, None): {expr:EIf(cond, map_compr(e2), None), pos:p};
						case EBlock([e]): {expr:EBlock([map_compr(e)]), pos:p};
						case EBlock(el):
							switch (List.rev(el)) {
								case e::el: {expr:EBlock(List.append(List.rev(el), [map_compr(e)])), pos:p};
								case []: {expr: e, pos:p};
							}
						case EParenthesis(e2): {expr:EParenthesis(map_compr(e2)), pos:p};
						case EBinop(OpArrow, a, b):
							et.set({
								expr:ENew({tp:{tpackage:["haxe", "ds"], tname:"Map", tparams:[], tsub:None}, pos:core.Globals.null_pos}, []),
								pos: p
							});
							{expr:ECall({expr:EField({expr:EConst(CIdent(v.v_name)),pos:p},"set"), pos:p},[a,b]), pos:p};
						case _:
							et.set({expr:EArrayDecl([]), pos: p});
							{expr:ECall({expr:EField({expr:EConst(CIdent(v.v_name)),pos:p},"push"), pos:p},[{expr:e, pos:p}]), pos:p};
					}
				}
				var e = map_compr(e);
				var ea = type_expr(ctx, et.get(), with_type);
				context.Typecore.unify(ctx, v.v_type, ea.etype, p);
				var efor = type_expr(ctx, e, NoValue);
				core.Type.mk(TBlock([
					core.Type.mk(TVar(v, Some(ea)), ctx.t.tvoid, p),
					efor,
					core.Type.mk(TLocal(v), v.v_type, p)
				]), v.v_type, p);
			case EArrayDecl((e1={expr:EBinop(OpArrow, _, _)})::el):
				type_map_declaration(ctx, e1, el, with_type, p);
			case EArrayDecl(el):
				type_array_decl(ctx, el, with_type, p);
			case EVars(vl):
				type_vars(ctx, vl, p);
			case EFor(it, e2):
				function loop_ident(display:Bool, e1:core.Ast.Expr) {
					return switch (e1) {
						case {expr:EConst(CIdent(i))}: {fst:i, snd:p, trd:display};
						case {expr:EDisplay(e1, _)}: loop_ident(true, e1);
						case _: core.Error.error("Identifier expected", e1.pos);
					}
				}
				function loop(display:Bool, e1:core.Ast.Expr) {
					return switch (e1.expr) {
						case EBinop(OpIn, e1, e2): {fst:loop_ident(display, e1), snd:e2};
						case EDisplay(e1, _): loop(true, e1);
						case _: core.Error.error("For expression should be 'v in epxr'", it.pos);
					}
				}
				var _tmp = loop(false, it);
				var i = _tmp.fst.fst; var pi = _tmp.fst.snd; var display = _tmp.fst.trd; var e1 = _tmp.snd;
				var e1 = type_expr(ctx, e1, Value);
				var old_loop = ctx.in_loop;
				var old_locals = context.Typecore.save_locals(ctx);
				ctx.in_loop = true;
				var e2 = core.ast.Expr.ensure_block(e2);
				function default_() : core.Type.TExpr {
					var _tmp = typing.Typeload.t_iterator(ctx);
					var t = _tmp.fst; var pt = _tmp.snd;
					var i = context.Typecore.add_local(ctx, i, pt, pi);
					var e1 = switch (core.Type.follow(e1.etype)) {
						case TMono(_), TDynamic(_):
							context.Typecore.display_error(ctx, "You can't iterate on a Dynamic value, please specify Iterator or Iterable", e1.epos);
							e1;
						case TLazy(_): trace("Shall not be seen"); throw false;
						case _:
							try {
								context.typecore.AbstractCast.cast_or_unify_raise(ctx, t, e1, p);
							}
							catch (err:core.Error) {
								switch (err.msg) {
									case Unify(_):
										var acc = build_call(ctx, type_field(ctx, e1, "iterator", e1.epos, MCall), [], Value, e1.epos);
										try {
											context.Typecore.unify_raise(ctx, acc.etype, t, acc.epos);
											acc;
										}
										catch (err2:core.Error) {
											switch (err2.msg) {
												case Unify(l):
													context.Typecore.display_error(ctx, "Field iterator has an invalid type", acc.epos);
													context.Typecore.display_error(ctx, core.Error.error_msg(err2.msg), err2.pos);
													core.Type.mk(TConst(TNull), core.Type.t_dynamic, p);
												case _: throw err;
											}
										}
									case _: throw err;
								}
							}

					}
					if (display) {
						handle_display(ctx, {expr:EConst(CIdent(i.v_name)), pos:i.v_pos}, WithType(i.v_type));
					}
					var e2 = type_expr(ctx, e2, NoValue);
					return try {
						optimization.Optimizer.optimize_for_loop_iterator(ctx, i, e1, e2, p);
					}
					catch (_:ocaml.Exit) {
						core.Type.mk(TFor(i, e1, e2), ctx.t.tvoid, p);
					}
				}
				var e = switch (optimization.Optimizer.optimize_for_loop(ctx, i, pi, e1, e2, p)) {
					case Some(e):
						if (display) {
							handle_display(ctx, {expr:EConst(CIdent(i)), pos:pi}, Value);
						}
						e;
					case None: default_();
				}
				ctx.in_loop = old_loop;
				old_locals();
				e;
			case ETernary(e1, e2, e3):
				type_expr(ctx, {expr:EIf(e1, e2, Some(e3)), pos:p}, with_type);
			case EIf(e, e1, e2):
				var e = type_expr(ctx, e, Value);
				e = context.typecore.AbstractCast.cast_or_unify(ctx, ctx.t.tbool, e, p);
				var e1 = type_expr(ctx, core.ast.Expr.ensure_block(e1), with_type);
				switch (e2) {
					case None:
						core.Type.mk(TIf(e, e1, None), ctx.t.tvoid, p);
					case Some(e2):
						var e2 = type_expr(ctx, core.ast.Expr.ensure_block(e2), with_type);
						var _tmp = switch (with_type) {
							case NoValue: {fst:e1, snd:e2, trd:ctx.t.tvoid};
							case Value: {fst:e1, snd:e2, trd:unify_min(ctx, [e1, e2])};
							case WithType(t) if (switch (core.Type.follow(t)) {case TMono(_): true; case _: false;} ):
								{fst:e1, snd:e2, trd:unify_min(ctx, [e1, e2])};
							case WithType(t):
								{
									fst:context.typecore.AbstractCast.cast_or_unify(ctx, t, e1, e1.epos),
									snd:context.typecore.AbstractCast.cast_or_unify(ctx, t, e2, e2.epos),
									trd:t
								}
						}
						var e1 = _tmp.fst; e2 = _tmp.snd; var t = _tmp.trd;
						core.Type.mk(TIf(e, e1, Some(e2)), t, p);
				}
			case EWhile(cond, e, NormalWhile):
				var old_loop = ctx.in_loop;
				var cond = type_expr(ctx, cond, Value);
				cond = context.typecore.AbstractCast.cast_or_unify(ctx, ctx.t.tbool, cond, p);
				ctx.in_loop = true;
				var e = type_expr(ctx, core.ast.Expr.ensure_block(e), NoValue);
				ctx.in_loop = old_loop;
				core.Type.mk(TWhile(cond, e, NormalWhile), ctx.t.tvoid, p);
			case EWhile(cond, e, DoWhile):
				var old_loop = ctx.in_loop;
				ctx.in_loop = true;
				var e = type_expr(ctx, core.ast.Expr.ensure_block(e), NoValue);
				ctx.in_loop = old_loop;
				var cond = type_expr(ctx, cond, Value);
				cond = context.typecore.AbstractCast.cast_or_unify(ctx, ctx.t.tbool, cond, cond.epos);
				core.Type.mk(TWhile(cond, e, DoWhile), ctx.t.tvoid, p);
			case ESwitch(e1, cases, def):
				function wrap(e1) {
					return core.Type.mk(TMeta({name:Ast, params:[{expr:e, pos:p}], pos:p}, e1), e1.etype, e1.epos);
				}
				var e = context.Typecore.match_expr(ctx, e1, cases, def, with_type, p);
				wrap(e);
			case EReturn(e):
				switch (e) {
					case None:
						var v = ctx.t.tvoid;
						context.Typecore.unify(ctx, v, ctx.ret, p);
						core.Type.mk(TReturn(None), core.Type.t_dynamic, p);
					case Some(e):
						try {
							var e = type_expr(ctx, e, WithType(ctx.ret));
							e = context.typecore.AbstractCast.cast_or_unify(ctx, ctx.ret, e, p);
							switch (core.Type.follow(e.etype)) {
								case TAbstract({a_path:{a:[], b:"Void"}}, _):
									// if we get a Void expression (e.g. from inlining) we don't want to return it (issue #4323)
									core.Type.mk(TBlock([
										e,
										core.Type.mk(TReturn(None), core.Type.t_dynamic, p)
									]), core.Type.t_dynamic, e.epos);
								case _:
									core.Type.mk(TReturn(Some(e)), core.Type.t_dynamic, p);
							}
						}
						catch (exc:core.Error) {
							var err = exc.msg; var p = exc.pos;
							check_error(ctx, err, p);
							/* If we have a bad return, let's generate a return null expression at least. This surpresses various
							   follow-up errors that come from the fact that the function no longer has a return expression (issue #6445). */
							var e_null = core.Type.mk(TConst(TNull), core.Type.mk_mono(), p);
							core.Type.mk(TReturn(Some(e_null)), core.Type.t_dynamic, p);
						}
				}
			case EBreak:
				if (!ctx.in_loop) {
					context.Typecore.display_error(ctx, "Break outside loop", p);
				}
				core.Type.mk(TBreak, core.Type.t_dynamic, p);
			case EContinue:
				if (!ctx.in_loop) {
					context.Typecore.display_error(ctx, "Continue outside loop", p);
				}
				core.Type.mk(TContinue, core.Type.t_dynamic, p);
			case ETry(e1, []):
				type_expr(ctx, e1, with_type);
			case ETry(e1, catches):
				type_try(ctx, e1, catches, with_type, p);
			case EThrow(e):
				var e = type_expr(ctx, e, Value);
				core.Type.mk(TThrow(e), core.Type.mk_mono(), p);
			case ECall(e, el):
				type_call(ctx, e, el, with_type, p);
			case ENew(t, el):
				type_new(ctx, t, el, with_type, p);
			case EUnop(op, flag, e):
				type_unop(ctx, op, flag, e, p);
			case EFunction(name, f):
				type_local_function(ctx, name, f, with_type, p);
			case EUntyped(e):
				var old = ctx.untyped_;
				ctx.untyped_ = true;
				if (!core.Meta.has(HasUntyped, ctx.curfield.cf_meta)) {
					ctx.curfield.cf_meta = ({name:HasUntyped,params:[],pos:p} : core.Ast.MetadataEntry) :: ctx.curfield.cf_meta;
				}
				var e = type_expr(ctx, e, with_type);
				ctx.untyped_ = old;
				{
					eexpr:e.eexpr,
					etype:core.Type.mk_mono(),
					epos:e.epos
				};
			case ECast(e, None):
				var e = type_expr(ctx, e, Value);
				core.Type.mk(TCast(e, None), core.Type.mk_mono(), p);
			case ECast(e, Some(t)):
				var t = typing.Typeload.load_complex_type(ctx, true, p, t);
				function check_param(pt:core.Type.T) : Void {
					switch (core.Type.follow(pt)) {
						case TMono(_): // This probably means that Dynamic wasn't bound (issue #4675).
						case t if (t.equals(core.Type.t_dynamic)):
						case _: core.Error.error("Cast type parameters must be Dynamic", p);
					}
				}
				function loop (t:core.Type.T) : core.Type.ModuleType {
					return switch(core.Type.follow(t)) {
						case TInst(_, params), TEnum(_, params):
							List.iter(check_param, params);
							switch (core.Type.follow(t)) {
								case TInst(c, _):
									switch (c.cl_kind) {
										case KTypeParameter(_): core.Error.error("Can't cast to a type parameter", p);
										case _:
									}
									TClassDecl(c);
								case TEnum(e, _): TEnumDecl(e);
								case _: trace("Shall not be seen"); throw false;
							}
						case TAbstract(a, params) if (core.Meta.has(RuntimeValue, a.a_meta)):
							List.iter(check_param, params);
							TAbstractDecl(a);
						case TAbstract(a, params):
							loop(core.Abstract.get_underlying_type(a, params));
						case _:
							core.Error.error("Cast type must be a class or an enum", p);
					}
				}
				var texpr = loop(t);
				core.Type.mk(TCast(type_expr(ctx, e, Value), Some(texpr)), t, p);
			case EDisplay(e, iscall):
				switch (ctx.com.display.dms_kind) {
					case DMField, DMSignature if (iscall):
						handle_signature_display(ctx, e, with_type);
					case _:
						handle_display(ctx, e, with_type);
				}
			case EDisplayNew(t):
				trace("Shall not be seen"); throw false;
				/*let t = Typeload.load_instance ctx t true p in
				(match follow t with
				| TInst (c,params) | TAbstract({a_impl = Some c},params) ->
					let ct, f = get_constructor ctx c params p in
					raise (Display.DisplaySignatures ((ct,f.cf_doc) :: List.map (fun f -> (f.cf_type,f.cf_doc)) f.cf_overloads))
				| _ ->
					error "Not a class" p)*/
			case ECheckType(e, t):
				var t = typing.Typeload.load_complex_type(ctx, true, p, t);
				var e = type_expr(ctx, e, WithType(t));
				var e = context.typecore.AbstractCast.cast_or_unify(ctx, t, e, p);
				(e.etype.equals(t)) ? e : core.Type.mk(TCast(e, None), t, p);
			case EMeta(m, e1):
				if (ctx.is_display_file) {
					context.display.DisplayEmitter.check_display_metadata(ctx, [m]);
				}
				var old = ctx.meta;
				ctx.meta = m :: ctx.meta;
				function e_ () {
					return type_expr(ctx, e1, with_type);
				}
				var e = switch (m) {
					case {name:ToString}:
						var e = e_();
						switch (core.Type.follow(e.etype)) {
							case TAbstract({a_impl:Some(c)}, _) if (PMap.mem("toString", c.cl_statics)): call_to_string(ctx, e);
							case _: e;
						}
					case {name:This}:
						var e = switch (ctx.this_stack) {
							case []: core.Error.error("Cannot type @:this this here", p);
							case e::_: e_();
						}
						function loop(e:core.Type.TExpr) : core.Type.TExpr {
							return switch (e.eexpr) {
								case TConst(TThis): get_this(ctx, e.epos);
								case _: core.Type.map_expr(loop, e);
							}
						}
						loop(e);
					case {name:Analyzer}:
						var e = e_();
						e.eexpr = TMeta(m, e);
						e;
					case {name:MergeBlock}:
						switch (e1.expr) {
							case EBlock(el):
								var e = type_block(ctx, el, with_type, p);
								e.eexpr = TMeta(m, e);
								e;
							case _: e_();
						}
					case {name:StoredTypedExpr}:
						var id = switch (e1) {
							case {expr:EConst(CIdent(s))}: Std.parseInt(s);
							case _: trace("Shall not be seen"); throw false;
						}
						typing.MacroContext.get_stored_typed_expr(ctx.com, id);
					case {name:NoPrivateAccess}:
						ctx.meta = List.filter(function (meta:core.Ast.MetadataEntry) {
							var m = meta.name;
							return m != PrivateAccess;
						}, ctx.meta);
						e_();
					case {name:Fixed} if (ctx.com.platform == Cpp):
						var e = e_();
						e.eexpr = TMeta(m, e);
						e;
					case _: e_();
				}
				e;
		}
	}

	public static function handle_display (ctx:context.Typecore.Typer, e_ast:core.Ast.Expr, with_type:context.Typecore.WithType) : core.Type.TExpr {
		trace("TODO: typing.Typer.handle_dislpay");
		throw false;
	}

	public static function handle_signature_display (ctx:context.Typecore.Typer, e_ast:core.Ast.Expr, with_type:context.Typecore.WithType) : core.Type.TExpr {
		trace("TODO: typing.Typer.handle_signature_dislpay");
		throw false;
	}

	public static function display_expr (ctx:context.Typecore.Typer, e_ast:core.Ast.Expr, e:core.Type.TExpr, with_type:context.Typecore.WithType, p:core.Globals.Pos) : Void {
		trace("TODO: display_expr");
		throw false;
	}

	public static function maybe_type_against_enum (ctx:context.Typecore.Typer, f:Void->AccessKind, with_type:context.Typecore.WithType, iscall:Bool, p:core.Globals.Pos) : AccessKind {
		return try {
			switch (with_type) {
				case WithType(t):
					function loop (stack, t:core.Type.T) : {fst:core.Path, snd:ImmutableList<String>, trd:core.Type.ModuleType} {
						return switch (core.Type.follow(t)) {
							case TEnum(en, _):
								{fst:en.e_path, snd:en.e_names, trd:TEnumDecl(en)};
							case TAbstract(a={a_impl:Some(c)}, _) if (core.Type.has_meta(Enum, a.a_meta)):
								var fields = List.filter_map(function (cf:core.Type.TClassField) {
									return (core.Meta.has(Enum, cf.cf_meta)) ? Some(cf.cf_name) : None;
								}, c.cl_ordered_statics);
								{fst:a.a_path, snd:fields, trd:TAbstractDecl(a)};
							case TAbstract(a, pl) if (!core.Meta.has(CoreType, a.a_meta)):
								switch (get_abstract_froms(a, pl)) {
									case [t2]:
										if (List.exists(core.Type.fast_eq.bind(t), stack)) {
											throw ocaml.Exit.instance;
										}
										loop(t::stack, t2);
									case _: throw ocaml.Exit.instance;
								}
							case _: throw ocaml.Exit.instance;
						}
					}
					var _tmp = loop([], t);
					var path = _tmp.fst; var fields = _tmp.snd; var mt = _tmp.trd;
					var old = ctx.m.curmod.m_types;
					function restore() { ctx.m.curmod.m_types = old; }
					ctx.m.curmod.m_types = List.append(ctx.m.curmod.m_types, [mt]);
					var e:AccessKind = try {
						f();
					}
					catch (exc:core.Error) {
						switch (exc.msg) {
							case Unknown_ident(n):
								restore();
								context.Typecore.raise_or_display_message(ctx, core.type.StringError.string_error(n, fields, "Identifier '"+n+"' is not part of " + core.Globals.s_type_path(path)), p);
								AKExpr(core.Type.mk(TConst(TNull), core.Type.mk_mono(), p));
							case _:
								restore();
								throw exc;
						}
					}
					catch (exc:Any) {
						restore();
						throw exc;
					}
					restore();
					switch (e) {
						case AKExpr(e):
							switch (core.Type.follow(e.etype)) {
								case TFun({args:_, ret:t_}):
									context.Typecore.unify(ctx, t_, t, e.epos);
									AKExpr(e);
								case _:
									if (iscall) {
										AKExpr(e);
									}
									else {
										AKExpr(context.typecore.AbstractCast.cast_or_unify_raise(ctx, t, e, e.epos));
									}
							}
						case _: e; // ???
					}
				case _: throw ocaml.Exit.instance;
			}
		}
		catch (_:ocaml.Exit) {
			f();
		}
	}

	public static function type_call (ctx:context.Typecore.Typer, e:core.Ast.Expr, el:ImmutableList<core.Ast.Expr>, with_type:context.Typecore.WithType, p:core.Globals.Pos) : core.Type.TExpr {
		function def() : core.Type.TExpr {
			var e = maybe_type_against_enum(ctx, function () { return type_access(ctx, e.expr, e.pos, MCall); }, with_type, true, p);
			return build_call(ctx, e, el, with_type, p);
		}
		return switch [e, el] {
			case [{expr:EConst(CIdent("trace")), pos:p}, e::el]:
				if (context.Common.defined(ctx.com, NoTraces)) {
					core.Type.null_(ctx.t.tvoid, p);
				}
				else {
					function mk_to_string_meta(e:core.Ast.Expr) : core.Ast.Expr {
						return {expr:EMeta({name:ToString, params:[], pos:core.Globals.null_pos}, e), pos:e.pos}
					}
					var params:ImmutableList<core.Ast.ObjectField> = switch (el) {
						case []: [];
						case _: [{name:"customParams", pos:core.Globals.null_pos, quotes:NoQuotes, expr:{expr:EArrayDecl(List.map(mk_to_string_meta,el)), pos:p}}];
					}
					var infos = mk_infos(ctx, p, params);
					if ((context.Common.platform(ctx.com, Js) || context.Common.platform(ctx.com, Python)) && el == Tl && context.Common.has_dce(ctx.com)) {
						var e = type_expr(ctx, e, Value);
						var infos = type_expr(ctx, infos, Value);
						var e = switch (core.Type.follow(e.etype)) {
							case TAbstract({a_impl:Some(c)}, _) if (PMap.mem("toString", c.cl_statics)):
								call_to_string(ctx, e);
							case _: e;
						}
						var e_trace = core.Type.mk(TIdent("`trace"), core.Type.t_dynamic, p);
						core.Type.mk(TCall(e_trace, [e, infos]), ctx.t.tvoid, p);
					}
					else {
						type_expr(ctx, {
								expr:ECall(
									{expr:EField({expr:EField({expr:EConst(CIdent("haxe")), pos:p}, "Log"), pos:p}, "trace"), pos:p},
									[mk_to_string_meta(e), infos]),
								pos:p
							}, NoValue);
					}
				}
			case [{expr:EField({expr:EConst(CIdent("super"))}, _)}, _]:
				def();
			case [{expr:EField(e, "bind"), pos:p}, args]:
				var e = type_expr(ctx, e, Value);
				switch (core.Type.follow(e.etype)) {
					case TFun(signature): type_bind(ctx, e, signature, args, p);
					case _: def();
				}
			case [{expr:EConst(CIdent("$type"))}, [e]]:
				var e = type_expr(ctx, e, Value);
				ctx.com.warning(core.Type.s_type(core.Type.print_context(), e.etype), e.epos);
				context.display.Diagnostics.secure_generated_code(ctx, e);
			case [{expr:EField(e, "match"), pos:p}, [epat]]:
				var et = type_expr(ctx, e, Value);
				switch (core.Type.follow(et.etype)) {
					case TEnum(_):
						var e = typing.matcher.Match.match_expr(ctx, e, [{values:[epat], guard:None, expr:Some({expr:core.Ast.ExprDef.EConst(CIdent("true")), pos:p}), pos: p}], Some({e:Some({expr:EConst(CIdent("false")), pos:p}), pos:p}), WithType(ctx.t.tbool), p);
						// TODO: add that back
						/* let locals = !get_pattern_locals_ref ctx epat t in
						PMap.iter (fun _ (_,p) -> display_error ctx "Capture variables are not allowed" p) locals; */
						e;
					case _:
						def();
				}
			case [{expr:EConst(CIdent("__unprotect__"))}, [e={expr:EConst(CString(_))}]]:
				var e = type_expr(ctx, e, Value);
				if (context.Common.platform(ctx.com, Flash)) {
					var t = core.Type.tfun([e.etype], e.etype);
					var e_unprotect = core.Type.mk(TIdent("__unprotect__"), t, p);
					core.Type.mk(TCall(e_unprotect, [e]), e.etype, e.epos);
				}
				else {
					e;
				}
			case [{expr:EDisplay(e1={expr:EConst(CIdent("super"))}, false)}, _]:
				handle_display(ctx, {expr:ECall(e1, el), pos:p}, with_type);
			case [{expr:EConst(CIdent("super")), pos:sp}, el]:
				if (ctx.curfun != FunConstructor) {
					core.Error.error("Cannot call super constructor outside class constructor", p);
				}
				var _tmp = switch (ctx.curclass.cl_super) {
					case None: core.Error.error("Current class does not have a super", p);
					case Some({c:c, params:params}):
						var _tmp = get_constructor(ctx,c, params, p);
						var ct = _tmp.t; var f = _tmp.cf;
						if (core.Meta.has(CompilerGenerated, f.cf_meta)) {
							context.Typecore.display_error(ctx, core.Error.error_msg(No_constructor(TClassDecl(c))), p);
						}
						var el = switch (core.Type.follow(ct)) {
							case TFun({args:args, ret:r}):
								var el = unify_field_call(ctx, FInstance(c, params, f), el, args, r, p, false).fst;
								el;
							case _:
								core.Error.error("Constructor is not a function", p);
						}
						{fst:el, snd:core.Type.T.TInst(c, params)};
				}
				var el = _tmp.fst; var t = _tmp.snd;
				core.Type.mk(TCall(core.Type.mk(TConst(TSuper), t, sp), el), ctx.t.tvoid, p);
			case _:
				def();
		}
	}

	public static function build_call (ctx:context.Typecore.Typer, acc, el:ImmutableList<core.Ast.Expr>, with_type:context.Typecore.WithType, p:core.Globals.Pos) : core.Type.TExpr {
		return switch (acc) {
			case AKInline(ethis, f, fmode, t) if (core.Meta.has(Generic, f.cf_meta)):
				type_generic_function(ctx, {fst:ethis, snd:fmode}, el, with_type, p);
			case AKInline(ethis, f, fmode, t):
				switch (core.Type.follow(t)) {
					case TFun({args:args, ret:r}):
						var mk_call = unify_field_call(ctx, fmode, el, args, r, p, true).trd;
						mk_call(ethis, p);
					case _:
						core.Error.error(core.Type.s_type(core.Type.print_context(), t) + " cannot be called", p);
				}
			case AKUsing(et, cl, ef, eparam) if (core.Meta.has(Generic, ef.cf_meta)):
				switch (et.eexpr) {
					case TField(ec, fa):
						type_generic_function(ctx, {fst:ec, snd:fa}, el, Some(eparam), with_type, p);
					case _: trace("Shall not be seen"); throw false;
				}
			case AKUsing(et, cl, ef, eparam):
				switch (ef.cf_kind) {
					case Method(MethMacro):
						var ethis = type_module_type(ctx, TClassDecl(cl), None, p);
						var _tmp = context.Typecore.push_this(ctx, eparam);
						var eparam = _tmp.fst; var f = _tmp.snd;
						var e = build_call(ctx, AKMacro(ethis, ef), (eparam::el), with_type, p);
						f();
						e;
					case _:
						var t = core.Type.follow(field_type(ctx, cl, [], ef, p));
						// for abstracts we have to apply their parameters to the static function
						var _tmp = switch (core.Type.follow(eparam.etype)) {
							case TAbstract(a, tl) if (core.Meta.has(Impl, ef.cf_meta)):
								{fst:core.Type.apply_params(a.a_params, tl, t), snd:core.Type.apply_params(a.a_params, tl, a.a_this)};
							case te: {fst:t, snd:te};
						}
						var t = _tmp.fst; var tthis = _tmp.snd;
						var _tmp = switch (t) {
							case TFun({args:{t:t1}::args, ret:r}):
								context.Typecore.unify(ctx, tthis, t1, eparam.epos);
								var ef = context.Typecore.prepare_using_field(ef);
								switch (unify_call_args(ctx, el, args, r, p, (ef.cf_kind.equals(Method(MethInline))), is_forced_inline(Some(cl), ef))) {
									case {fst:el, snd:TFun({args:args, ret:r})}: {fst:el, snd:args, trd:r, frth:eparam};
									case _: trace("Shall not be seen"); throw false;
								}
							case _: trace("Shall not be seen"); throw false;
						}
						var params = _tmp.fst; var args = _tmp.snd; var r = _tmp.trd; var eparam = _tmp.frth;
						make_call(ctx, et, (eparam::params), r, p);

				}
			case AKMacro(ethis, cf):
				if (ctx.macro_depth > 300) {
					core.Error.error("Stack overflow", p);
				}
				ctx.macro_depth++;
				ctx.with_type_stack = with_type :: ctx.with_type_stack;
				var ethis_f = new Ref<Void->Void>(function () {});
				var f = switch (ethis.eexpr) {
					case TTypeExpr(TClassDecl(c)):
						switch (ctx.g.do_macro(ctx, MExpr, c.cl_path, cf.cf_name, el, p)) {
							case None: function () { return type_expr(ctx, {expr:EConst(CIdent("null")), pos:p}, Value); };
							case Some({expr:EMeta({name:MergeBlock}, {expr:EBlock(el)})}): function () {
								var e = type_block(ctx, el, with_type, p);
								return  core.Type.mk(TMeta({name:MergeBlock, params:[], pos:p}, e), e.etype, e.epos);
							};
							case Some(e): function () { return type_expr(ctx, e, with_type); };
						}
					case _: // member-macro call : since we will make a static call, let's found the actual class and not its subclass
						switch (core.Type.follow(ethis.etype)) {
							case TInst(c, _):
								function loop (c:core.Type.TClass) {
									if (PMap.mem(cf.cf_name, c.cl_fields)) {
										var _tmp = context.Typecore.push_this(ctx, ethis);
										var eparam = _tmp.fst; var f = _tmp.snd;
										ethis_f.set(f);
										var e = switch (ctx.g.do_macro(ctx, MExpr, c.cl_path, cf.cf_name, eparam::el, p)) {
											case None: function () { return type_expr(ctx, {expr:EConst(CIdent("null")), pos:p}, Value); };
											case Some(e): function () { return type_expr(ctx, e, Value); };
										}
										return e;
									}
									else {
										return switch (c.cl_super) {
											case None: trace("Shall not be seen"); throw false;
											case Some({c:csup}): loop(csup);
										}
									}
								}
								loop(c);
							case _: trace("Shall not be seen"); throw false;
						}
				}
				ctx.macro_depth--;
				ctx.with_type_stack = List.tl(ctx.with_type_stack);
				var old = ctx.on_error;
				ctx.on_error = function (ctx, msg, ep) {
					// display additional info in the case the error is not part of our original call
					if (ep.pfile != p.pfile || ep.pmax < p.pmin || ep.pmin > p.pmax) {
						typing.Typeload.locate_macro_error.set(false);
						old(ctx, msg, ep);
						typing.Typeload.locate_macro_error.set(true);
						ctx.com.error("Called from macro here", p);
					}
					else {
						old(ctx, msg, ep);
					}
				}
				var e = try { f(); }
				catch (exc:core.Error) {
					var m =exc.msg; var p = exc.pos;
					ctx.on_error = old;
					ethis_f.get()();
					throw new core.Error.Fatal_error(core.Error.error_msg(m), p);
				}
				var e = context.display.Diagnostics.secure_generated_code(ctx, e);
				ctx.on_error = old;
				ethis_f.get()();
				e;
			case AKNo(_), AKSet(_), AKAccess(_):
				acc_get(ctx, acc, p);
				trace("Shall not be seen"); throw false;
			case AKExpr(e):
				function loop (t:core.Type.T) {
					return switch (core.Type.follow(t)) {
						case TFun({args:args, ret:r}):
							switch (e.eexpr) {
								case TField(e1, fa) if (!fa.match(FEnum(_))):
									switch (fa) {
										case FInstance(_, _, cf), FStatic(_, cf) if (core.Meta.has(Generic, cf.cf_meta)):
											type_generic_function(ctx, {fst:e1, snd:fa}, el, with_type, p);
										case _:
											var mk_call = unify_field_call(ctx, fa, el, args, r, p, false).trd;
											mk_call(e1, e.epos);
									}
								case _:
									var _tmp = unify_call_args(ctx, el, args, r, p, false, false);
									var el = _tmp.fst; var tfunc = _tmp.snd;
									var r = switch (tfunc) { case TFun({ret:r}): r; case _: trace("Shall not be seen"); throw false; };
									core.Type.mk(TCall(e, el), r, p);
							}
						case TAbstract(a, tl) if (core.Meta.has(Callable, a.a_meta)):
							loop(core.Abstract.get_underlying_type(a, tl));
						case TMono(_):
							var t = core.Type.mk_mono();
							var el = List.map(function (e) { return type_expr(ctx, e, Value); }, el);
							context.Typecore.unify(ctx, core.Type.tfun(List.map(function (e) { return e.etype; }, el), t), e.etype, e.epos);
							core.Type.mk(TCall(e, el), t, p);
						case t:
							var el = List.map(function (e) { return type_expr(ctx, e, Value); }, el);
							var t = if (t.equals(core.Type.t_dynamic)) {
								core.Type.t_dynamic;
							}
							else if (ctx.untyped_) {
								core.Type.mk_mono();
							}
							else {
								core.Error.error(core.Type.s_type(core.Type.print_context(), e.etype)+" cannot be called", e.epos);
							}
							core.Type.mk(TCall(e, el), t, p);
					}
				}
				loop(e.etype);
		}
	}
	// ----------------------------------------------------------------------
	// FINALIZATION

	public static function get_main (ctx:context.Typecore.Typer, types:ImmutableList<core.Type.ModuleType>) : Option<core.Type.TExpr> {
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
						default: trace("Shall not be seen"); throw false;
					}
					var ef = PMap.find("run", ec.cl_statics);
					var p = core.Globals.null_pos;
					var _et = core.Type.mk(
						core.Type.TExprExpr.TTypeExpr(et),
						core.Type.T.TAnon({a_fields: PMap.empty(), a_status: new ocaml.Ref(core.Type.AnonStatus.Statics(ec))}),
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
		throw false;
	}

	public static function sort_types(com:context.Common.Context, modules:Hashtbl<core.Path, core.Type.ModuleDef>) : {types:ImmutableList<core.Type.ModuleType>, modules:ImmutableList<core.Type.ModuleDef>} {
		// var types = new Ref<ImmutableList<core.Type.ModuleType>>([]);
		var types:ImmutableList<core.Type.ModuleType> = [];
		var states = new Hashtbl<core.Path, typing.Typer.State>();

		function state (p:core.Path) {
			if (states.exists(p)) {
				return states.get(p);
			}
			return NotYet;
		}
		var statics:PMap<{path:core.Path, s:String}, Bool> = PMap.empty();

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
						case TTypeDecl(_): trace("Shall not be seen"); throw false;
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
				modules : new Hashtbl<core.Path, core.Type.ModuleDef>(),
				types_module : new Hashtbl<core.Path, core.Path>(),
				type_patches : new Hashtbl<core.Path, {map:Hashtbl<{s:String, b:Bool}, context.Typecore.TypePatch>, tp:context.Typecore.TypePatch}>(),
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
				module_globals : PMap.empty(), // <String, {a:core.Type.ModuleType, b:String, pos:core.Globals.Pos}>
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
			locals : PMap.empty(), // <String, core.Type.TVar>
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
			case _: trace("Shall not be seen"); throw false;
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
			trace("Shall not be seen"); throw false;
		}
		catch (_:ocaml.Exit) {}

		m = typing.Typeload.load_module(ctx, new core.Path(["haxe"], "EnumTools"), core.Globals.null_pos);
		switch (m.m_types) {
			case [TClassDecl(c1), TClassDecl(c2)]:
				ctx.g.global_using = {tc:c1, pos:c1.cl_pos} :: ({tc:c2, pos:c2.cl_pos} :: ctx.g.global_using);
			case [TClassDecl(c1)]:
				var m = typing.Typeload.load_module(ctx, new core.Path(["haxe"], "EnumValueTools"), core.Globals.null_pos);
				switch (m.m_types) {
					case [TClassDecl(c2)]:
						ctx.g.global_using = {tc:c1, pos:c1.cl_pos} :: ({tc:c2, pos:c2.cl_pos} :: ctx.g.global_using);
					case _: trace("Shall not be seen"); throw false;
				}
			case _: trace("Shall not be seen"); throw false;
		}
		return ctx;
	}
}