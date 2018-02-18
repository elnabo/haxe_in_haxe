package typing;

import haxe.Utf8;
import haxe.ds.ImmutableList;
import haxe.ds.Option;
import ocaml.Hashtbl;
import ocaml.List;
import ocaml.PMap;
import ocaml.Ref;

using equals.Equal;
// using ocaml.Cloner;

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
		trace("TODO: unify_min_raise");
		throw false;
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

	public static function unify_field_call (ctx:context.Typecore.Typer, fa:core.Type.TFieldAccess, el:ImmutableList<core.Ast.Expr>, args:ImmutableList<core.Type.TSignatureArg>, ret:core.Type.T, p:core.Globals.Pos, _inline:Bool) : {fst:Dynamic, snd:Dynamic, trd:ImmutableList<core.Type.TExpr>} {
		trace("TODO: typing.Typer.unify_field_call");
		throw false;
	}

	public static function type_type (ctx:context.Typecore.Typer, tpath:core.Path, p:core.Globals.Pos) : Dynamic {
		trace("TODO: typing.Typer.type_type");
		throw false;
		// return type_module_type ctx (Typeload.load_type_def ctx p { tpackage = fst tpath; tname = snd tpath; tparams = []; tsub = None }) None p
	}

	public static function get_constructor (ctx:context.Typecore.Typer, c:core.Type.TClass, params:core.Type.TParams, p:core.Globals.Pos) : {fst:core.Type.T, snd:core.Type.TClassField} {
		trace("TODO: typing.Typer.get_constructor");
		throw false;
	}

	public static function acc_get(ctx:context.Typecore.Typer, g:AccessKind, p:core.Globals.Pos) : core.Type.TExpr {
		trace("TODO: typing.Typer.acc_get");
		throw false;
	}

	public static function type_field (?resume:Bool=false, ctx:context.Typecore.Typer, e:core.Type.TExpr, i:String, p:core.Globals.Pos, mode:AccessMode) : Dynamic {
		trace("TODO: type_field");
		throw false;
	}

	public static function type_bind (ctx:context.Typecore.Typer, e:core.Type.TExpr, sig:core.Type.TSignature, params, p:core.Globals.Pos) : core.Type.TExpr {
		var args = sig.args; var ret = sig.ret;
		trace("TODO: type_bind");
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
		trace("TODO: typing.Typer.type_binop");
		throw false;
	}

	public static function type_ident(ctx:context.Typecore.Typer, i:String, p:core.Globals.Pos, mode:AccessMode) : AccessKind {
		trace("TODO: typing.Typer.type_ident");
		throw false;
	}
	
	// MORDOR

	public static function type_access (ctx:context.Typecore.Typer, e:core.Ast.ExprDef, p:core.Globals.Pos, mode:AccessMode) : AccessKind {
		trace("TODO: typing.Typer.type_access");
		throw false;
	}

	public static function type_vars (ctx:context.Typecore.Typer, vl:ImmutableList<core.Ast.Var>, p:core.Globals.Pos) : core.Type.TExpr {
		trace("TODO: typing.Typer.type_vars");
		throw false;
	}

	public static function type_block (ctx:context.Typecore.Typer, el:ImmutableList<core.Ast.Expr>, with_type:context.Typecore.WithType, p:core.Globals.Pos) : core.Type.TExpr {
		trace("TODO: typing.Typer.type_block");
		throw false;
	}

	public static function type_object_decl (ctx:context.Typecore.Typer, fl:ImmutableList<core.Ast.ObjectField>, with_type:context.Typecore.WithType, p:core.Globals.Pos) : core.Type.TExpr {
		trace("TODO: typing.Typer.type_object_decl");
		throw false;
	}

	public static function type_map_declaration (ctx:context.Typecore.Typer, e1:core.Ast.Expr, el:ImmutableList<core.Ast.Expr>, with_type:context.Typecore.WithType, p:core.Globals.Pos) : core.Type.TExpr {
		trace("TODO: typing.Typer.type_map_declaration");
		throw false;
	}

	public static function type_new (ctx:context.Typecore.Typer, t:core.Ast.PlacedTypePath, el:ImmutableList<core.Ast.Expr>, with_type:context.Typecore.WithType, p:core.Globals.Pos) : core.Type.TExpr {
		trace("TODO: typing.Typer.type_try");
		throw false;
	}

	public static function type_try (ctx:context.Typecore.Typer, e1:core.Ast.Expr, catches:ImmutableList<core.Ast.Catch>, with_type:context.Typecore.WithType, p:core.Globals.Pos) : core.Type.TExpr {
		trace("TODO: typing.Typer.type_try");
		throw false;
	}

	public static function type_array_decl (ctx:context.Typecore.Typer, el:ImmutableList<core.Ast.Expr>, with_type:context.Typecore.WithType, p:core.Globals.Pos) : core.Type.TExpr {
		trace("TODO: typing.Typer.type_array_declaration");
		throw false;
	}

	public static function type_expr (ctx:context.Typecore.Typer, expr:core.Ast.Expr, with_type:context.Typecore.WithType) : core.Type.TExpr {
		trace("TODO: typing.Typer.type_expr");
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
				core.Type.mk(TNew(switch (t) { case TInst(c, []): c; case _: throw false; }, [], [str, opt]), t, p);
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
						case TLazy(_): throw false;
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
			case _:
				trace("TODO: finish", e);
				throw false;
		}
	}

	public static function handle_display (ctx:context.Typecore.Typer, e_ast:core.Ast.Expr, with_type:context.Typecore.WithType) : core.Type.TExpr {
		trace("TODO: typing.Typer.handle_dislpay");
		throw false;
	}

	public static function maybe_type_against_enum (ctx:context.Typecore.Typer, f:Void->AccessKind, with_type:context.Typecore.WithType, iscall:Bool, p:core.Globals.Pos) : AccessKind {
		trace("TODO: typing.Typer.maybe_type_against_enum");
		throw false;
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
						var ct = _tmp.fst; var f = _tmp.snd;
						if (core.Meta.has(CompilerGenerated, f.cf_meta)) {
							context.Typecore.display_error(ctx, core.Error.error_msg(No_constructor(TClassDecl(c))), p);
						}
						var el = switch (core.Type.follow(ct)) {
							case TFun({args:args, ret:r}):
								var el = unify_field_call(ctx, FInstance(c, params, f), el, args, r, p, false).trd;
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
		trace("TODO: build_call");
		throw false;
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