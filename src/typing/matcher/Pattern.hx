package typing.matcher;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
import ocaml.List;
import ocaml.PMap;

using ocaml.Cloner;

typedef Pattern_context = {
	ctx:context.Typecore.Typer,
	or_locals: Option<Map<String, {fst:core.Type.TVar, snd:core.Globals.Pos}>>,
	ctx_locals:Map<String, core.Type.TVar>,
	current_locals: Map<String, {fst:core.Type.TVar, snd:core.Globals.Pos}>,
	in_reification:Bool
}

class Bad_pattern {
	public var msg:String;
	public function new(msg:String) {
		this.msg = msg;
	}
}

@:structInit
class Pattern {
	public var t:typing.matcher.pattern.T;
	public var pos:core.Globals.Pos;

	public static function to_string (pat:Pattern) : String {
		return switch (pat.t) {
			case PatConstructor(con,patterns): '${Constructor.to_string(con)}(${List.join(", ", List.map(to_string, patterns))})';
			case PatVariable(v): '${v.v_name}<${v.v_id}>';
			case PatAny: "_";
			case PatBind(v,pat1): '${v.v_name} = ${to_string(pat1)}';
			case PatOr(pat1,pat2): '(${to_string(pat1)}) | (${to_string(pat2)})';
			case PatTuple(pl): '[${List.join(", ", List.map(to_string, pl))}]';
			case PatExtractor(v,e,pat1):
				'${typing.Matcher.s_expr_pretty(e)} => ${to_string(pat1)}';
		}
	}

	public static function unify_type_pattern (ctx:context.Typecore.Typer, mt:core.Type.ModuleType, t:core.Type.T, p:core.Globals.Pos) {
		var tcl = typing.Matcher.get_general_module_type(ctx, mt, p);
		switch (tcl) {
			case TAbstract(a, _):
				context.Typecore.unify(ctx, TAbstract(a, [core.Type.mk_mono()]), t, p);
			case _:
				trace("Shall not be seen"); throw false;
		}
	}

	public static function make_(pctx:Pattern_context, toplevel:Bool, t:core.Type.T, e:core.Ast.Expr) : Pattern {
		var ctx = pctx.ctx;
		var p = e.pos;
		function fail () : Dynamic {
			return core.Error.error("Unrecognized pattern: "+core.Ast.s_expr(e), p);
		}
		function unify_expected(t_:core.Type.T) : Void {
			context.Typecore.unify(ctx, t_, t, p);
		}
		function verror (name:String, p:core.Globals.Pos) : Void {
			core.Error.error('Variable ${name} must appear exactly once in each sub-pattern', p);
		}
		function add_local (name:String, p:core.Globals.Pos) {
			var is_wildcard_local = name == "_";
			if (!is_wildcard_local && PMap.mem(name, pctx.current_locals)) {
				core.Error.error('Variable ${name} is bound multiple times', p);
			}
			return
			switch (pctx.or_locals) {
				case Some(map) if (!is_wildcard_local):
					var _tmp = try {
						PMap.find(name, map);
					}
					catch (_:ocaml.Not_found) {
						verror(name, p);
						trace("Shall not be seen"); throw false; // Should never be reached
					}
					var v = _tmp.fst; var p = _tmp.snd;
					context.Typecore.unify(ctx, t, v.v_type, p);
					pctx.current_locals = PMap.add(name, {fst:v, snd:p}, pctx.current_locals);
					v;
				case _:
					var v = core.Type.alloc_var(name, t, p);
					pctx.current_locals = PMap.add(name, {fst:v, snd:p}, pctx.current_locals);
					ctx.locals = PMap.add(name, v, ctx.locals);
					v;
			}
		}
		function con_enum (en:core.Type.TEnum, ef:core.Type.TEnumField, p:core.Globals.Pos) : typing.matcher.constructor.T {
			context.display.DeprecationCheck.check_enum(pctx.ctx.com, en, p);
			context.display.DeprecationCheck.check_ef(pctx.ctx.com, ef, p);
			return ConEnum(en, ef);
		}
		function check_expr (e:core.Type.TExpr) : typing.matcher.pattern.T {
			function loop(e:core.Type.TExpr) : typing.matcher.pattern.T {
				return switch (e.eexpr) {
					case TField(_, FEnum(en, ef)):
						// Let the unification afterwards fail so we don't recover.
						// (match follow ef.ef_type with TFun _ -> raise Exit | _ -> ());
						PatConstructor(con_enum(en, ef, e.epos), []);
					case TField(_, FStatic(c, cf={cf_kind:Var({v_write:AccNever})})):
						PatConstructor(ConStatic(c, cf), []);
					case TConst(ct):
						PatConstructor(ConConst(ct), []);
					case TCast(e1, None):
						loop(e1);
					case TField(_, _):
						throw new Bad_pattern("Only inline or read only (default, never) fields can beused as a pattern");
					case _:
						throw ocaml.Exit.instance;
				}
			}
			return loop(e);
		}
		function try_typing (e:core.Ast.Expr) : typing.matcher.pattern.T {
			var old = ctx.untyped_;
			ctx.untyped_ = true;
			var e = try {
				typing.Typer.type_expr(ctx, e, WithType(t));
			}
			catch (_:Bool) {
				trace("Shall not be seen"); throw false;
			}
			catch (exc:Any) {
				ctx.untyped_ = old;
				throw exc;
			}
			return switch (e.eexpr) {
				case TTypeExpr(mt):
					unify_type_pattern(ctx, mt, t, e.epos);
					PatConstructor(ConTypeExpr(mt), []);
				case _:
					var pat = check_expr(e);
					try {
						core.Type.unify(e.etype, t);
					}
					catch (ue:core.Type.Unify_error) {
						var l = ue.l;
						// Hack: Allow matching the underlying type against its abstract.
						switch (core.Type.follow(e.etype)) {
							case TAbstract(a, tl) if (!core.Meta.has(CoreType, a.a_meta) && core.Type.type_iseq(t, core.Abstract.get_underlying_type(a, tl))):
							case _: context.Typecore.raise_or_display(ctx, l, p);
						}
					}
					pat;
			}
		}
		function handle_ident (s:String, p:core.Globals.Pos) {
			var save = {
				var old = ctx.locals;
				ctx.locals = try {
					PMap.add("this", PMap.find("this", old), new Map<String, core.Type.TVar>());
				}
				catch (_:ocaml.Not_found) {
					new Map<String, core.Type.TVar>();
				}
				function () { ctx.locals = old; }
			}
			try {
				var pat = try_typing({expr:EConst(CIdent(s)), pos:p});
				save();
				return pat;
			}
			catch (_:ocaml.Exit) {}
			catch (_:Bad_pattern) {}
			catch (_:Bool) { trace("Shall not be seen"); throw false; }
			catch (exc:Any) { save(); throw exc;}
			return
			try {
				var mt = core.Type.module_type_of_type(t);
				var e_mt = typing.Typer.type_module_type(ctx, mt, None, p);
				var e = typing.Matcher.type_field_access(ctx, true, e_mt, s);
				var pat = check_expr(e);
				save();
				pat;
			}
			catch (_:Bool) { trace("Shall not be seen"); throw false; }
			catch (_:Any) {
				save();
				if (!core.Ast.is_lower_ident(s) && (s.charAt(0) != "`" && s.charAt(0) != "_")) {
					context.Typecore.display_error(ctx, "Capture variables must be lower-case",  p);
				}
				var sl:ImmutableList<String> = switch (core.Type.follow(t)) {
					case TEnum(en, _):
						en.e_names;
					case TAbstract(a={a_impl:Some(c)}, pl) if (core.Meta.has(Enum, a.a_meta)):
						List.filter_map(function (cf) {
							return
							if (core.Meta.has(Impl, cf.cf_meta) && core.Meta.has(Enum, cf.cf_meta)) {
								Some(cf.cf_name);
							}
							else {
								None;
							}
						}, c.cl_ordered_statics);
					case _: [];
				}
				switch (core.type.StringError.get_similar(s, sl)) {
					case []:
						/* if toplevel then
							pctx.ctx.com.warning (Printf.sprintf "`case %s` has been deprecated, use `case var %s` instead" s s) p */
					case l:
						pctx.ctx.com.warning("Potential typo detected (expected similar values are " + List.join(", ", l) + "). Consider using `var " + s + "` instead", p);
				}
				var v = add_local(s, p);
				PatVariable(v);
			}
		}
		function loop (e:core.Ast.Expr) : typing.matcher.pattern.T {
			return switch (e.expr) {
				case EParenthesis(e1), ECast(e1, None):
					loop(e1);
				case ECheckType(e, {ct:CTPath({tpackage:["haxe", "macro"], tname:"Expr"})}):
					var old = pctx.in_reification;
					pctx.in_reification = true;
					var e = loop(e);
					pctx.in_reification = old;
					e;
				case EConst(ct=((CIdent(("false"|"true"))) | CInt(_) | CString(_) | CFloat(_))):
					var e = core.Texpr.type_constant(ctx.com.basic, ct, p);
					unify_expected(e.etype);
					var ct = switch (e.eexpr) {
						case TConst(ct): ct;
						case _: trace("Shall not be seen"); throw false;
					}
					PatConstructor(ConConst(ct), []);
				case EConst(CIdent(i)):
					switch (i) {
						case "_":
							switch (core.Type.follow(t)) {
								case TFun({args:ta, ret:tr}) if (tr.equals(typing.Matcher.fake_tuple_type)):
									PatTuple(List.map(function (arg) : Pattern {
										var t = arg.t;
										return {t:PatAny, pos:e.pos};
									}, ta));
								case _: PatAny;
							}
						case _:
							handle_ident(i, e.pos);
					}
				case EVars([{name:{pack:s, pos:p}, type:None, expr:None}]):
					var v = add_local(s,p);
					PatVariable(v);
				case ECall(e1, el):
					var e1 = typing.Typer.type_expr(ctx, e1, WithType(t));
					switch [e1.eexpr, core.Type.follow(e1.etype)] {
						case [TField(_, FEnum(en, ef)), TFun({ret:TEnum(_, tl)})]:
							var monos = List.map(function(_) { return core.Type.mk_mono(); }, ef.ef_params);
							function map(t) {
								return core.Type.apply_params(en.e_params, tl, core.Type.apply_params(ef.ef_params, monos, t));
							}
							// We cannot use e1.etype here because it has applied type parameters (issue #1310).
							var args = switch (core.Type.follow(map(ef.ef_type))) {
								case TFun({args:args, ret:r}):
									unify_expected(r);
									args;
								case _: trace("Shall not be seen"); throw false;
							}
							function loop_ (el:ImmutableList<core.Ast.Expr>, tl:ImmutableList<core.Type.TSignatureArg>) : ImmutableList<Pattern> {
								return switch [el, tl] {
									case [[{expr:EConst(CIdent("_")), pos:p}], {t:t}::tl]:
										// Allow using final _ to match "multiple" arguments
										({t:PatAny, pos:p} : Pattern) :: {switch (tl) { case []: []; case _: loop_(el, tl); }};
									case [e::el, {t:t}::tl]:
										make_(pctx, false, t, e)::loop_(el, tl);
									case [[], {opt:true, t:t}::tl]:
										({t:PatAny, pos:e.pos} : Pattern) :: loop_([], tl);
									case [[], []]:
										[];
									case [[], _]:
										core.Error.error("Not enough arguments", p);
									case [_, []]:
										core.Error.error("Too many arguments", p);
								}
							}
							var patterns = loop_(el, args);
							/* We want to change the original monomorphs back to type parameters, but we don't want to do that
								if they are bound to other monomorphs (issue #4578). */
							typing.Matcher.unapply_type_parameters(ef.ef_params, monos);
							PatConstructor(con_enum(en, ef, e1.epos), patterns);
						case _: fail();
					}
				case EField(_):
					try {
						try_typing(e);
					}
					catch (_:ocaml.Exit) { fail(); }
					catch (bp:Bad_pattern) {
						var s = bp.msg;
						core.Error.error(s, p);
					}
				case EArrayDecl(el):
					switch (core.Type.follow(t)) {
						case TFun({args:tl, ret:tr}) if (tr.equals(typing.Matcher.fake_tuple_type)): // strict
							function loop_(el:ImmutableList<core.Ast.Expr>, tl:ImmutableList<core.Type.TSignatureArg>) : ImmutableList<Pattern> {
								return switch [el, tl] {
									case [e::el, {t:t}::tl]:
										var pat = make_(pctx, false, t, e);
										pat::loop_(el, tl);
									case [[], []]:
										[];
									case [[],_]: core.Error.error("Not enough arguments", p);
									case [{pos:p}::_, []]: core.Error.error("Too many arguments", p);
								}
							}
							var patterns = loop_(el, tl);
							PatTuple(patterns);
						case TInst({cl_path:{a:[], b:"Array"}}, [t2]), t2=TDynamic(_):
							var patterns = List.map(function (e) { // real is mapi but i is unused
								return make_(pctx, false, t2, e);
							}, el);
							PatConstructor(ConArray(List.length(patterns)), patterns);
						case _: fail();
					}
				case EObjectDecl(fl):
					function known_fields(t:core.Type.T) : ImmutableList<{fst:core.Type.TClassField, snd:core.Type.T}> {
						return switch (core.Type.follow(t)) {
							case TAnon(an):
								PMap.fold(function (cf:core.Type.TClassField, acc:ImmutableList<{fst:core.Type.TClassField, snd:core.Type.T}>) {
									return {fst:cf, snd:cf.cf_type}::acc;
								}, an.a_fields, []);
							case TInst(c, tl):
								function loop (fields:ImmutableList<{fst:core.Type.TClassField, snd:core.Type.T}>, c:core.Type.TClass, tl:core.Type.TParams) {
									var fields = List.fold_left(function (acc:ImmutableList<{fst:core.Type.TClassField, snd:core.Type.T}>, cf:core.Type.TClassField) {
										return
										if (context.Typecore.can_access(ctx, c, cf, false)) {
											{fst:cf, snd:core.Type.apply_params(c.cl_params, tl, cf.cf_type)}::acc;
										}
										else {
											acc;
										}
									}, fields, c.cl_ordered_fields);
									return switch (c.cl_super) {
										case None: fields;
										case Some({c:csup, params:tlsup}): loop(fields, csup, List.map(core.Type.apply_params.bind(c.cl_params, tl), tlsup));
									}
								}
								loop([], c, tl);
							case TAbstract(a={a_impl:Some(c)}, tl):
								var fields:ImmutableList<{fst:core.Type.TClassField, snd:core.Type.T}> = try {
									var el = core.Meta.get(Forward, a.a_meta).params;
									var sl = List.filter_map(function (e:core.Ast.Expr) {
										return switch (e.expr) {
											case EConst(CIdent(s)): Some(s);
											case _: None;
										}
									}, el);
									var fields = known_fields(core.Abstract.get_underlying_type(a, tl));
									if (sl != []) {
										fields;
									}
									else {
										List.filter(function (arg:{fst:core.Type.TClassField, snd:core.Type.T}) {
											var cf = arg.fst; var t = arg.snd;
											return List.mem(cf.cf_name, sl);
										}, fields);
									}
								}
								catch (_:ocaml.Not_found) { Tl; } // []
								var fields = List.fold_left(function (acc:ImmutableList<{fst:core.Type.TClassField, snd:core.Type.T}>, cf:core.Type.TClassField) {
									return
									if (core.Meta.has(Impl, cf.cf_meta)) {
										{fst:cf, snd:core.Type.apply_params(a.a_params, tl, cf.cf_type)}::acc;
									}
									else {
										acc;
									}
								}, fields, c.cl_ordered_statics);
								fields;
							case _: core.Error.error('Cannot field-match against ${typing.Matcher.s_type(t)}', e.pos);
						}
					}
					var known_fields_ = known_fields(t);
					function is_matchable(cf:core.Type.TClassField) : Bool {
						return !cf.cf_kind.match(Method(_));
					}
					var _tmp = List.fold_left(function (arg1:{fst:ImmutableList<Pattern>, snd:ImmutableList<String>}, arg2) {
						var patterns = arg1.fst; var fields = arg1.snd;
						var cf = arg2.fst; var t = arg2.snd;
						return
						try {
							if (pctx.in_reification && cf.cf_name == "pos") {
								throw ocaml.Not_found.instance;
							}
							var e1 = core.ast.Expr.field_assoc(cf.cf_name, fl);
							{fst:make_(pctx, false, t, e1)::patterns, snd:cf.cf_name::fields};
						}
						catch (_:ocaml.Not_found) {
							if (is_matchable(cf)) {
								{fst:({t:PatAny, pos:cf.cf_pos}: Pattern) :: patterns, snd:cf.cf_name::fields};
							}
							else {
								{fst:patterns, snd:fields};
							}
						}
					}, {fst:Tl, snd:Tl}, known_fields_);
					var patterns = _tmp.fst; var fields =_tmp.snd;
					List.iter(function (of:core.Ast.ObjectField) {
						var s = of.name; var e = of.expr;
						if (!List.mem(s, fields)) {
							core.Error.error('${typing.Matcher.s_type(t)} has no field ${s}', e.pos);
						}
					}, fl);
					PatConstructor(ConFields(fields), patterns);
				case EBinop(OpOr, e1, e2):
					var pctx1 = pctx.clone(); pctx1.current_locals = new Map<String, {fst:core.Type.TVar, snd:core.Globals.Pos}>();
					var pat1 = make_(pctx1, toplevel, t, e1);
					var pctx2 = pctx.clone(); pctx2.current_locals = new Map<String, {fst:core.Type.TVar, snd:core.Globals.Pos}>(); pctx2.or_locals = Some(pctx1.current_locals);
					var pat2 = make_(pctx2, toplevel, t, e2);
					PMap.iter (function (name:String, other:{fst:core.Type.TVar, snd:core.Globals.Pos}) {
						var v = other.fst; var p = other.snd;
						if (!PMap.mem(name, pctx2.current_locals)) {
							verror(name, p);
						}
						pctx.current_locals = PMap.add(name, {fst:v, snd:p}, pctx.current_locals);
					}, pctx1.current_locals);
					PatOr(pat1, pat2);
				case EBinop(OpAssign, e1, e2):
					function loop (in_display:Bool, e:core.Ast.Expr) : typing.matcher.pattern.T {
						return switch (e) {
							case {expr:EConst(CIdent(s)), pos:p}:
								var v = add_local(s, p);
								if (in_display) {
									typing.Typer.display_expr(ctx, e, core.Type.mk(TLocal(v), v.v_type, p), WithType(t), p);
								}
								var pat = make_(pctx, false, t, e2);
								PatBind(v, pat);
							case {expr:EParenthesis(e1)}: loop(in_display, e1);
							case {expr:EDisplay(e1, _)}: loop(true, e1);
							case _: fail();

						}
					}
					loop(false, e1);
				case EBinop(OpArrow, e1, e2):
					var restore = context.Typecore.save_locals(ctx);
					ctx.locals = pctx.ctx.locals;
					var v = add_local("_", core.Globals.null_pos);
					var e1 = typing.Typer.type_expr(ctx, e1, Value);
					v.v_name = "tmp";
					restore();
					var pat = make_(pctx, toplevel, e1.etype, e2);
					PatExtractor(v, e1, pat);
				case EDisplay(e, iscall):
					var pat = loop(e);
					if (iscall) {
						typing.Typer.handle_signature_display(ctx, e, WithType(t));
					}
					else {
						typing.Typer.handle_display(ctx, e, WithType(t));
					}
					pat;
				case _: fail();

			}
		}
		var pat = loop(e);
		return {t:pat, pos:p};
	}
	public static function make(ctx:context.Typecore.Typer, t:core.Type.T, e:core.Ast.Expr) : Pattern {
		var pctx = {
			ctx:ctx,
			current_locals: new Map<String, {fst:core.Type.TVar, snd:core.Globals.Pos}>(),
			ctx_locals: ctx.locals,
			or_locals:None,
			in_reification: false
		};
		return make_(pctx, true, t, e);
	}
}