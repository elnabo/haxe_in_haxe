package optimization;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
using ocaml.Cloner;
using ocaml.Hashtbl;
using ocaml.List;
using ocaml.PMap;
using ocaml.Ref;

using equals.Equal;

typedef In_local = {
	i_var: core.Type.TVar,
	i_subst: core.Type.TVar,
	i_outside: Bool,
	i_abstract_this: Bool,
	i_captured: Bool,
	i_write: Bool,
	i_read: Int,
	i_force_temp: Bool
}

class Optimizer {

	// ----------------------------------------------------------------------
	// API OPTIMIZATIONS
	public static function mk_untyped_call (name:String, p:core.Globals.Pos, params:ImmutableList<core.Type.TExpr>) : core.Type.TExpr {
		return {
			eexpr: TCall({ eexpr: TIdent(name), etype:core.Type.t_dynamic, epos: p }, params),
			etype: core.Type.t_dynamic,
			epos: p
		}
	}

	public static function api_inline2 (com:context.Common.Context, c:core.Type.TClass, field:String, params:ImmutableList<core.Type.TExpr>, p:core.Globals.Pos) : Option<core.Type.TExpr> {
		return
		switch [c.cl_path, field, params] {
			case [{a:[], b:"Type"}, "enumIndex", [{eexpr:TField(_, FEnum(en, f))}]]:
				switch (com.platform) {
					case Cs if (en.e_extern && !core.Meta.has(HxGen, en.e_meta)):
						// We don't want to optimize enums from external sources; as they might change unexpectedly
						// and since native C# enums don't have the concept of index - they have rather a value,
						// which can't be mapped to a native API - this kind of substitution is dangerous
						None;
					case _:
						Some(core.Type.mk(TConst(TInt(f.ef_index)), com.basic.tint, p));
				}
			case [{a:[], b:"Type"}, "enumIndex", [{eexpr:TCall({eexpr:TField(_, FEnum(en, f))}, pl)}]] if (List.for_all(function (e) { return !OptimizerTexpr.has_side_effect(e); }, pl)):
				switch (com.platform) {
					case Cs if (en.e_extern && !core.Meta.has(HxGen, en.e_meta)):
						// see comment above
						None;
					case _:
						Some(core.Type.mk(TConst(TInt(f.ef_index)), com.basic.tint, p));
				}
			case [{a:[], b:"Std"}, "int", [e={eexpr:TConst(TInt(_))}]]:
				Some(e.with({epos:p}));
			case [{a:[], b:"String"}, "fromCharCode", [{eexpr:TConst(TInt(i))}]] if (i > 0 && i < 128):
				Some(core.Type.mk(TConst(TString(String.fromCharCode(i))), com.basic.tstring, p));
			case [{a:[], b:"Std"}, "string", [{eexpr:TCast(e={eexpr:TConst(c)}, None)}]],
				 [{a:[], b:"Std"}, "string", [e={eexpr:TConst(c)}]]:
				switch (c) {
					case TString(s):
						Some(e.with({epos:p}));
					case TInt(i):
						Some({
							eexpr: TConst(TString(Std.string(i))),
							epos: p,
							etype: com.basic.tstring
						});
					case TBool(b):
						Some({
							eexpr: TConst(TString((b) ? "true" : "false")),
							epos: p,
							etype: com.basic.tstring
						});
					case _:
						None;
				}
			case [{a:[], b:"Std"}, "string", [e={eexpr:TIf(_, {eexpr:TConst(TString(_))}, Some({eexpr:TConst(TString(_))}))}]]:
				Some(e);
			case [{a:[], b:"Std"}, "string", [ev={eexpr:(TLocal(v)|TField({eexpr:TLocal(v)}, _))}]] if ((com.platform == Js || com.platform == Flash) && !core.Meta.has(CompilerGenerated, v.v_meta)):
				var pos = ev.epos;
				function stringv () {
					var to_str = core.Type.mk(TBinop(OpAdd, core.Type.mk(TConst(TString("")), com.basic.tstring, pos), ev), com.basic.tstring, p);
					return
					if (com.platform == Js || core.Type.is_nullable(ev.etype)) {
						var chk_null = core.Type.mk(TBinop(OpEq, ev, core.Type.mk(TConst(TNull), core.Type.t_dynamic, pos)), com.basic.tbool, pos);
						core.Type.mk(TIf(chk_null, core.Type.mk(TConst(TString("null")), com.basic.tstring, pos), Some(to_str)), com.basic.tstring, pos);
					}
					else {
						to_str;
					}
				}
				switch (core.Type.follow(ev.etype)) {
					case TInst({cl_path:{a:[], b:"String"}}, []),
						 TAbstract({a_path:{a:[], b:"Float"}}, []),
						 TAbstract({a_path:{a:[], b:"Int"}}, []),
						 TAbstract({a_path:{a:[], b:"UInt"}}, []),
						 TAbstract({a_path:{a:[], b:"Bool"}}, []):
						Some(stringv());
					case _:
						None;
				}
			case [{a:[], b:"Std"}, "int", [{eexpr:TConst(TFloat(f))}]]:
				var f = ocaml.FloatUtils.float_of_string(f);
				switch (ocaml.FloatUtils.classify_float(f)) {
					case FP_infinite, FP_nan:
						None;
					case _ if (f <= ocaml.FloatUtils.min_int32 -1. || f >= ocaml.FloatUtils.max_int32 +1.):
						None; // out range, keep platform-specific behavior
					case _:
						Some({eexpr:TConst(TInt(Std.int(f))), etype:com.basic.tint, epos:p});
				}
			case [{a:[], b:"Math"}, "ceil", [{eexpr:TConst(TFloat(f))}]]:
				var f = ocaml.FloatUtils.float_of_string(f);
				switch (ocaml.FloatUtils.classify_float(f)) {
					case FP_infinite, FP_nan:
						None;
					case _ if (f <= ocaml.FloatUtils.min_int32 - 1 || f >= ocaml.FloatUtils.max_int32 +1):
						None; // out range, keep platform-specific behavior
					case _:
						Some({eexpr:TConst(TInt(Math.ceil(f))), etype:com.basic.tint, epos:p});
				}
			case [{a:[], b:"Math"}, "floor", [{eexpr:TConst(TFloat(f))}]]:
				var f = ocaml.FloatUtils.float_of_string(f);
				switch (ocaml.FloatUtils.classify_float(f)) {
					case FP_infinite, FP_nan:
						None;
					case _ if (f <= ocaml.FloatUtils.min_int32 - 1 || f >= ocaml.FloatUtils.max_int32 +1):
						None; // out range, keep platform-specific behavior
					case _:
						Some({eexpr:TConst(TInt(Math.floor(f))), etype:com.basic.tint, epos:p});
				}
			case [{a:["cs"], b:"Lib"}, ("fixed"|"checked"|"unsafe"), [e]]:
				Some(mk_untyped_call("__"+field+"__", p, [e]));
			case [{a:["cs"], b:"Lib"}, ("lock"), [obj, block]]:
				Some(mk_untyped_call("__lock__", p, [obj, core.Type.mk_block(block)]));
			case [{a:["java"], b:"Lib"}, ("lock"), [obj, block]]:
				Some(mk_untyped_call("__lock__", p, [obj, core.Type.mk_block(block)]));
			case _:
				None;
		}
	}

	public static function api_inline (ctx:context.Typecore.Typer, c:core.Type.TClass, field:String, params:ImmutableList<core.Type.TExpr>, p:core.Globals.Pos) : Option<core.Type.TExpr> {
		return switch [c.cl_path, field, params] {
			case [{a:[], b:"Std"}, "is", [o, t]], [{a:["js"], b:"Boot"}, "__instanceof", [o, t]] if (ctx.com.platform == Js):
				var tstring = ctx.com.basic.tstring;
				var tbool = ctx.com.basic.tbool;
				var tint = ctx.com.basic.tint;
				var esyntax = {
					var m = Hashtbl.find(ctx.g.modules, new core.Path(["js"], "Syntax"));
					List.find_map(function (mt:core.Type.ModuleType) {
						return switch (mt) {
							case TClassDecl(cl={cl_path:{a:["js"], b:"Syntax"}}):
								Some(context.Typecore.make_static_this(cl, p));
							case _: None;
						}
					}, m.m_types);
				}
				function is_trivial (e:core.Type.TExpr) : Bool {
					return switch (e.eexpr) {
						case TConst(_), TLocal(_): true;
						case _: false;
					}
				}
				function typeof (t) : core.Type.TExpr {
					var tof = core.Texpr.Builder.fcall(esyntax, "typeof", [o], tstring, p);
					return core.Type.mk(TBinop(OpEq, tof, core.Type.mk(TConst(TString(t)), tstring, p)), tbool, p);
				}
				switch (t.eexpr) {
					// generate simple typeof checks for basic types
					case TTypeExpr(TClassDecl({cl_path:{a:[], b:"String"}})) : Some(typeof("string"));
					case TTypeExpr(TAbstractDecl({a_path:{a:[], b:"Bool"}})) : Some(typeof("boolean"));
					case TTypeExpr(TAbstractDecl({a_path:{a:[], b:"Float"}})) : Some(typeof("number"));
					case TTypeExpr(TAbstractDecl({a_path:{a:[], b:"Int"}})) if (is_trivial(o)):
						// generate typeof(o) == "number" && (o|0) === o check
						var lhs = core.Type.mk(TBinop(OpOr, o, core.Type.mk(TConst(TInt(0)), tint, p)), tint, p);
						var jscheck = core.Texpr.Builder.fcall(esyntax, "strictEq", [lhs, o], tbool, p);
						Some(core.Type.mk(TBinop(OpBoolAnd, typeof("number"), jscheck), tbool, p));
					case TTypeExpr(TClassDecl({cl_path:{a:[], b:"Array"}})):
						// generate (o instanceof Array) && o.__enum__ == null check
						var iof = core.Texpr.Builder.fcall(esyntax, "instanceof", [o,t], tbool, p);
						var enum_ = core.Type.mk(TField(o, FDynamic("__enum__")), core.Type.mk_mono(), p);
						var null_ = core.Type.mk(TConst(TNull), core.Type.mk_mono(), p);
						var not_enum = core.Type.mk(TBinop(OpEq, enum_, null_), tbool, p);
						Some(core.Type.mk(TBinop(OpBoolAnd, iof, not_enum), tbool, p));
					case TTypeExpr(TClassDecl(cls)) if (!cls.cl_interface):
						Some(core.Texpr.Builder.fcall(esyntax, "instanceof", [o, t], tbool, p));
					case _: None;
				}
			case [{a:(["cs"] | ["java"]), b:"Lib"}, "nativeArray", [edecl={eexpr:TArrayDecl(args)}, _] ],
				 [{a:["haxe", "ds", "_Vector"], b:"Vector_Impl_"}, "fromArrayCopy", [edecl={eexpr:TArrayDecl(args)}]] :
				try {
					var platf = switch (ctx.com.platform) {
						case Cs: "cs";
						case Java: "java";
						case _: throw ocaml.Exit;
					}
					var mpath = (field == "fromArrayCopy") ? new core.Path(["haxe", "ds"], "Vector") : new core.Path([platf], "NativeArray");
					var m = ctx.g.do_load_module(ctx, mpath, core.Globals.null_pos);
					var main = List.find(function (mt:core.Type.ModuleType) { return mt.match(TClassDecl(_)|TAbstractDecl(_)); }, m.m_types);
					var t:core.Type.T = switch [core.Type.follow(edecl.etype), main] {
						case [TInst({cl_path:{a:[], b:"Array"}}, [t]), TClassDecl(cl)]: TInst(cl, [t]);
						case [TInst({cl_path:{a:[], b:"Array"}}, [t]), TAbstractDecl(a)]: TAbstract(a, [t]);
						case _: trace("Shall not be seen"); throw false;
					}
					var _tmp = mk_untyped_call("__array__", p, args);
					_tmp.etype = t;
					Some(_tmp);
				}
				catch (_:ocaml.Exit) { None; }
			case _: api_inline2(ctx.com, c, field, params, p);
		}
	}

	// ----------------------------------------------------------------------
	// INLINING
	public static function inline_default_config (cf:core.Type.TClassField, t:core.Type.T) : {fst:Bool, snd:core.Type.T->core.Type.T} {
		// type substitution on both class and function type parameters
		function get_params (c:core.Type.TClass, pl:core.Type.TParams) {
			return switch (c.cl_super) {
				case None: {fst:c.cl_params, snd:pl};
				case Some({c:csup, params:spl}):
					var spl = switch (core.Type.apply_params(c.cl_params, pl, TInst(csup, spl))) {
						case TInst(_, pl): pl;
						case _: trace("Shall not be seen"); throw false;
					}
					var _tmp = get_params(csup, spl);
					var ct = _tmp.fst; var cpl = _tmp.snd;
					{fst:List.append(c.cl_params, ct), snd:List.append(pl, cpl)};
			}
		}
		var tparams = switch (core.Type.follow(t)) {
			case TInst(c, pl): get_params(c, pl);
			case _: {fst:Tl, snd:Tl};
		}
		var pmonos = List.map(function (_) { return core.Type.mk_mono(); }, cf.cf_params);
		var tmonos = List.append(tparams.snd, pmonos);
		var tparams = List.append(tparams.fst, cf.cf_params);
		return {fst:tparams!=Tl, snd:core.Type.apply_params.bind(tparams, tmonos)};
	}

	public static function type_inline (ctx:context.Typecore.Typer, cf:core.Type.TClassField, f:core.Type.TFunc, ethis:core.Type.TExpr, params:ImmutableList<core.Type.TExpr>, tret:core.Type.T, config:Option<{fst:Bool, snd:core.Type.T->core.Type.T}>, p:core.Globals.Pos, ?self_calling_closure:Bool=false, force:Bool) : Option<core.Type.TExpr> {
		return
		// perform some specific optimization before we inline the call since it's not possible to detect at final optimization time
		try {
			var cl = switch (core.Type.follow(ethis.etype)) {
				case TInst(c, _): c;
				case TAnon(a):
					switch (a.a_status.get()) {
						case Statics(c): c;
						case _: throw ocaml.Exit.instance;
					}
				case _: throw ocaml.Exit.instance;
			}
			switch (api_inline(ctx, cl, cf.cf_name, params, p)) {
				case None: throw ocaml.Exit.instance;
				case Some(e): Some(e);
			}
		}
		catch (_:ocaml.Exit) {
			var _tmp = switch (config) {
				case Some(config): config;
				case None: inline_default_config(cf, ethis.etype);
			}
			var has_params = _tmp.fst; var map_type = _tmp.snd;
			// locals substitution
			var locals = new Hashtbl<Int, In_local>();//Hashtbl.create(0);
			function local (v:core.Type.TVar) : In_local {
				return
				try {
					Hashtbl.find(locals, v.v_id);
				}
				catch (_:ocaml.Not_found) {
					var v_ = core.Type.alloc_var(v.v_name, v.v_type, v.v_pos);
					v_.v_extra = v.v_extra; // clone or not ?
					var i = {
						i_var: v_,
						i_subst: v_,
						i_outside: false,
						i_abstract_this: core.Meta.has(This, v.v_meta),
						i_captured: false,
						i_write: false,
						i_force_temp: false,
						i_read: 0
					}
					i.i_subst.v_meta = List.filter(function (meta:core.Ast.MetadataEntry) { var m = meta.name; return m != This; }, v.v_meta);
					Hashtbl.add(locals, v.v_id, i);
					Hashtbl.add(locals, i.i_subst.v_id, i);
					i;
				}
			}
			var in_local_fun = new Ref(false);
			function read_local (v:core.Type.TVar) : In_local {
				var l:In_local = try {
					Hashtbl.find(locals, v.v_id);
				}
				catch (_:ocaml.Not_found) {
					{
						i_var: v,
						i_subst: v,
						i_outside: true,
						i_abstract_this: core.Meta.has(This, v.v_meta),
						i_captured: false,
						i_write: false,
						i_force_temp: false,
						i_read: 0
					}
				}
				if (in_local_fun.get()) {
					l.i_captured = true;
				}
				return l;
			}
			// use default values for null/unset arguments
			function loop (pl:ImmutableList<core.Type.TExpr>, al:ImmutableList<{v:core.Type.TVar, c:Option<core.Type.TConstant>}>, first:Bool) : ImmutableList<core.Type.TExpr> {
				return
				switch [pl, al] {
					case [_, []]: [];
					case [e::pl, {v:v, c:opt}::al]:
						/*
							if we pass a Null<T> var to an inlined method that needs a T.
							we need to force a local var to be created on some platforms.
						*/
						if (ctx.com.config.pf_static && !core.Type.is_nullable(v.v_type) && core.Type.is_null(e.etype)) {
							local(v).i_force_temp = true;
						}
						/*
							if we cast from Dynamic, create a local var as well to do the cast
							once and allow DCE to perform properly.
						*/
						var e = if (core.Type.follow(v.v_type) != core.Type.t_dynamic && core.Type.follow(e.etype) != core.Type.t_dynamic) {
							core.Type.mk(TCast(e, None), v.v_type, e.epos);
						}
						else { e; }
						var _tmp = switch [e.eexpr, opt] {
							case [TConst(TNull), Some(c)]:
								core.Type.mk(TConst(c), v.v_type, e.epos);
							/*
								This is really weird and should be reviewed again. The problem is that we cannot insert a TCast here because
								the abstract `this` value could be written to, which is not possible if it is wrapped in a cast.

								The original problem here is that we do not generate a temporary variable and thus mute the type of the
								`this` variable, which leads to unification errors down the line. See issues #2236 and #3713.
							*/
							/* | _ when first && (Meta.has Meta.Impl cf.cf_meta) -> {e with etype = v.v_type} */
							case _: e;
						}
						_tmp :: loop(pl, al, false);
					case [[], {v:v, c:opt}::al]:
						core.Type.mk(TConst(switch (opt) { case None: TNull; case Some(c): c;}), v.v_type, p) :: loop([], al, false);
				}
			}
			/*
				Build the expr/var subst list
			*/
			var ethis = switch (ethis.eexpr) {
				case TConst(TSuper):
					ethis.with({eexpr:core.Type.TExprExpr.TConst(TThis)});
				case _:
					ethis;
			}
			var vthis = core.Type.alloc_var("_this", ethis.etype, ethis.epos);
			var _tmp = optimization.OptimizerTexpr.create_affection_checker();
			var might_be_affected = _tmp.fst; var collected_modified_locals = _tmp.snd;
			var has_side_effect_ = new Ref(false);
			var inlined_vars = List.map2(function (e:core.Type.TExpr, arg:{v:core.Type.TVar, c:Option<core.Type.TConstant>}) {
				var v = arg.v;
				var l = local(v);
				if (OptimizerTexpr.has_side_effect(e)) {
					collected_modified_locals(e);
					has_side_effect_.set(true);
					l.i_force_temp = true;
				}
				if (l.i_abstract_this) {
					l.i_subst.v_extra = Some({params:[], expr:Some(e)});
				}
				return {fst:l, snd:e};
			}, (ethis::loop(params, f.tf_args, true)), ({v:vthis, c:None}::f.tf_args));
			inlined_vars = List.rev(inlined_vars);
			/*
				here, we try to eliminate final returns from the expression tree.
				However, this is not entirely correct since we don't yet correctly propagate
				the type of returned expressions upwards ("return" expr itself being Dynamic).

				We also substitute variables with fresh ones that might be renamed at later stage.
			*/
			function opt (f:core.Type.TExpr->core.Type.TExpr, o:Option<core.Type.TExpr>) : Option<core.Type.TExpr> {
				return switch (o) {
					case None: None;
					case Some(e): Some(f(e));
				}
			}
			var has_vars = new Ref(false);
			var in_loop = new Ref(false);
			var cancel_inlining = new Ref(false);
			var has_return_value = new Ref(false);
			function return_type (t, el) {
				return
				/* If the function return is Dynamic or Void, stick to it. */
				if (core.Type.follow(f.tf_type) == core.Type.t_dynamic || core.Type.ExtType.is_void(core.Type.follow(f.tf_type))) {
					f.tf_type;
				}
				/* If the expression is Void, find common type of its branches. */
				else if (core.Type.ExtType.is_void(t)) {
					typing.Typer.unify_min(ctx, el);
				}
				else { t; }
			}
			var map_pos = (self_calling_closure) ? function (e:core.Type.TExpr) { return e; } : function (e:core.Type.TExpr) { return e.with({epos:p}); };
			function map (term:Bool, e:core.Type.TExpr) : core.Type.TExpr {
				var po = e.epos;
				var e = map_pos(e);
				return
				switch (e.eexpr) {
					case TLocal(v):
						var l = read_local(v);
						l.i_read = l.i_read + ((in_loop.get()) ? 2 : 1);
						/* never inline a function which contain a delayed macro because its bound
							to its variables and not the calling method */
						if (v.v_name == "$__delayed_call__") { cancel_inlining.set(true); }
						var e = e.with({eexpr:core.Type.TExprExpr.TLocal(l.i_subst)});
						(l.i_abstract_this) ? core.Type.mk(TCast(e, None), v.v_type, e.epos) : e;
					case TConst(TThis):
						var l = read_local(vthis);
						l.i_read = l.i_read + ((in_loop.get()) ? 2 : 1);
						e.with({eexpr:core.Type.TExprExpr.TLocal(l.i_subst)});
					case TVar(v, eo):
						has_vars.set(true);
						e.with({eexpr:core.Type.TExprExpr.TVar(local(v).i_subst, opt(map.bind(false), eo))});
					case TReturn(eo) if (!in_local_fun.get()):
						if (!term) { core.Error.error("Cannot inline a not final return", po); }
						switch (eo) {
							case None: core.Type.mk(TConst(TNull), f.tf_type, p);
							case Some(e):
								has_return_value.set(true);
								map(term, e);
						}
					case TFor(v, e1, e2):
						var i = local(v);
						var e1 = map(false, e1);
						var old = in_loop.get();
						in_loop.set(true);
						var e2 = map(false, e2);
						in_loop.set(old);
						e.with({eexpr:core.Type.TExprExpr.TFor(i.i_subst, e1, e2)});
					case TWhile(cond, eloop, flag):
						var cond = map(false, cond);
						var old = in_loop.get();
						in_loop.set(true);
						var eloop = map(false, eloop);
						in_loop.set(old);
						e.with({eexpr:core.Type.TExprExpr.TWhile(cond, eloop, flag)});
					case TSwitch(e1, cases, def) if (term):
						var term = term && (def != None || OptimizerTexpr.is_exhaustive(e1));
						var cases = List.map(function (c:{values:ImmutableList<core.Type.TExpr>, e:core.Type.TExpr}) {
							var el = c.values; var e = c.e;
							var el = List.map(map.bind(false), el);
							return {values:el, e:map(term, e)};
						}, cases);
						var def = opt(map.bind(term), def);
						var t = return_type(e.etype, List.append(List.map(function(c) {return c.e; }, cases), switch (def) { case None: []; case Some(e): [e];}));
						e.with({
							eexpr:core.Type.TExprExpr.TSwitch(map(false, e1), cases, def),
							etype:t
						});
					case TTry(e1, catches):
						var t = (!term) ? e.etype : return_type(e.etype, e1::List.map(function (c) { return c.e; }, catches));
						e.with({
							eexpr:core.Type.TExprExpr.TTry(map(term, e1), List.map(function (c) {
								var v = c.v; var e = c.e;
								var lv = local(v).i_subst;
								var e = map(term, e);
								return {v:lv, e:e};
							}, catches)),
							etype: t
						});
					case TBlock(l):
						var old = context.Typecore.save_locals(ctx);
						var t = new Ref(e.etype);
						function has_term_return(e:core.Type.TExpr) : Bool {
							function loop (e:core.Type.TExpr) : Void {
								var r = switch (e.eexpr) {
									case TReturn(_): true;
									case TFunction(_): false;
									case TIf(_,_,None), TSwitch(_,_,None), TFor(_), TWhile(_,_,NormalWhile): false; // we might not enter this code at all
									case TTry(a, catches): List.for_all(has_term_return, a::List.map(function (c) { return c.e; }, catches));
									case TIf(cond, a, Some(b)): has_term_return(cond) || (has_term_return(a) && has_term_return(b));
									case TSwitch(cond, cases, Some(def)): has_term_return(cond) || List.for_all(has_term_return, def::List.map(function (c) { return c.e; }, cases));
									case TBinop(OpBoolAnd, a, b): has_term_return(a) && has_term_return(b);
									case _: core.Type.iter(loop, e); false;
								}
								if (r) { throw ocaml.Exit.instance; }
							}
							return try { loop(e); false; } catch (_:ocaml.Exit) { true; }
						}
						function loop (l:ImmutableList<core.Type.TExpr>) : ImmutableList<core.Type.TExpr> {
							return switch (l) {
								case [] if (term):
									t.set(core.Type.mk_mono());
									[core.Type.mk(TConst(TNull), t.get(), p)];
								case []: [];
								case [e]:
									var e = map(term, e);
									if (term) { t.set(e.etype); }
									[e];
								case (e={eexpr:TIf(cond, e1, None)})::l if (term && has_term_return(e1)):
									var _e = e.with({
										eexpr: core.Type.TExprExpr.TIf(cond, e1, Some(core.Type.mk(TBlock(l), e.etype, e.epos))),
										epos: core.Ast.punion(e.epos, switch (List.rev(l)) { case e::_:e.epos; case []: throw false;})
									});
									loop([_e]);
								case e::l:
									var e = map(false, e);
									e::loop(l);
							}
						}
						var l = loop(l);
						old();
						e.with({eexpr: core.Type.TExprExpr.TBlock(l), etype: t.get()});
					case TIf(econd, eif, Some(eelse)) if (term):
						var econd = map(false, econd);
						var eif = map(term, eif);
						var eelse = map(term, eelse);
						var t = return_type(e.etype, [eif, eelse]);
						e.with({
							eexpr: core.Type.TExprExpr.TIf(econd, eif, Some(eelse)),
							etype: t
						});
					case TParenthesis(e1):
						var e1 = map(term, e1);
						core.Type.mk(TParenthesis(e1), e1.etype, e.epos);
					case TUnop(op=(OpIncrement|OpDecrement), flag, e1={eexpr:TLocal(v)}):
						has_side_effect_.set(true);
						var l = read_local(v);
						l.i_write = true;
						e.with({eexpr: core.Type.TExprExpr.TUnop(op, flag, e1.with({eexpr: core.Type.TExprExpr.TLocal(l.i_subst)}))});
					case TBinop(op=(OpAssign|OpAssignOp(_)), e1={eexpr:TLocal(v)}, e2):
						has_side_effect_.set(true);
						var l = read_local(v);
						l.i_write = true;
						var e2 = map(false, e2);
						e.with({eexpr: core.Type.TExprExpr.TBinop(op, e1.with({eexpr:core.Type.TExprExpr.TLocal(l.i_subst)}), e2)});
					case TObjectDecl(fl):
						var fl = List.map(function (of:core.Type.TObjectField) : core.Type.TObjectField {
							var e = of.expr;
							return {name:of.name, pos:of.pos, quotes:of.quotes, expr:map(false, e)};
						}, fl);
						switch (core.Type.follow(e.etype)) {
							case TAnon(an) if (an.a_status.get().match(Const)):
								e.with({
									eexpr:core.Type.TExprExpr.TObjectDecl(fl),
									etype:core.Type.T.TAnon({a_fields:an.a_fields, a_status:new Ref(core.Type.AnonStatus.Closed)})
								 });
							case _:
								e.with({eexpr:core.Type.TExprExpr.TObjectDecl(fl)});
						}
					case TFunction(f):
						switch (f.tf_args) {
							case []:
							case _: has_vars.set(true);
						}
						var old = context.Typecore.save_locals(ctx);
						var old_fun = in_local_fun.get();
						var args = List.map(function (arg) { var v = arg.v; var c = arg.c; return {v:local(v).i_subst, c:c}; }, f.tf_args);
						in_local_fun.set(true);
						var expr = map(false, f.tf_expr);
						in_local_fun.set(old_fun);
						old();
						e.with({eexpr: core.Type.TExprExpr.TFunction({tf_args:args, tf_expr:expr, tf_type:f.tf_type})});
					case TCall({eexpr:TConst(TSuper), etype:t}, el):
						has_side_effect_.set(true);
						switch (core.Type.follow(t)) {
							case TInst(c={cl_constructor:Some({cf_kind:Method(MethInline), cf_expr:Some({eexpr:TFunction(tf)})})}, _):
								switch (type_inline_ctor(ctx, c, cf, tf, ethis, el, po)) {
									case Some(e): map(term, e);
									case None: core.Error.error("Could not inline super constructor call", po);
								}
							case _: core.Error.error("Cannot inline function containing super", po);
						}
					case TConst(TSuper):
						core.Error.error("Cannot inline function containing super", po);
					case TMeta(m, e1):
						var e1 = map(term, e1);
						e.with({eexpr: core.Type.TExprExpr.TMeta(m, e1)});
					case TNew(_), TCall(_), TBinop((OpAssignOp(_)|OpAssign), _, _), TUnop((OpIncrement|OpDecrement), _, _):
						has_side_effect_.set(true);
						core.Type.map_expr(map.bind(false), e);
					case _:
						core.Type.map_expr(map.bind(false), e);
				}
			}
			var e = map(true, f.tf_expr);
			if (has_side_effect_.get()) {
				List.iter(function (iv:{fst:In_local, snd:core.Type.TExpr}) {
					var l = iv.fst; var e = iv.snd;
					if (might_be_affected(e)) {
						l.i_force_temp = true;
					}
				}, inlined_vars);
			}
			/*
				if variables are not written and used with a const value, let's substitute
				with the actual value, either create a temp var
			*/
			var subst = new Ref<PMap<Int, core.Type.TExpr>>(PMap.empty());
			function is_constant (e:core.Type.TExpr) : Bool {
				function loop (e:core.Type.TExpr) : Void {
					switch (e.eexpr) {
						case TLocal(_), TConst(TThis): // not really, but should not be move inside a function body
							throw ocaml.Exit.instance;
						case TObjectDecl(_), TArrayDecl(_): throw ocaml.Exit.instance;
						case TField(_, FEnum(_)), TTypeExpr(_), TConst(_):
						case _: core.Type.iter(loop, e);
					}
				}
				return try { loop(e); true; } catch (_:ocaml.Exit) { false; }
			}
			function is_writable (e:core.Type.TExpr) : Bool {
				return switch (e.eexpr) {
					case TField(_), TEnumParameter(_), TLocal(_), TArray(_): true;
					case _: false;
				}
			}
			var force = new Ref(force);
			var vars = List.fold_left(function (acc:ImmutableList<{fst:core.Type.TVar, snd:Option<core.Type.TExpr>}>, iv:{fst:In_local, snd:core.Type.TExpr}) {
				var i = iv.fst; var e = iv.snd;
				var flag = !i.i_force_temp && (switch (e.eexpr) {
					case TLocal(_) if (i.i_abstract_this) : true;
					case TLocal(_), TConst(_): !i.i_write;
					case TFunction(_) if (i.i_write):
						core.Error.error("Cannot modify a closure parameter inside inline method", p);
						true;
					case _: !i.i_write && i.i_read <= 1;
				});
				var flag = flag && (!i.i_captured || is_constant(e));
				// force inlining if we modify 'this'
				if (i.i_write && i.i_abstract_this) { force.set(true); }
				// force inlining of 'this' variable if it is written
				var flag = if (!flag && i.i_abstract_this && i.i_write) {
					if (!is_writable(e)) { core.Error.error("Cannot modify the abstract value, store it into a local first", p); }
					true;
				}
				else { flag; }
				return
				if (flag) {
					subst.set(PMap.add(i.i_subst.v_id, e, subst.get()));
					acc;
				}
				else {
					// mark the replacement local for the analyzer
					if (i.i_read <= 1 && !i.i_write) {
						i.i_subst.v_meta = ({name:CompilerGenerated, params:[], pos:p} : core.Ast.MetadataEntry) :: i.i_subst.v_meta;
					}
					{fst:i.i_subst, snd:Some(e)}::acc;
				}
			}, [], inlined_vars);
			var subst = subst.get();
			var inline_params:core.Type.TExpr->core.Type.TExpr = null;
			inline_params = function (e:core.Type.TExpr) : core.Type.TExpr {
				return switch (e.eexpr) {
					case TLocal(v):
						try {
							PMap.find(v.v_id, subst);
						}
						catch (_:ocaml.Not_found) {
							e;
						}
					case _:
						core.Type.map_expr(inline_params, e);
				}
			}
			var e = (PMap.is_empty(subst)) ? e : inline_params(e);
			var init = switch (vars) { case []: None; case l: Some(l); }
			/*
				If we have local variables and returning a value, then this will result in
				unoptimized JS code, so let's instead skip inlining.

				This could be fixed with better post process code cleanup (planed)
			*/
			if (cancel_inlining.get()) {
				None;
			}
			else {
				function wrap(e:core.Type.TExpr) : core.Type.TExpr {
					// we can't mute the type of the expression because it is not correct to do so
					var etype = (has_params) ? map_type(e.etype) : e.etype;
					// if the expression is "untyped" and we don't want to unify it accidentally !
					return
					try {
						switch (core.Type.follow(e.etype)) {
							case TMono(_), TInst({cl_kind:KTypeParameter(_)}, _):
								switch (core.Type.follow(tret)) {
									case TAbstract({a_path:{a:[], b:"Void"}}, _): e;
									case _: throw new core.Type.Unify_error([]);
								}
							case _:
								core.Type.type_eq((ctx.com.config.pf_static) ? EqDoNotFollowNull : EqStrict, etype, tret);
								e;
						}
					}
					catch (_:core.Type.Unify_error) {
						core.Type.mk(TCast(e, None), tret, e.epos);
					}
				}
				var e = switch [e.eexpr, init] {
					case [_, None] if (!has_return_value.get()):
						switch (e.eexpr) {
							case TBlock(_):
								e.with({etype:tret});
							case _: core.Type.mk(TBlock([e]), tret, e.epos);
						}
					case [TBlock([e]), None]: wrap(e);
					case [_, None]: wrap(e);
					case [TBlock(l), Some(vl)]:
						var el_v = List.map(function (arg:{fst:core.Type.TVar, snd:Option<core.Type.TExpr>}) {
							var v = arg.fst; var eo = arg.snd;
							return core.Type.mk(TVar(v, eo), ctx.t.tvoid, e.epos);
						}, vl);
						core.Type.mk(TBlock(List.append(el_v, l)), tret, e.epos);
					case [_, Some(vl)]:
						var el_v = List.map(function (arg:{fst:core.Type.TVar, snd:Option<core.Type.TExpr>}) {
							var v = arg.fst; var eo = arg.snd;
							return core.Type.mk(TVar(v, eo), ctx.t.tvoid, e.epos);
						}, vl);
						core.Type.mk(TBlock(List.append(el_v, [e])), tret, e.epos);
				};
				function _inline_meta (e:core.Type.TExpr, meta:core.Ast.MetadataEntry) : core.Type.TExpr {
					return switch (meta) {
						case {name:(Deprecated|Pure)}: core.Type.mk(TMeta(meta, e), e.etype, e.epos);
						case _:e;
					}
				}
				var e = List.fold_left(_inline_meta, e, cf.cf_meta);
				var e = context.display.Diagnostics.secure_generated_code(ctx, e);
				if (core.Meta.has(Custom(":inlineDebug"), ctx.meta)) {
					function se (t:String, e:core.Type.TExpr) {
						return core.Type.s_expr_pretty(true, t, true, core.Type.s_type.bind(core.Type.print_context()), e);
					}
					Sys.println('Inline ${cf.cf_name}:\n\tArgs: ${List.join("", List.map(function (iv) {
						var i = iv.fst; var e = iv.snd;
						return '\n\t\t${i.i_subst.v_name}<${i.i_subst.v_id}> = ${se("\t\t", e)}';
					}, inlined_vars))}\n\tExpr: ${se("\t", f.tf_expr)}\n\tResult: ${se("\t", e)}');
				}
				// we need to replace type-parameters that were used in the expression
				if (!has_params) {
					Some(e);
				}
				else {
					var mt = map_type(cf.cf_type);
					function unify_func () {
						context.Typecore.unify_raise(ctx, mt, TFun({args:List.map(function (e):core.Type.TSignatureArg { return {name:"", opt:false, t:e.etype}; }, params), ret:tret}), p);
					}
					switch (core.Type.follow(ethis.etype)) {
						case TAnon(a):
							switch (a.a_status.get()) {
								case Statics({cl_kind:KAbstractImpl(a)}) if (core.Meta.has(Impl, cf.cf_meta)):
									if (cf.cf_name != "_new") {
										// the first argument must unify with a_this for abstract implementation functions
										var tb:core.Type.T = TFun({args:{name:"", opt:false, t:map_type(a.a_this)}::List.map(function (e) { return {name:"", opt:false, t:e.etype};}, List.tl(params)), ret:tret});
										context.Typecore.unify_raise(ctx, mt, tb, p);
									}
								case _: unify_func();
							}
						case _: unify_func();
					}
					/*
						this is very expensive since we are building the substitution list for
						every expression, but hopefully in such cases the expression size is small
					*/
					var vars = new Hashtbl<Int, Bool>();
					function map_var (v:core.Type.TVar) : core.Type.TVar{
						if (!Hashtbl.mem(vars, v.v_id)) {
							Hashtbl.add(vars, v.v_id, true);
							if (!read_local(v).i_outside) {
								v.v_type = map_type(v.v_type);
							}
						}
						return v;
					}
					function map_expr_type (e:core.Type.TExpr) {
						return core.Type.map_expr_type(map_expr_type, map_type, map_var, e);
					}
					Some(map_expr_type(e));
				}
			}
		}
	}

	// Same as type_inline, but modifies the function body to add field inits
	public static function type_inline_ctor (ctx:context.Typecore.Typer, c:core.Type.TClass, cf:core.Type.TClassField, tf:core.Type.TFunc, ethis:core.Type.TExpr, el:ImmutableList<core.Type.TExpr>, po:core.Globals.Pos) : Option<core.Type.TExpr> {
		trace("TODO: type_inline_ctor");
		throw false;
	}

	// ----------------------------------------------------------------------
	// LOOPS
	public static function optimize_for_loop (ctx:context.Typecore.Typer, i:String, pi:core.Globals.Pos, e1:core.Type.TExpr, e2:core.Ast.Expr, p:core.Globals.Pos) : Option<core.Type.TExpr> {
		var t_void = ctx.t.tvoid;
		var t_int = ctx.t.tint;
		function lblock(el) : Option<core.Type.TExpr> {
			return Some(core.Type.mk(TBlock(el), t_void, p));
		}
		function mk_field (e, n) : core.Type.TExprExpr {
			return TField(e, try {core.Type.quick_field(e.etype, n); } catch (_:ocaml.Not_found) { trace("Shall not be seen"); throw false; });
		}
		function gen_int_iter(pt:core.Type.T, f_get:(core.Type.TExpr, core.Type.TExpr, core.Type.T, core.Globals.Pos) -> core.Type.TExpr, f_length:(core.Type.TExpr, core.Globals.Pos)->core.Type.TExpr) : Option<core.Type.TExpr> {
			var i = context.Typecore.add_local(ctx, i, pt, pi);
			var index = context.Typecore.gen_local(ctx, t_int, pi);
			index.v_meta = ({name:ForLoopVariable, params:[], pos:core.Globals.null_pos} : core.Ast.MetadataEntry) :: index.v_meta;
			var _tmp = switch (e1.eexpr) {
				case TLocal(_): {fst:e1, snd:None};
				case _:
					var atmp = context.Typecore.gen_local(ctx, e1.etype, e1.epos);
					{fst:core.Type.mk(TLocal(atmp), e1.etype, e1.epos), snd:Some({fst:atmp, snd:Some(e1)})};
			}
			var arr = _tmp.fst; var avars = _tmp.snd;
			var iexpr = core.Type.mk(TLocal(index), t_int, p);
			var e2 = typing.Typer.type_expr(ctx, e2, NoValue);
			var aget= core.Type.mk(TVar(i, Some(f_get(arr, iexpr, pt, p))), t_void, pi);
			var incr = core.Type.mk(TUnop(OpIncrement, Prefix, iexpr), t_int, p);
			var block = switch (e2.eexpr) {
				case TBlock(el):
					core.Type.mk(TBlock(aget::incr::el), t_void, e2.epos);
				case _:
					core.Type.mk(TBlock([aget, incr, e2]), t_void, p);
			}
			var ivar = Some(core.Type.mk(TConst(TInt(0)), t_int, p));
			var elength = f_length(arr, p);
			var el:ImmutableList<core.Type.TExpr> = [ core.Type.mk(
				TWhile(
					core.Type.mk(TBinop(OpLt, iexpr, elength), ctx.t.tbool, p),
					block,
					NormalWhile
				), t_void, p)
			];
			var el = switch (avars) {
				case None: el;
				case Some({fst:v, snd:eo}):
					core.Type.mk(TVar(v, eo), t_void, p) :: el;
			}
			var el = core.Type.mk(TVar(index, ivar), t_void, p)::el;
			return lblock(el);
		}

		function get_next_array_element(arr:core.Type.TExpr, iexpr:core.Type.TExpr, pt:core.Type.T, p:core.Globals.Pos) : core.Type.TExpr {
			return core.Type.mk(TArray(arr, iexpr), pt, p);
		}
		function get_array_length(arr:core.Type.TExpr, p:core.Globals.Pos) : core.Type.TExpr {
			return core.Type.mk(mk_field(arr, "length"), ctx.com.basic.tint, p);
		}
		return switch [e1.eexpr, core.Type.follow(e1.etype)] {
			case [TNew({cl_path:{a:[], b:"IntIterator"}}, [], [i1, i2]), _]:
				var max = switch [i1.eexpr, i2.eexpr] {
					case [TConst(TInt(a)), TConst(TInt(b))] if (b < a):
						core.Error.error("Range operator can't iterate backwards", p);
					case [_, TConst(_)]: None;
					case _: Some(context.Typecore.gen_local(ctx, t_int, e1.epos));
				}
				var tmp = context.Typecore.gen_local(ctx, t_int, pi);
				tmp.v_meta = ({name:ForLoopVariable, params:[], pos:core.Globals.null_pos} : core.Ast.MetadataEntry) :: tmp.v_meta;
				var i = context.Typecore.add_local(ctx, i, t_int, pi);
				function check(e:core.Type.TExpr) : Void {
					switch (e.eexpr) {
						case TBinop(OpAssign, {eexpr:TLocal(l)}, _),
							TBinop(OpAssignOp(_), {eexpr:TLocal(l)}, _),
							TUnop(OpIncrement, _, {eexpr:TLocal(l)}),
							TUnop(OpDecrement, _, {eexpr:TLocal(l)}) if (l.equals(i)):
								core.Error.error("Loop variable cannot be modified", e.epos);
						case _: core.Type.iter(check, e);
					}
				}
				var e2 = context.Typecore.type_expr(ctx, e2, NoValue);
				check(e2);
				var etmp = core.Type.mk(TLocal(tmp), t_int, p);
				var incr = core.Type.mk(TUnop(OpIncrement, Postfix, etmp), t_int, p);
				var init = core.Type.mk(TVar(i, Some(incr)), t_void, pi);
				var block = switch (e2.eexpr) {
					case TBlock(el): core.Type.mk(TBlock(init::el), t_void, e2.epos);
					case _: core.Type.mk(TBlock([init, e2]), t_void, p);
				}
				// force locals to be of Int type (to prevent Int/UInt issues)
				var i2 = switch (core.Type.follow(i2.etype)) {
					case TAbstract({a_path:{a:[], b:"Int"}}, []): i2;
					case _:
						i2.with({
							eexpr: core.Type.TExprExpr.TCast(i2, None),
							etype: t_int
						});
				}
				switch (max) {
					case None:
						lblock([
							core.Type.mk(TVar(tmp, Some(i1)), t_void, p),
							core.Type.mk(TWhile(
								core.Type.mk(TBinop(OpLt, etmp, i2), ctx.t.tbool, p),
								block,
								NormalWhile
							), t_void, p)
						]);
					case Some(max):
						lblock([
							core.Type.mk(TVar(tmp, Some(i1)), t_void, p),
							core.Type.mk(TVar(max, Some(i2)), t_void, p),
							core.Type.mk(TWhile(
								core.Type.mk(TBinop(OpLt, etmp, core.Type.mk(TLocal(max), t_int, p)), ctx.t.tbool, p),
								block,
								NormalWhile
							), t_void, p)
						]);
				}
			case [TArrayDecl(el), TInst({cl_path:{a:[], b:"Array"}}, [pt])] if (false):
				// Not doing unreachable code
				trace("Shall not be seen"); throw false;
			case [_, TInst({cl_path:{a:[], b:"Array"}}, [pt])],
				[_, TInst({cl_path:{a:["flash"], b:"Vector"}}, [pt])]:
				gen_int_iter(pt, get_next_array_element, get_array_length);
			case [_, TInst(c={cl_array_access:Some(pt)}, pl)] if ((try {switch (core.Type.follow(PMap.find("length", c.cl_fields).cf_type)) { case TAbstract({a_path:{a:[], b:"Int"}}, []): true; case _: false; }} catch (_:ocaml.Not_found) { false; }) && !PMap.mem("iterator", c.cl_fields)):
				gen_int_iter(core.Type.apply_params(c.cl_params, pl, pt), get_next_array_element, get_array_length);
			case [_, TAbstract(a={a_impl:Some(c)}, tl)]:
				try {
					var cf_length = PMap.find("get_length", c.cl_statics);
					function get_length (e:core.Type.TExpr, p:core.Globals.Pos) {
						return context.Typecore.make_static_call(ctx, c, cf_length, core.Type.apply_params.bind(a.a_params, tl), [e], ctx.com.basic.tint, p);
					}
					switch (core.Type.follow(cf_length.cf_type)) {
						case TFun({ret:tr}):
							switch (core.Type.follow(tr)) {
								case TAbstract({a_path:{a:[], b:"Int"}}, _):
								case _: throw ocaml.Not_found.instance;
							}
						case _: throw ocaml.Not_found.instance;
					}
					try {
						// first try: do we have an @:arrayAccess getter field?
						var todo = core.Type.mk(TConst(TNull), ctx.t.tint, p);
						var _tmp = context.Typecore.find_array_access_raise_ref.get()(ctx, a, tl, todo, None, p);
						var cf = _tmp.cf; var r = _tmp.r;
						function get_next (e_base:core.Type.TExpr, e_index:core.Type.TExpr, t:core.Type.T, p:core.Globals.Pos) : core.Type.TExpr {
							return context.Typecore.make_static_call(ctx, c, cf, core.Type.apply_params.bind(a.a_params, tl), [e_base, e_index], r, p);
						}
						gen_int_iter(r, get_next, get_length);
					}
					catch (_:ocaml.Not_found) {
						// second try: do we have @:arrayAccess on the abstract itself? *)
						if (!core.Meta.has(ArrayAccess, a.a_meta)) {
							throw ocaml.Not_found.instance;
						}
						// let's allow this only for core-type abstracts *)
						if (!core.Meta.has(CoreType, a.a_meta)) {
							throw ocaml.Not_found.instance;
						}
						// in which case we assume that a singular type parameter is the element type *)
						var t = switch (tl) {
							case [t]: t;
							case _: throw ocaml.Not_found.instance;
						}
						gen_int_iter(t, get_next_array_element, get_length);
					}
				}
				catch (_:ocaml.Not_found) {
					None;
				}
			case [_, TInst(c={cl_kind:KGenericInstance({cl_path:{a:["haxe", "ds"], b:"GenericStack"}}, [t])}, [])]:
				var tcell = try {
					PMap.find("head", c.cl_fields).cf_type;
				}
				catch (_:ocaml.Not_found) {
					trace("Shall not be seen"); Sys.exit(255); throw false;
				}
				var i = context.Typecore.add_local(ctx, i, t, p);
				var cell = context.Typecore.gen_local(ctx, tcell, p);
				var cexpr = core.Type.mk(TLocal(cell), tcell, p);
				var e2 = typing.Typer.type_expr(ctx, e2, NoValue);
				var evar = core.Type.mk(TVar(i, Some(core.Type.mk(mk_field(cexpr, "elt"), t, p))), t_void, pi);
				var enext = core.Type.mk(TBinop(OpAssign, cexpr, core.Type.mk(mk_field(cexpr, "next"), tcell, p)), tcell, p);
				var block = switch (e2.eexpr) {
					case TBlock(el): core.Type.mk(TBlock(evar::enext::el), t_void, e2.epos);
					case _: core.Type.mk(TBlock([evar, enext, e2]), t_void, p);
				}
				lblock([
					core.Type.mk(TVar(cell, Some(core.Type.mk(mk_field(e1, "head"), tcell, p))), t_void, p),
					core.Type.mk(TWhile(
						core.Type.mk(TBinop(OpNotEq, cexpr, core.Type.mk(TConst(TNull), tcell, p)), ctx.t.tbool, p),
						block,
						NormalWhile
					), t_void, p)
				]);
			case _: None;

		}
	}

	public static function optimize_for_loop_iterator (ctx:context.Typecore.Typer, v:core.Type.TVar, e1:core.Type.TExpr, e2:core.Type.TExpr, p:core.Globals.Pos) : core.Type.TExpr {
		var _tmp = switch (core.Type.follow(e1.etype)) { case TInst(c, pl): {fst:c, snd:pl}; case _: throw ocaml.Exit.instance; };
		var c = _tmp.fst; var tl = _tmp.snd;
		var fhasnext = try {
			core.Type.raw_class_field(function (cf) {
				return core.Type.apply_params(c.cl_params, tl, cf.cf_type);
			}, c, tl, "hasNext").trd;
		}
		catch (_:ocaml.Not_found) {
			throw ocaml.Exit.instance;
		}
		if (!fhasnext.cf_kind.match(Method(MethInline))) { throw ocaml.Exit.instance; }
		var tmp = context.Typecore.gen_local(ctx, e1.etype, e1.epos);
		var eit = core.Type.mk(TLocal(tmp), e1.etype, p);
		var ehasnext = context.Typecore.make_call(ctx, core.Type.mk(TField(eit, FInstance(c, tl, fhasnext)), TFun({args:[], ret:ctx.t.tbool}), p), [], ctx.t.tbool, p);
		var enext = core.Type.mk(TVar(v, Some(context.Typecore.make_call(ctx, core.Type.mk(TField(eit, core.Type.quick_field_dynamic(eit.etype, "next")), TFun({args:[], ret:v.v_type}), p), [], v.v_type, p))), ctx.t.tvoid, p);
		var eblock = switch (e2.eexpr) {
			case TBlock(el): e2.with({eexpr:core.Type.TExprExpr.TBlock(enext::el)});
			case _: core.Type.mk(TBlock([enext, e2]), ctx.t.tvoid, p);
		}
		return core.Type.mk(TBlock([
			core.Type.mk(TVar(tmp, Some(e1)), ctx.t.tvoid, p),
			core.Type.mk(TWhile(ehasnext, eblock, NormalWhile), ctx.t.tvoid, p)
		]), ctx.t.tvoid, p);
	}

	// ----------------------------------------------------------------------
	// SANITIZE

	/*
		makes sure that when an AST get generated to source code, it will not
		generate expressions that evaluate differently. It is then necessary to
		add parenthesises around some binary expressions when the AST does not
		correspond to the natural operand priority order for the platform
	*/
	// this is the standard C++ operator precedence, which is also used by both JS and PHP
	public static function standard_precedence (op:core.Ast.Binop) : {fst:Int, snd:Bool} {
		var left = true; var right = false;
		return switch(op) {
			case OpIn: {fst:4, snd:right};
			case OpMult, OpDiv, OpMod: {fst:5, snd:left};
			case OpAdd, OpSub: {fst:6, snd:left};
			case OpShl, OpShr, OpUShr: {fst:7, snd:left};
			case OpLt, OpLte, OpGt, OpGte: {fst:8, snd:left};
			case OpEq, OpNotEq: {fst:9, snd:left};
			case OpAnd: {fst:10, snd:left};
			case OpXor: {fst:11, snd:left};
			case OpOr: {fst:12, snd:left};
			case OpInterval: {fst:13, snd:right}; // haxe specific
			case OpBoolAnd: {fst:14, snd:left};
			case OpBoolOr: {fst:15, snd:left};
			case OpArrow: {fst:16, snd:left};
			case OpAssignOp(OpAssign): {fst:18, snd:right}; // mimics ?:
			case OpAssign, OpAssignOp(_): {fst:19, snd:right};
		}
	}

	public static function need_parent (e:core.Type.TExpr) : Bool {
		return switch(e.eexpr) {
			case TConst(_), TLocal(_), TArray(_), TField(_), TEnumParameter(_), TEnumIndex(_), TParenthesis(_), TCall(_), TNew(_), TTypeExpr(_), TObjectDecl(_), TArrayDecl(_), TIdent(_) : false;
			case TCast(e,None), TMeta(_,e): need_parent(e);
			case TCast(_), TThrow(_), TReturn(_), TTry(_), TSwitch(_), TFor(_), TIf(_), TWhile(_), TBinop(_), TContinue, TBreak, TBlock(_), TVar(_), TFunction(_), TUnop(_): true;
		}
	}

	public static function sanitize_expr (com:context.Common.Context, e:core.Type.TExpr) : core.Type.TExpr {
		function parent (e:core.Type.TExpr) : core.Type.TExpr {
			return switch (e.eexpr) {
				case TParenthesis(_): e;
				case _: core.Type.mk(TParenthesis(e), e.etype, e.epos);
			}
		}
		function block (e:core.Type.TExpr) : core.Type.TExpr {
			return switch (e.eexpr) {
				case TBlock(_): e;
				case _: core.Type.mk(TBlock([e]), e.etype, e.epos);
			}
		}
		function complex (e:core.Type.TExpr) : core.Type.TExpr {
			// complex expressions are the one that once generated to source consists in several expressions
			return switch (e.eexpr) {
				case TVar(_), // needs to be put into blocks
					TFor(_), // a temp var is needed for holding iterator
					TCall({eexpr:TIdent("__js__")}, _): // we never know
					block(e);
				case _: e;
			}
		}
		// tells if the printed expresssion ends with an if without else
		function has_if (e:core.Type.TExpr) : Bool {
			return switch (e.eexpr) {
				case TIf(_, _, None): true;
				case TWhile(_, e, NormalWhile): has_if(e);
				case TFor(_, _, e): has_if(e);
				case _: false;
			}
		}
		return switch (e.eexpr) {
			case TConst(TNull):
				if (com.config.pf_static && !core.Type.is_nullable(e.etype)) {
					function loop (t:core.Type.T) {
						return switch(core.Type.follow(t)) {
							case TMono(_): // in these cases the null will cast to default value
							case TFun(_): // this is a bit a particular case, maybe flash-specific actually
							// TODO: this should use get_underlying_type, but we do not have access to Codegen here.
							case TAbstract(a,tl) if (!core.Meta.has(CoreType, a.a_meta)): loop(core.Type.apply_params(a.a_params, tl, a.a_this));
							case _: com.error("On static platforms, null can't be used as basic type " + core.Type.s_type(core.Type.print_context(), e.etype), e.epos);
						}
					}
					loop(e.etype);
				}
				e;
			case TBinop(op, e1, e2):
				function swap (op1:core.Ast.Binop, op2:core.Ast.Binop) : Bool {
					var _tmp = standard_precedence(op1);
					var p1 = _tmp.fst; var left1 = _tmp.snd;
					var p2 = standard_precedence(op2).fst;
					return left1 && p1 <= p2;
				}
				function loop(ee:core.Type.TExpr, left:Bool) {
					return switch (ee.eexpr) {
						case TBinop(op2, _, _):
							if (left) {
								!swap(op2, op);
							}
							else {
								swap(op, op2);
							}
						case TIf(_):
							if (left) {
								!swap(OpAssignOp(OpAssign),  op);
							}
							else {
								swap(op, OpAssignOp(OpAssign));
							}
						case TCast(e, None), TMeta(_, e): loop(e, left);
						case _: false;
					}
				}
				var e1 = (loop(e1, true)) ? parent(e1) : e1;
				var e2 = (loop(e2, false)) ? parent(e2) : e2;
				e.with({eexpr: core.Type.TExprExpr.TBinop(op, e1, e2)});
			case TUnop(op, mode, e1):
				function loop(ee:core.Type.TExpr) : core.Type.TExpr {
					return switch (ee.eexpr) {
						case TBinop(_), TIf(_), TUnop(_): parent(e1);
						case TCast(e, None), TMeta(_, e): loop(e);
						case _: e1;
					}
				}
				e.with({eexpr: core.Type.TExprExpr.TUnop(op, mode, loop(e1))});
			case TIf(e1, e2, eelse):
				var e1 = parent(e1);
				var e2 = if ((eelse != None && has_if(e2)) || switch (e2.eexpr) {case TIf(_): true; case _: false; }) {
					block(e2);
				}
				else { complex(e2); }
				var eelse = switch (eelse) { case None: None; case Some(e): Some(complex(e));}
				e.with({eexpr: core.Type.TExprExpr.TIf(e1, e2, eelse)});
			case TWhile(e1, e2, flag):
				var e1 = parent(e1);
				var e2 = complex(e2);
				e.with({eexpr: core.Type.TExprExpr.TWhile(e1, e2, flag)});
			case TFor(v, e1, e2):
				var e2 = complex(e2);
				e.with({eexpr: core.Type.TExprExpr.TFor(v, e1, e2)});
			case TFunction(f):
				var f = switch (f.tf_expr.eexpr) {
					case TBlock(_): f;
					case _:
						f.with({tf_expr:block(f.tf_expr)});
				}
				e.with({eexpr: core.Type.TExprExpr.TFunction(f)});
			case TCall(e2, args):
				if (need_parent(e2)) {
					e.with({eexpr: core.Type.TExprExpr.TCall(parent(e2), args)});
				}
				else {
					e;
				}
			case TEnumParameter(e2, ef, i):
				if (need_parent(e2)) {
					e.with({eexpr: core.Type.TExprExpr.TEnumParameter(parent(e2), ef, i)});
				}
				else {
					e;
				}
			case TEnumIndex(e2):
				if (need_parent(e2)) {
					e.with({eexpr: core.Type.TExprExpr.TEnumIndex(parent(e2))});
				}
				else {
					e;
				}
			case TField(e2, f):
				if (need_parent(e2)) {
					e.with({eexpr: core.Type.TExprExpr.TField(parent(e2), f)});
				}
				else {
					e;
				}
			case TArray(e1, e2):
				if (need_parent(e1)) {
					e.with({eexpr: core.Type.TExprExpr.TArray(parent(e1), e2)});
				}
				else {
					e;
				}
			case TTry(e1, catches):
				var e1 = block(e1);
				var catches = List.map(function (c) { return {v:c.v, e:block(c.e)};}, catches);
				e.with({eexpr: core.Type.TExprExpr.TTry(e1, catches)});
			case TSwitch(e1, cases, def):
				var e1 = parent(e1);
				var cases = List.map(function (c) { return {values:c.values, e:complex(c.e)};}, cases);
				var def = switch (def) {
					case None: None;
					case Some(e): Some(complex(e));
				}
				e.with({eexpr: core.Type.TExprExpr.TSwitch(e1, cases, def)});
			case _: e;
		}

	}

	public static function reduce_expr (com:Any, e:core.Type.TExpr) : core.Type.TExpr {
		return switch (e.eexpr) {
			case TSwitch (_,cases,_):
				List.iter(function (_c) {
					List.iter (function (e:core.Type.TExpr) {
						switch (e.eexpr) {
							case TCall({eexpr:TField(_, FEnum(_))}, _): core.Error.error("Not-constant enum in switch cannot be matched", e.epos);
							case _:
						}
					}, _c.values);
				}, cases);
				e;
			case TBlock(l):
				switch (List.rev(l)) {
					case []: e;
					case ec::l:
						var l:ImmutableList<core.Type.TExpr> = l;
						// remove all no-ops : not-final constants in blocks
						function _f(e:core.Type.TExpr) {
							return switch (e.eexpr) {
								case TConst(_):false;
								case TBlock([]), TObjectDecl([]): false;
								case _: true;
							}
						}
						switch (List.filter(_f, l)) {
							case []: ec;
							case l:
								e.with({eexpr: core.Type.TExprExpr.TBlock(ocaml.List.rev(ec :: l))});
						}
				}
			case TParenthesis(ec):
				ec.with({epos:e.epos});
			case TTry(e,[]):
				e;
			case _:
				e;
		}
	}

	public static function sanitize (com:context.Common.Context, e:core.Type.TExpr) : core.Type.TExpr {
		return sanitize_expr(com, reduce_expr(com, core.Type.map_expr(sanitize.bind(com), e)));
	}

	// ----------------------------------------------------------------------
	// REDUCE

	public static function reduce_control_flow (ctx:context.Typecore.Typer, e:core.Type.TExpr) : core.Type.TExpr {
		return switch (e.eexpr) {
			case TIf({eexpr:TConst(TBool(t))}, e1, e2):
				if (t) { e1; }
				else {
					switch (e2) {
						case None:
							e.with({eexpr: core.Type.TExprExpr.TBlock([])});
						case Some(e): e;
					}
				}
			case TWhile({eexpr:TConst(TBool(false))}, sub, flag):
				switch (flag) {
					case NormalWhile:
						e.with({eexpr: core.Type.TExprExpr.TBlock([])}); // erase sub
					case DoWhile: e; // we cant remove while since sub can contain continue/break
				}
			case TSwitch(e1, cases, def):
				var e = switch (core.Texpr.skip(e1)) {
					case e1={eexpr:TConst(ct)}:
						function loop(cases:ImmutableList<{values:ImmutableList<core.Type.TExpr>, e:core.Type.TExpr}>) : core.Type.TExpr {
							return switch (cases) {
								case ({values:el, e:e})::cases:
									var cases:ImmutableList<{values:ImmutableList<core.Type.TExpr>, e:core.Type.TExpr}> = cases;
									if (List.exists(core.Texpr.equal.bind(e1), el)) {
										e;
									}
									else {
										loop(cases);
									}
								case []:
									switch (def) {
										case None: e;
										case Some(e): e;
									}
							}
						}
						loop(cases);
					case _: e;
				}
				e;
			case TBinop(op, e1, e2):
				optimization.OptimizerTexpr.optimize_binop(e, op, e1, e2);
			case TUnop(op, flag, esub):
				optimization.OptimizerTexpr.optimize_unop(e, op, flag, esub);
			case TCall(f={eexpr:TField(o, FClosure(c, cf))}, el):
				var fmode:core.Type.TFieldAccess = switch (c) {
					case None: FAnon(cf);
					case Some({c:c, params:tl}): FInstance(c, tl, cf);
				}
				e.with({eexpr:core.Type.TExprExpr.TCall(f.with({eexpr: core.Type.TExprExpr.TField(o, fmode)}), el)});
			case _: e;
		}
	}

	public static function reduce_loop (ctx:context.Typecore.Typer, e:core.Type.TExpr) : core.Type.TExpr {
		var e = core.Type.map_expr(reduce_loop.bind(ctx), e);
		return sanitize_expr(ctx.com, switch (e.eexpr) {
			case TCall(e1, el):
				switch (core.Texpr.skip(e1)) {
					case ef={eexpr:TFunction(func)}:
						var cf = core.Type.mk_field("", ef.etype, e.epos, core.Globals.null_pos);
						var ethis = core.Type.mk(TConst(TThis), core.Type.t_dynamic, e.epos);
						var rt = switch (core.Type.follow(ef.etype)) {
							case TFun({ret:rt}): rt;
							case _: trace("Shall not be seen"); throw false;
						}
						var inl = try {
							type_inline(ctx, cf, func, ethis, el, rt, None, e.epos, true, false);
						}
						catch (err:core.Error) {
							switch (err.msg) {
								case Custom(_): None;
								case _: throw err;
							}
						}
						switch (inl) {
							case None: reduce_expr(ctx, e);
							case Some(e): reduce_expr(ctx, e);
						}
					case {eexpr:TField({eexpr:TTypeExpr(TClassDecl(c))}, field)}:
						switch (api_inline(ctx, c, core.Type.field_name(field), el, e.epos)) {
							case None: reduce_expr(ctx, e);
							case Some(e): reduce_expr(ctx, e);
						}
					case _: reduce_expr(ctx, e);
				}
			case _:
				reduce_expr(ctx, reduce_control_flow(ctx, e));
		});
	}

	public static function reduce_expression (ctx:context.Typecore.Typer, e:core.Type.TExpr) : core.Type.TExpr {
		return (ctx.com.foptimize) ? reduce_loop(ctx, e) : e;
	}

	public static function make_constant_expression (ctx:context.Typecore.Typer, ?concat_strings:Bool=false, e:core.Type.TExpr) : Option<core.Type.TExpr> {
		var e = reduce_loop(ctx, e);
		return switch (e.eexpr) {
			case TConst(_): Some(e);
			case TBinop(op=(OpAdd|OpSub|OpMult|OpDiv|OpMod|OpShl|OpShr|OpUShr|OpOr|OpAnd|OpXor), e1, e2):
				switch ({fst:make_constant_expression(ctx, e1), snd:make_constant_expression(ctx, e2)}) {
					case {fst:Some({eexpr:TConst(TString(s1))}), snd:Some({eexpr:TConst(TString(s2))})} if (concat_strings):
						Some(core.Type.mk(TConst(TString(s1 + s2)), ctx.com.basic.tstring, core.Ast.punion(e1.epos, e2.epos)));
					case {fst:Some(e1), snd:Some(e2)}:
						Some(core.Type.mk(TBinop(op, e1, e2), e.etype, e.epos));
					case _: None;
				}
			case TUnop(op=(OpNeg|OpNegBits), Prefix, e1):
				switch (make_constant_expression(ctx, e1)) {
					case Some(e1): Some(core.Type.mk(TUnop(op, Prefix, e1), e.etype, e.epos));
					case None: None;
				}
			case TCast(e1, None):
				switch (make_constant_expression(ctx, e1)) {
					case None: None;
					case Some(e1):
						Some(e.with({eexpr:core.Type.TExprExpr.TCast(e1, None)}));
				}
			case TParenthesis(e1):
				switch (make_constant_expression(ctx, e1)) {
					case None: None;
					case Some(e1):
						Some(e.with({eexpr:core.Type.TExprExpr.TParenthesis(e1)}));
				}
			case TMeta(m, e1):
				switch (make_constant_expression(ctx, e1)) {
					case None: None;
					case Some(e1):
						Some(e.with({eexpr:core.Type.TExprExpr.TMeta(m, e1)}));
				}
			case TTypeExpr(_): Some(e);
			// try to inline static function calls
			// Disabled for now, see #4254.
			/* | TCall ({ etype = TFun(_,ret); eexpr = TField (_,FStatic (c,cf)) },el) ->
				(try
					let func = match cf.cf_expr with Some ({eexpr = TFunction func}) -> func | _ -> raise Not_found in
					let ethis = mk (TConst TThis) t_dynamic e.epos in
					let inl = (try type_inline ctx cf func ethis el ret None e.epos false with Error (Custom _,_) -> None) in
					(match inl with
					| None -> None
					| Some e -> make_constant_expression ctx e)
				with Not_found -> None) */
			case _: None;
		}
	}

	// ----------------------------------------------------------------------
	// INLINE CONSTRUCTORS
	// This version is disabled by default, use -D old-constructor-inline to use this

	/*
		First pass :
		We will look at local variables in the form   var v = new ....
		we only capture the ones which have constructors marked as inlined
		then we make sure that these locals are no more referenced except for fields accesses

		Second pass :
		We replace the variables by their fields lists, and the corresponding fields accesses as well
	*/

	// ----------------------------------------------------------------------
	// COMPLETION
	public static function optimize_completion_expr (e:core.Ast.Expr) : core.Ast.Expr {
		trace("Optimizer.optimize_completion_expr");
		throw false;
	}
}