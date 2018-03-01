package typing.matcher;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
import ocaml.Hashtbl;
import ocaml.List;
import ocaml.PMap;

using ocaml.Cloner;

class Not_exhaustive {
	public static final instance = new Not_exhaustive();
	function new () {}
}

typedef TCase = {fst:typing.matcher.constructor.T, snd:typing.matcher.decisiontree.Dt, trd:core.Type.TParams};

enum Match_kind {
	SKValue;
	SKEnum;
	SKFakeEnum;
	SKLength;
}

class TexprConverter {
	public static function unify_constructor (ctx:context.Typecore.Typer, params:core.Type.TParams, t:core.Type.T, con:typing.matcher.constructor.T) : Option<{con:typing.matcher.constructor.T, params:core.Type.TParams}> {
		trace("TODO: unify_constructors");
		throw false;
	}
	public static function all_ctors (ctx:context.Typecore.Typer, e:core.Type.TExpr, cases:ImmutableList<{fst:typing.matcher.constructor.T, snd:Bool, trd:typing.matcher.decisiontree.Dt}>) : {fst:core.Type.TExpr, snd:ImmutableList<typing.matcher.constructor.T>, trd:Match_kind, frth:typing.matcher.Decision_tree.Type_finiteness} {
		trace("TODO: all_ctors");
		throw false;
	}

	public static function report_not_exhaustive (e_subject:core.Type.TExpr, unmatched:ImmutableList<{con:typing.matcher.constructor.T, params:Dynamic}>) : Dynamic { // real = void always throw
		trace("TODO: report_not_exhaustive");
		throw false;
	}

	public static function to_texpr (ctx:context.Typecore.Typer, t_switch:core.Type.T, match_debug:Bool, with_type:context.Typecore.WithType, dt:typing.matcher.decisiontree.Dt) : core.Type.TExpr {
		var com = ctx.com;
		var p = dt.dt_pos;
		var c_type = switch(core.Type.follow(typing.Typeload.load_instance(ctx, {tp:{tpackage:["std"], tname:"Type", tparams:[], tsub:None}, pos:core.Globals.null_pos}, true , p))) {
			case TInst(c, _): c;
			case t: trace("Shall not be seen"); throw false;
		}
		function mk_index_call(e:core.Type.TExpr) : core.Type.TExpr {
			return
			if (!ctx.in_macro && !ctx.com.display.dms_full_typing) {
				/* If we are in display mode there's a chance that these fields don't exist. Let's just use a
					(correctly typed) neutral value because it doesn't actually matter. */
					core.Type.mk(TConst(TInt(0)), ctx.t.tint, e.epos);
			}
			else {
				core.Type.mk(TEnumIndex(e), com.basic.tint, e.epos);
			}
		}
		function mk_name_call(e:core.Type.TExpr) : core.Type.TExpr {
			return
			if (!ctx.in_macro && !ctx.com.display.dms_full_typing) {
				/* If we are in display mode there's a chance that these fields don't exist. Let's just use a
					(correctly typed) neutral value because it doesn't actually matter. */
					core.Type.mk(TConst(TString("")), ctx.t.tstring, e.epos);
			}
			else {
				var cf = PMap.find("enumConstructor", c_type.cl_statics);
				context.Typecore.make_static_call(ctx, c_type, cf, function (t) { return t; }, [e], com.basic.tstring, e.epos);
			}
		}
		function loop (toplevel:Bool, params:core.Type.TParams, dt:typing.matcher.decisiontree.Dt) : core.Type.TExpr {
			return switch (dt.dt_t) {
				case Leaf(case_):
					switch (case_.case_expr) {
						case Some(e): e;
						case None: core.Type.mk(TBlock([]), ctx.t.tvoid, case_.case_pos);
					}
				case Switch(_, [{fst:ConFields(_), trd:dt}], _): // TODO: Can we improve this by making it more general?
					loop(false, params, dt);
				case Switch(e_subject, cases, default_):
					var _tmp = all_ctors(ctx, e_subject, cases);
					var e_subject = _tmp.fst; var unmatched = _tmp.snd; var kind = _tmp.trd; var finiteness = _tmp.frth;
					var unmatched = List.filter_map(unify_constructor.bind(ctx, params, e_subject.etype), unmatched);
					function loop_ (toplevel:Bool, params:core.Type.TParams, dt:typing.matcher.decisiontree.Dt) : Option<core.Type.TExpr> {
						return
						try {
							Some(loop(toplevel, params, dt));
						}
						catch (_:Not_exhaustive) {
							switch [with_type, finiteness] {
								case [NoValue, Infinite]: None;
								case [_, CompileTimeFinite] if (unmatched == []): None;
								case _ if (ctx.com.display.dms_error_policy == EPIgnore): None;
								case _: report_not_exhaustive(e_subject, unmatched);
							}
						}
					}
					var cases = List.filter_map(function (c) : Option<TCase> {
						var con = c.fst; var dt = c.trd;
						return switch (unify_constructor(ctx, params, e_subject.etype, con)) {
							case Some({params:params}): Some({fst:con, snd:dt, trd:params});
							case None: None;
						}
					}, cases);
					function group (cases:ImmutableList<TCase>) {
						var h = new Map<typing.matcher.decisiontree.T, {fst:ImmutableList<typing.matcher.constructor.T>, snd:typing.matcher.decisiontree.Dt, trd:core.Type.TParams}>();
						List.iter(function (c:TCase) {
							var con = c.fst; var dt = c.snd; var params = c.trd;
							var l = try {
								Hashtbl.find(h, dt.dt_t).fst;
							}
							catch (_:ocaml.Not_found) {
								Tl;
							}
							Hashtbl.replace(h, dt.dt_t, {fst:con::l, snd:dt, trd:params});
						}, cases);
						return Hashtbl.fold(function (_, c, acc) {
							var cons = c.fst; var dt = c.snd; var params = c.trd;
							return {fst:cons, snd:dt, trd:params}::acc;
						}, h, []);
					}
					var cases = group(cases);
					var cases = List.sort(function (c1:{fst:ImmutableList<typing.matcher.constructor.T>, snd:typing.matcher.decisiontree.Dt, trd:core.Type.TParams}, c2:{fst:ImmutableList<typing.matcher.constructor.T>, snd:typing.matcher.decisiontree.Dt, trd:core.Type.TParams}) {
						var cons1 = c1.fst; var cons2 = c2.fst;
						return switch [cons1, cons2] {
							case [con1::_, con2::_]:
								typing.matcher.Constructor.compare(con1, con2);
							case _: -1;
						}
					}, cases);
					var e_default = switch [unmatched, finiteness] {
						case [[], RunTimeFinite]: None;
						case _: loop_(false, params, default_);
					}
					var cases = List.filter_map(function (c) {
						var cons = c.fst; var dt = c.snd; var params = c.trd;
						var eo = loop_(false, params, dt);
						return switch (eo) {
							case None: None;
							case Some(e):
								Some({values:List.map(Constructor.to_texpr.bind(ctx, match_debug, dt.dt_pos), List.sort(Constructor.compare, cons)), e:e});
						}
					}, cases);
					var e_subject = switch (kind) {
						case SKValue, SKFakeEnum: e_subject;
						case SKEnum:
							if (match_debug) {
								mk_name_call(e_subject);
							}
							else {
								mk_index_call(e_subject);
							}
						case SKLength:
							typing.Matcher.type_field_access(ctx, e_subject, "length");
					}
					switch (cases) {
						case [{e:e2}] if (e_default.match(None) && finiteness.match(RunTimeFinite)):
							var e2 = e2.clone(); e2.etype = t_switch;
							e2;
						case [{values:[e1], e:e2}] if ((with_type.match(NoValue) || !e_default.match(None)) && ctx.com.platform != Java) /* TODO: problem with TestJava.hx:285 */:
							var e_op = core.Type.mk(TBinop(OpEq, e_subject, e1), ctx.t.tbool, e_subject.epos);
							core.Type.mk(TIf(e_op, e2, e_default), t_switch, dt.dt_pos);
						case _:
							var e_subject = switch (finiteness) {
								case RunTimeFinite, CompileTimeFinite if (e_default == None):
									var meta:core.Ast.MetadataEntry = {name:Exhaustive, params:[], pos:dt.dt_pos};
									core.Type.mk(TMeta(meta, e_subject), e_subject.etype, e_subject.epos);
								case _:
									e_subject;
							}
							core.Type.mk(TSwitch(e_subject, cases, e_default), t_switch, dt.dt_pos);
					}
				case Guard(e, dt1, dt2):
					var e_then = loop(false, params, dt1);
					try {
						var e_else = loop(false, params, dt2);
						core.Type.mk(TIf(e, e_then, Some(e_else)), t_switch, core.Ast.punion(e_then.epos, e_else.epos));
					}
					catch (exc:Not_exhaustive) {
						if (with_type == NoValue) {
							core.Type.mk(TIf(e, e_then, None), ctx.t.tvoid, core.Ast.punion(e.epos, e_then.epos));
						}
						else {
							throw exc;
						}
					}
				case GuardNull(e, dt1, dt2):
					var e_null = core.Texpr.Builder.make_null(e.etype, e.epos);
					var f = try {
						var e_then = loop(false, params, dt1);
						function () {
							var e_else = loop(false, params, dt2);
							var e_op = core.Type.mk(TBinop(OpEq, e, e_null), ctx.t.tbool, e.epos);
							return core.Type.mk(TIf(e_op, e_then, Some(e_else)), t_switch, core.Ast.punion(e_then.epos, e_else.epos));
						}
					}
					catch (_:Not_exhaustive) {
						if (toplevel) {
							function () { return loop(false, params, dt2); }
						}
						else if (ctx.com.display.dms_error_policy == EPIgnore) {
							function () {
								return core.Type.mk(TConst(TNull), core.Type.mk_mono(), dt2.dt_pos);
							}
						}
						else {
							report_not_exhaustive(e, [{con:ConConst(TNull), params:dt.dt_pos}]);
						}
					}
					f();
				case Bind(bl, dt):
					var el = List.rev_map(function (arg:Compile.Var) {
						var v = arg.v; var p = arg.p; var e = arg.e;
						return core.Type.mk(TVar(v, Some(e)), com.basic.tvoid, p);
					}, bl);
					var e = loop(toplevel, params, dt);
					core.Type.mk(TBlock(List.append(el, [e])), e.etype, dt.dt_pos);
				case Fail: throw Not_exhaustive.instance;

			}
		}
		var params = List.map(function (tp) { return tp.t; }, ctx.type_params);
		var e = loop(true, params, dt);
		return core.Texpr.duplicate_tvars(e);
	}
}