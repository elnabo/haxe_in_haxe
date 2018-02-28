package typing.matcher;

import haxe.ds.ImmutableList;
import ocaml.Hashtbl;
import ocaml.List;
import ocaml.Ref;

typedef Matcher_context = {
	ctx:context.Typecore.Typer,
	dt_table:Map<typing.matcher.decisiontree.T, typing.matcher.decisiontree.Dt>, // dt DtTable.t;
	match_pos: core.Globals.Pos,
	match_debug: Bool,
	dt_count: Int
}

typedef Var = {fst:core.Type.TVar, snd:core.Globals.Pos, trd:core.Type.TExpr};
class Extractor {
	public static final instance = new Extractor();
	function new () {}
}

class Compile {

	public static function sort<A>(i:Int, cur:Int, acc:ImmutableList<A>, l:ImmutableList<A>) : ImmutableList<A> {
		return switch (l) {
			case x::l :
				if (i == cur) {
					x :: List.append(acc, l);
				}
				else {
					sort(i, cur+1, x::acc, l);
				}
			case []: acc;
		}
	}

	public static function hashcons (mctx:Matcher_context, dt:typing.matcher.decisiontree.T, p:core.Globals.Pos) : typing.matcher.decisiontree.Dt {
		return
		try {
			Hashtbl.find(mctx.dt_table, dt);
		}
		catch (_:ocaml.Not_found) {
			var dti = {
				dt_t: dt,
				dt_i: mctx.dt_count,
				dt_pos: p,
				dt_goto_target: false
			};
			Hashtbl.add(mctx.dt_table, dt, dti);
			mctx.dt_count++;
			dti;
		}
	}

	public static function leaf (mctx:Matcher_context, case_:typing.matcher.Case.T) : typing.matcher.decisiontree.Dt {
		return hashcons(mctx, Leaf(case_), case_.case_pos);
	}
	public static function fail (mctx:Matcher_context, p:core.Globals.Pos) : typing.matcher.decisiontree.Dt {
		return hashcons(mctx, Fail, p);
	}
	public static function switch_ (mctx:Matcher_context, subject:core.Type.TExpr, cases:ImmutableList<{fst:typing.matcher.constructor.T, snd:Bool, trd:typing.matcher.decisiontree.Dt}>, _default:typing.matcher.decisiontree.Dt) :typing.matcher.decisiontree.Dt {
		return hashcons(mctx, Switch(subject, cases, _default), subject.epos);
	}
	public static function bind (mctx:Matcher_context, bindings:ImmutableList<Var>, dt:typing.matcher.decisiontree.Dt) : typing.matcher.decisiontree.Dt {
		return hashcons(mctx, Bind(bindings, dt), dt.dt_pos);
	}
	public static function guard (mctx:Matcher_context, e:core.Type.TExpr, dt1:typing.matcher.decisiontree.Dt, dt2:typing.matcher.decisiontree.Dt) : typing.matcher.decisiontree.Dt {
		return hashcons(mctx, Guard(e, dt1, dt2), core.Ast.punion(dt1.dt_pos, dt2.dt_pos));
	}
	public static function guard_null (mctx:Matcher_context, e:core.Type.TExpr, dt1:typing.matcher.decisiontree.Dt, dt2:typing.matcher.decisiontree.Dt) : typing.matcher.decisiontree.Dt {
		return hashcons(mctx, GuardNull(e, dt1, dt2), core.Ast.punion(dt1.dt_pos, dt2.dt_pos));
	}

	public static function get_sub_subjects(mctx:Matcher_context, e:core.Type.TExpr, con:typing.matcher.constructor.T) : ImmutableList<core.Type.TExpr> {
		return switch (con) {
			case ConEnum(en, ef):
				var tl = List.map(function (_) { return core.Type.mk_mono(); }, en.e_params);
				var t_en:core.Type.T = TEnum(en, tl);
				var e = (!core.Type.type_iseq(t_en, e.etype)) ? core.Type.mk(TCast(e, None), t_en, e.epos) : e;
				switch (core.Type.follow(ef.ef_type)) {
					case TFun({args:args}):
						List.mapi(function (i, arg) {
							var t = arg.t;
							return core.Type.mk(TEnumParameter(e, ef, i), core.Type.apply_params(en.e_params, tl, core.Type.monomorphs(ef.ef_params, t)), e.epos);
						}, args);
					case _: [];
				}
			case ConFields(sl):
				List.map(typing.Matcher.type_field_access.bind(mctx.ctx, false, e), sl);
			case ConArray(0): [];
			case ConArray(i):
				var t = switch (core.Type.follow(e.etype)) {
					case TInst({cl_path:{a:[], b:"Array"}}, [t]): t;
					case t=TDynamic(_): t;
					case _: trace("Shall not be seen"); Sys.exit(255); throw false;
				}
				List.init(i, function (i:Int):core.Type.TExpr {
					var ei = core.Texpr.Builder.make_int(mctx.ctx.com.basic, i, e.epos);
					return core.Type.mk(TArray(e, ei), t, e.epos);
				});
			case ConConst(_), ConTypeExpr(_), ConStatic(_): [];
		}
	}

	public static function specialize (subject:core.Type.TExpr, con:typing.matcher.constructor.T, cases:ImmutableList<Case>) : ImmutableList<Case> {
		trace("TODO: Compile.specialize");
		// var arity = typing.matcher.Constructor.arity(con);
		// function loop(acc:ImmutableList<Case>, cases:ImmutableList<Case>) :ImmutableList<Case> {
		// 	return switch (cases) {
		// 		case {fst:case_, snd:bindings, trd:patterns}::cases:
		// 			switch (patterns) {
		// 				case {t:PatConstructor(con_, patterns1)}::patterns2 if ():
		// 			}
		// 		case []: List.rev(acc);
		// 	}
		// }
		// return loop([], cases);
		throw false;
	}

	public static function default_ (subject:core.Type.TExpr, cases:ImmutableList<Case>) : ImmutableList<Case> {
		trace("TODO: Compile.default_");
		throw false;
	}

	public static function is_wildcard_pattern (pat:Pattern) {
		return pat.t.match(PatVariable(_)|PatAny);
	}

	public static function expand (cases:ImmutableList<Case>) :ImmutableList<Case>{
		var _tmp = List.fold_left(function (a:{fst:Bool, snd:ImmutableList<Case>}, c:Case) : {fst:Bool, snd:ImmutableList<Case>} {
			var changed = a.fst; var acc = a.snd;
			var case_ = c.fst; var bindings = c.snd; var patterns = c.trd;
			function loop (f:Pattern->Pattern, patterns:ImmutableList<Pattern>) {
				return switch (patterns) {
					case {t:PatOr(pat1, pat2)}::patterns:
						var _tmp:Case = {fst:case_, snd:bindings, trd:f(pat2)::patterns};
						{fst:true, snd:_tmp::acc};
					case {t:PatBind(v, pat1), pos:p}::patterns:
						loop(function (pat2) { return f({t:PatBind(v, pat2), pos:p}); }, (pat1::patterns));
					case {t:PatTuple(patterns1)}::patterns2:
						loop(f, List.append(patterns1, patterns2));
					case pat::patterns:
						var _tmp:Case = {fst:case_, snd:bindings, trd:f(pat)::patterns};
						{fst:changed, snd:_tmp::acc};
					case []:
						var _tmp:Case = {fst:case_, snd:bindings, trd:patterns};
						{fst:changed, snd:_tmp::acc};
				}
			}
			return loop(function (pat) {return pat;}, patterns);
		}, {fst:false, snd:[]}, cases);
		var changed = _tmp.fst; var cases = _tmp.snd;
		cases = List.rev(cases);
		return
		if (changed) {
			expand(cases);
		}
		else {
			cases;
		}
	}

	public static function s_subjects (subjects:ImmutableList<core.Type.TExpr>) : String {
		return List.join(" ", List.map(typing.Matcher.s_expr_pretty, subjects));
	}

	public static function s_case (c:Case) : String {
		trace("TODO Compile.s_case");
		throw false;
	}

	public static function s_cases(cases:ImmutableList<Case>) : String {
		return List.join("\n", List.map(s_case, cases));
	}

	public static function select_column (subjects:ImmutableList<core.Type.TExpr>, cases:ImmutableList<Case>) : {fst:ImmutableList<core.Type.TExpr>, snd:ImmutableList<Case>} {
		function loop (i:Int, patterns:ImmutableList<Pattern>) : Int {
			return switch (patterns) {
				case {t:(PatVariable(_)|PatAny|PatExtractor(_))} :: patterns:
					loop(i+1, patterns);
				case []: 0;
				case _: i;
			}
		}
		var patterns = List.hd(cases).trd;
		var i = loop(0, patterns);
		var _tmp = if (i==0) {
			{fst:subjects, snd:cases};
		}
		else {
			var subjects = sort(i, 0, [], subjects);
			var cases = List.map(function (c:Case):Case {
				var case_ = c.fst; var bindings = c.snd; var patterns = c.trd;
				var patterns = sort(i, 0, [], patterns);
				return {fst:case_, snd:bindings, trd:patterns};
			}, cases);
			{fst:subjects, snd:cases};
		}
		var subjects = _tmp.fst; var cases = _tmp.snd;
		return {fst:subjects, snd:cases};
	}

	public static function compile_ (mctx:Matcher_context, subjects:ImmutableList<core.Type.TExpr>, cases:ImmutableList<Case>) : typing.matcher.decisiontree.Dt {
		return switch (cases) {
			case []:
				fail(mctx, switch (subjects) {
					case e::_: e.epos;
					case _: mctx.match_pos;
				});
			case (case_={trd:patterns})::cases if (List.for_all(is_wildcard_pattern, patterns)):
				compile_leaf(mctx, subjects, case_, cases);
			case _:
				var cases = expand(cases);
				var _tmp = select_column(subjects, cases);
				var subjects = _tmp.fst; var cases = _tmp.snd;
				var cases = expand(cases); // TODO: is this really necessary?
				try {
					compile_switch(mctx, subjects, cases);
				}
				catch (_:Extractor) {
					compile_extractors(mctx, subjects, cases);
				}
		}
	}

	public static function compile_leaf (mctx:Matcher_context, subjects:ImmutableList<core.Type.TExpr>, _case:Case, cases:ImmutableList<Case>) : typing.matcher.decisiontree.Dt {
		var case_ = _case.fst; var bindings = _case.snd; var patterns = _case.trd;
		trace("TODO: Compile.compile_leaf");
		throw false;
	}

	public static function compile_switch (mctx:Matcher_context, subjects:ImmutableList<core.Type.TExpr>, cases:ImmutableList<Case>) : typing.matcher.decisiontree.Dt {
		var _tmp = switch (subjects) {
			case []: throw typing.Matcher.Internal_match_failure.instance;
			case subject::subjects: {fst:subject, snd:subjects};
		}
		var subject = _tmp.fst; var subjects = _tmp.snd;
		function get_column_sigma (cases:ImmutableList<Case>) {
			var sigma = new Map<typing.matcher.constructor.T, Bool>();
			var unguarded = new Map<typing.matcher.constructor.T, Bool>();
			var null_ = new Ref<ImmutableList<Case>>([]);
			List.iter(function (c:Case) {
				var case_ = c.fst; var bindings = c.snd; var patterns = c.trd;
				function loop(pat:Pattern) {
					switch (pat.t) {
						case PatConstructor(ConConst(TNull), _):
							null_.set(({fst:case_, snd:bindings, trd:List.tl(patterns)} : Case)::null_.get());
						case PatConstructor(con, _):
							if (case_.case_guard == None) {
								Hashtbl.replace(unguarded, con, true);
							}
							Hashtbl.replace(sigma, con, true);
						case PatBind(_, pat): loop(pat);
						case PatVariable(_), PatAny:
						case PatExtractor(_): throw Extractor.instance;
						case _: core.Error.error("Unexpected pattern: "+Pattern.to_string(pat), case_.case_pos);
					}
				}
				loop(List.hd(patterns));
			}, cases);
			var sigma = Hashtbl.fold(function (con:typing.matcher.constructor.T, _, acc) {
				return {fst:con, snd:Hashtbl.mem(unguarded, con)}::acc;
			}, sigma, []);
			return {fst:sigma, snd:List.rev(null_.get())};
		}
		var _tmp = get_column_sigma(cases);
		var sigma = _tmp.fst; var null_ = _tmp.snd;
		if (mctx.match_debug) {
			Sys.println('compile_switch:\n\tsubject: ${typing.Matcher.s_expr_pretty(subject)}\n\ttsubjects: ${s_subjects(subjects)}\n\tcases: ${s_cases(cases)}');
		}
		var switch_cases = List.map(function (s:{fst:typing.matcher.constructor.T, snd:Bool}) {
			var con = s.fst; var unguarded = s.snd;
			var subjects = List.append(get_sub_subjects(mctx, subject, con), subjects);
			var spec = specialize(subject, con, cases);
			var dt = compile_(mctx, subjects, spec);
			return {fst:con, snd:unguarded, trd:dt};

		}, sigma);
		var _default = default_(subject, cases);
		var switch_default = compile_(mctx, subjects, _default);
		var dt = (switch_cases == []) ? switch_default : switch_(mctx, subject, switch_cases, switch_default);
		function null_guard (dt_null) {
			return guard_null(mctx, subject, dt_null, dt);
		}
		return switch (null_) {
			case []:
				(core.Type.is_explicit_null(subject.etype)) ? null_guard(switch_default) : dt;
			case cases:
				var dt_null = compile_(mctx, subjects, List.append(cases, _default));
				null_guard(dt_null);
		}
	}

	public static function compile_extractors (mctx:Matcher_context, subjects:ImmutableList<core.Type.TExpr>, cases:ImmutableList<Case>) : typing.matcher.decisiontree.Dt {
		trace("TODO: Compile.compile_extractors");
		throw false;
	}

	public static function compile (ctx:context.Typecore.Typer, match_debug:Bool, subjects:ImmutableList<core.Type.TExpr>, cases:ImmutableList<Case>, p:core.Globals.Pos) : typing.matcher.decisiontree.Dt {
		var mctx:Matcher_context = {
			ctx: ctx,
			match_debug: match_debug,
			dt_table: new Map<typing.matcher.decisiontree.T, typing.matcher.decisiontree.Dt>(),
			match_pos: p,
			dt_count: 0
		};
		var _tmp = List.fold_left(function (arg:{fst:ImmutableList<core.Type.TExpr>, snd:ImmutableList<Var>}, e:core.Type.TExpr) {
			var subjects = arg.fst; var vars = arg.snd;
			return switch (e.eexpr) {
				case TConst(_), TLocal(_):
					{fst:e::subjects, snd:vars};
				case _:
					var v = context.Typecore.gen_local(ctx, e.etype, e.epos);
					var ev = core.Type.mk(TLocal(v), e.etype, e.epos);
					{fst:ev::subjects, snd:{fst:v, snd:e.epos, trd:e}::vars};
			}
		}, {fst:[], snd:[]}, subjects);
		var subjects = _tmp.fst; var vars = _tmp.snd;
		var dt = compile_(mctx, subjects, cases);
		Useless.check(mctx.ctx.com, cases);
		return switch (vars) {
			case []: dt;
			case _: bind(mctx, vars, dt);
		}
	}
}