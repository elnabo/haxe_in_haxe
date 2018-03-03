package typing.matcher;

import haxe.ds.ImmutableList;
import haxe.ds.Option;

import ocaml.List;
using equals.Equal;
using ocaml.Cloner;

class Match {
	public static function match_expr (ctx:context.Typecore.Typer, e:core.Ast.Expr, cases:ImmutableList<core.Ast.Case>, def:Option<{e:Option<core.Ast.Expr>, pos:core.Globals.Pos}>, with_type:context.Typecore.WithType, p:core.Globals.Pos) : core.Type.TExpr {
		var match_debug = core.Meta.has(Custom(":matchDebug"), ctx.curfield.cf_meta);
		function loop (e:core.Ast.Expr) : {fst:core.Type.T, snd:ImmutableList<core.Type.TExpr>} {
			return switch (e.expr) {
				case EArrayDecl(el) if (switch (el) { case [({expr:EFor(_,_)}|{expr:EWhile(_,_,_)})]: false; case _: true;}):
					var el = List.map(function (e:core.Ast.Expr) { return typing.Typer.type_expr(ctx, e, Value); }, el);
					var t = typing.Matcher.tuple_type(List.map(function(e:core.Type.TExpr) { return e.etype; }, el));
					{fst:t, snd:el};
				case EParenthesis(e1):
					loop(e1);
				case _:
					var e = typing.Typer.type_expr(ctx, e, Value);
					{fst:e.etype, snd:[e]};
			}
		}
		var _tmp = loop(e); var t = _tmp.fst; var subjects = _tmp.snd;
		var subjects = List.rev(subjects);
		var cases = switch (def) {
			case None:
				cases;
			case Some({e:eo, pos:p}) :
				var _tmp:core.Ast.Case = {values:[{expr:EConst(CIdent("_")), pos:p}], guard:None, expr:eo, pos:p};
				List.append(cases, [_tmp]);
		}
		var _tmp = switch (with_type) {
			case WithType(t):
				switch (core.Type.follow(t)) {
					case TMono(_): {fst:Some(t), snd:context.Typecore.WithType.Value}
					case _:  {fst:None, snd:with_type};
				}
			case _: {fst:None, snd:with_type};
		}
		var tmono = _tmp.fst; var with_type = _tmp.snd;
		var cases = List.map(function (c:core.Ast.Case) : typing.matcher.Case { //{fst:typing.matcher.Case.T, snd:ImmutableList<Any>, trd:ImmutableList<typing.matcher.Pattern>} {
			var el = c.values; var eg = c.guard; var eo = c.expr; var p = c.pos;
			var p = switch (eo) {
				case Some(e) if (p.equals(core.Globals.null_pos)):
					e.pos;
				case _:
					p;
			}
			var _tmp = typing.matcher.Case.make(ctx, t, el, eg, eo, with_type, p);
			var case_ = _tmp.fst; var bindings = _tmp.snd; var pat = _tmp.trd;
			return {fst:case_, snd:bindings, trd:[pat]};
		}, cases);
		function infer_switch_type () : core.Type.T {
			return switch (with_type) {
				case NoValue: core.Type.mk_mono();
				case Value:
					var el = List.map(function(c:typing.matcher.Case) {
						var case_ = c.fst;
						return switch (case_.case_expr) {
							case Some(e): e;
							case None: core.Type.mk(TBlock([]), ctx.t.tvoid, p);
						}
					}, cases);
					typing.Typer.unify_min(ctx, el);
				case WithType(t): t;
			}
		}
		if (match_debug) {
			Sys.println("CASES BEGIN");
			List.iter(function (c:typing.matcher.Case) {
				var patterns = c.trd;
				Sys.println(List.join(",", List.map(typing.matcher.Pattern.to_string, patterns)));
			}, cases);
			Sys.println("CASES END");
		}
		var dt = typing.matcher.Compile.compile(ctx, match_debug, subjects, cases, p);
		if (match_debug) {
			Sys.println("DECISION TREE BEGIN");
			Sys.println(typing.matcher.Decision_tree.to_string("", dt));
			Sys.println("DECISION TREE END");
		}
		var e = try {
			var t_switch = infer_switch_type();
			switch (tmono) {
				case Some(t): context.Typecore.unify(ctx, t_switch, t, p);
				case _:
			}
			typing.matcher.TexprConverter.to_texpr(ctx, t_switch, match_debug, with_type, dt);
		}
		catch (_:typing.matcher.TexprConverter.Not_exhaustive) {
			core.Error.error("Unmatched patterns: _", p);
		}
		if (match_debug) {
			Sys.println("TEXPR BEGIN");
			Sys.println(typing.Matcher.s_expr_pretty(e));
			Sys.println("TEXPR END");
		}
		return e.with({epos:p});
	}
}