package typing.matcher;

import haxe.ds.ImmutableList;
import haxe.ds.Option;

import ocaml.List;

class Match {
	public static function match_expr (ctx:context.Typecore.Typer, e:core.Ast.Expr, cases:ImmutableList<core.Ast.Case>, def:Option<{e:Option<core.Ast.Expr>, pos:core.Globals.Pos}>, with_type:context.Typecore.WithType, p:core.Globals.Pos) : core.Type.TExpr {
		trace("typing.matcher.Match.match_expr");
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

		throw false;
	}
}