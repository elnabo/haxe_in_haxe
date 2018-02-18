package typing.matcher;

import haxe.ds.ImmutableList;
import haxe.ds.Option;

class Match {
	public static function match_expr (ctx:context.Typecore.Typer, e:core.Ast.Expr, cases:ImmutableList<core.Ast.Case>, def:Option<{e:Option<core.Ast.Expr>, pos:core.Globals.Pos}>, with_type:context.Typecore.WithType, p:core.Globals.Pos) : core.Type.TExpr {
		trace("typing.matcher.Match.match_expr");
		throw false;
	}
}