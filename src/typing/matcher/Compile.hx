package typing.matcher;

import haxe.ds.ImmutableList;

class Compile {

	public static function compile_ (mctx, subjects, cases) : typing.matcher.decisiontree.Dt {
		trace("TODO: compile_");
		throw false;
	}

	public static function compile (ctx:context.Typecore.Typer, match_debug:Bool, subjects:ImmutableList<core.Type.TExpr>, cases:ImmutableList<{fst:typing.matcher.Case.T, snd:ImmutableList<Any>, trd:ImmutableList<typing.matcher.Pattern>}>, p:core.Globals.Pos) : typing.matcher.decisiontree.Dt {
		trace("TODO: compile");
		throw false;
	}
}