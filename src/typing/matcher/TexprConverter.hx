package typing.matcher;

class Not_exhaustive {
	public static final instance = new Not_exhaustive();
	function new () {}
}

class TexprConverter {
	public static function to_texpr (ctx:context.Typecore.Typer, t_switch:core.Type.T, match_debug:Bool, with_type:context.Typecore.WithType, dt:typing.matcher.decisiontree.Dt) : core.Type.TExpr {
		trace("TODO: TexprConverter.to_texpr");
		throw false;
	}
}