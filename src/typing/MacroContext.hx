package typing;

import haxe.ds.Option;

class MacroContext {

	public static function call_init_macro (ctx:context.Typecore.Typer) : Void {
		trace("TODO: typing.MacroContext.call_init_macro");
	}

	public static function type_macro (ctx:context.Typecore.Typer, mode:context.Typecore.MacroMode, cpath:core.Path, f:String, els:Array<core.Ast.Expr>, p:core.Globals.Pos) : Option<core.Ast.Expr> {
		trace("TODO typing.MacroContext.type_macro");
		return null;
	}

	public static function setup () : Void {
		trace("TODO typing.MacroContext.setup");
	}
}