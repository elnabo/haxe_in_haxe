package typing;

import haxe.ds.ImmutableList;
import haxe.ds.Option;

class MacroContext {

	public static function call_init_macro (ctx:context.Typecore.Typer, e:String) : Void {
		trace("TODO: typing.MacroContext.call_init_macro");
	}

	public static function type_macro (ctx:context.Typecore.Typer, mode:context.Typecore.MacroMode, cpath:core.Path, f:String, els:ImmutableList<core.Ast.Expr>, p:core.Globals.Pos) : Option<core.Ast.Expr> {
		trace("TODO typing.MacroContext.type_macro");
		throw false;
	}

	public static function setup () : Void {
		trace("TODO typing.MacroContext.setup");
	}
}