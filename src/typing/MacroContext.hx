package typing;

import haxe.ds.ImmutableList;
import haxe.ds.Option;

import ocaml.PMap;

class MacroContext {

	public static function get_stored_typed_expr (com:context.Common.Context, id:Int) : core.Type.TExpr {
		var e = PMap.find(id, com.stored_typed_exprs);
		return core.Texpr.duplicate_tvars(e);
	}

	public static function call_init_macro (ctx:context.Typecore.Typer, e:String) : Void {
		trace("TODO: typing.MacroContext.call_init_macro");
	}

	public static function type_macro (ctx:context.Typecore.Typer, mode:context.Typecore.MacroMode, cpath:core.Path, f:String, els:ImmutableList<core.Ast.Expr>, p:core.Globals.Pos) : Option<core.Ast.Expr> {
		trace("TODO typing.MacroContext.type_macro");
		throw false;
	}

	public static function interpret (ctx:context.Typecore.Typer) : Void {
		trace("TODO: typing.MacroContext.interpret");
		throw false;
	}

	public static function setup () : Void {
		trace("TODO typing.MacroContext.setup");
	}
}