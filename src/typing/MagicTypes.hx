package typing;

class MagicTypes {

	public static function extend_remoting (ctx:context.Typecore.Typer, c:core.Type.TClass, t:core.Ast.TypePath, p:core.Globals.Pos, async:Bool, prot:Bool) {
		trace("TODO: typing.MagicTypes.extend_remoting");
	}
	
	public static function extend_xml_proxy (ctx:context.Typecore.Typer, c:core.Type.TClass, t:core.Ast.ComplexType, file:String, p:core.Globals.Pos) {
		trace("TODO: typing.MagicTypes.extend_xml_proxy");
	}

	public static function on_inherit (ctx:context.Typecore.Typer, c:core.Type.TClass, p:core.Globals.Pos, r:{is_extends:Bool, tp:core.Ast.PlacedTypePath}) : Bool {
		if (!r.is_extends) { return true; }
		return switch(r.tp.tp) {
			case { tpackage: ["haxe","remoting"],
				   tname: "Proxy",
				   tparams: [TPType({ct:CTPath(t),pos:{pfile:"?", pmin:-1, pmax:-1}})] }:
				extend_remoting(ctx, c, t, p, false, true);
				false;
			case { tpackage: ["haxe","remoting"],
				   tname: "AsyncProxy",
				   tparams: [TPType({ct:CTPath(t),pos:{pfile:"?", pmin:-1, pmax:-1}})] }:
				extend_remoting(ctx, c, t, p, true, true);
				false;
			case { tpackage: ["haxe","xml"],
				   tname: "Proxy",
				   tparams: [TPExpr({expr:EConst(CString(file)), pos:p}), TPType({ct:t, pos:_})] }:
				extend_xml_proxy(ctx, c, t, file, p);
				true;
			default:
				true;
		};
	}
}