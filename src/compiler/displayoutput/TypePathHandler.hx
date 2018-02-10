package compiler.displayoutput;

import haxe.ds.Option;

class TypePathHandler {

	public static function complete_type_path (com:context.Common.Context, p:Array<String>) : Option<Array<{name:String, kind:context.Display.DisplayFieldKind, doc:String}>> {
		trace("TODO: compiler.displayoutput.TypePathHandler.complete_type_path");
		return null;
	}

	public static function complete_type_path_inner (com:context.Common.Context, p:Array<String>, c:String, cur_package:Bool, is_import:Bool) : Option<Array<{name:String, kind:context.Display.DisplayFieldKind, doc:String}>> {
		trace("TODO: compiler.displayoutput.TypePathHandler.complete_type_path");
		return null;
	}
}