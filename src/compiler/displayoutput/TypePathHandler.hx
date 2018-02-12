package compiler.displayoutput;

import haxe.ds.ImmutableList;
import haxe.ds.Option;

class TypePathHandler {

	public static function complete_type_path (com:context.Common.Context, p:ImmutableList<String>) : Option<ImmutableList<{name:String, kind:context.Display.DisplayFieldKind, doc:String}>> {
		trace("TODO: compiler.displayoutput.TypePathHandler.complete_type_path");
		return null;
	}

	public static function complete_type_path_inner (com:context.Common.Context, p:ImmutableList<String>, c:String, cur_package:Bool, is_import:Bool) : Option<ImmutableList<{name:String, kind:context.Display.DisplayFieldKind, doc:String}>> {
		trace("TODO: compiler.displayoutput.TypePathHandler.complete_type_path");
		return null;
	}
}