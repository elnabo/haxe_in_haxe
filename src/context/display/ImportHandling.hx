package context.display;

import ocaml.Ref;

class ImportHandling {

	public static function add_import_position (com:context.Common.Context, p:core.Globals.Pos, path:Array<core.Ast.PlacedName>) {
		com.shared.shared_display_information.import_positions.set(p, {b:new Ref(false), l:path});
	}

	public static function mark_import_position (com:context.Common.Context, p:core.Globals.Pos) {
		var v = com.shared.shared_display_information.import_positions.get(p);
		if (v != null) {
			v.b.set(true);
		}
	}
}