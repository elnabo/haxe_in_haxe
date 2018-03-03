package context.display;

import haxe.ds.ImmutableList;
import ocaml.PMap;
import ocaml.Ref;

class ImportHandling {

	public static function add_import_position (com:context.Common.Context, p:core.Globals.Pos, path:ImmutableList<core.Ast.PlacedName>) {
		PMap.add(p, {b:new Ref(false), l:path}, com.shared.shared_display_information.import_positions);
	}

	public static function mark_import_position (com:context.Common.Context, p:core.Globals.Pos) : Void {
		try {
			var r = PMap.find(p, com.shared.shared_display_information.import_positions).b;
			r.set(true);
		}
		catch (_:ocaml.Not_found) {}
	}

	public static function maybe_mark_import_position (ctx:context.Typecore.Typer, p:core.Globals.Pos) : Void {
		if (context.display.Diagnostics.is_diagnostics_run(ctx)) {
			mark_import_position(ctx.com, p);
		}
	}
}