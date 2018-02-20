package context.display;

import haxe.ds.ImmutableList;
import ocaml.Ref;

class ImportHandling {

	public static function add_import_position (com:context.Common.Context, p:core.Globals.Pos, path:ImmutableList<core.Ast.PlacedName>) {
		com.shared.shared_display_information.import_positions.set(p, {b:new Ref(false), l:path});
	}

	public static function mark_import_position (com:context.Common.Context, p:core.Globals.Pos) : Void {
		var v = com.shared.shared_display_information.import_positions.get(p);
		if (v != null) {
			v.b.set(true);
		}
	}

	public static function maybe_mark_import_position (ctx:context.Typecore.Typer, p:core.Globals.Pos) : Void {
		if (context.display.Diagnostics.is_diagnostics_run(ctx)) {
			mark_import_position(ctx.com, p);
		}
	}
}