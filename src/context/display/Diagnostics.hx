package context.display;

class Diagnostics {
	public static function is_diagnostics_run (ctx:context.Typecore.Typer) : Bool {
		return switch (ctx.com.display.dms_kind) {
			case DMDiagnostics(true): true;
			case DMDiagnostics(false): ctx.is_display_file;
			case _: false;
		};
	}
}