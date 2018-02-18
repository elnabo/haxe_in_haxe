package context.display;

class Diagnostics {
	public static function is_diagnostics_run (ctx:context.Typecore.Typer) : Bool {
		return switch (ctx.com.display.dms_kind) {
			case DMDiagnostics(true): true;
			case DMDiagnostics(false): ctx.is_display_file;
			case _: false;
		};
	}
	public static function secure_generated_code (ctx:context.Typecore.Typer, e:core.Type.TExpr) : core.Type.TExpr {
		return if (is_diagnostics_run(ctx)) {
			core.Type.mk(TMeta({name:Extern, params:[], pos:e.epos}, e), e.etype, e.epos);
		}
		else {
			e;
		}
	}
}