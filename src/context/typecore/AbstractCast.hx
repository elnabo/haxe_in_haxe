package context.typecore;

class AbstractCast {
	public static function do_check_cast (ctx:context.Typecore.Typer, tleft:core.Type.T, eright:core.Type.TExpr, p:core.Globals.Pos) : core.Type.TExpr {
		trace("TODO: context.typecore.AbstractCast.do_check_cast");
		throw false;
	}

	public static function cast_or_unify_raise (ctx:context.Typecore.Typer, tleft:core.Type.T, eright:core.Type.TExpr, p:core.Globals.Pos) : core.Type.TExpr {
		return try {
			// can't do that anymore because this might miss macro calls (#4315)
			// if ctx.com.display <> DMNone then raise Not_found;
			do_check_cast(ctx, tleft, eright, p);
		}
		catch (_:ocaml.Not_found) {
			context.Typecore.unify_raise(ctx, eright.etype, tleft, p);
			eright;
		}
	}
	public static function cast_or_unify (ctx:context.Typecore.Typer, tleft:core.Type.T, eright:core.Type.TExpr, p:core.Globals.Pos) : core.Type.TExpr {
		return try {
			cast_or_unify_raise(ctx, tleft, eright, p);
		}
		catch (err:core.Error) {
			var p = err.pos;
			switch (err.msg) {
				case Unify(l):
					context.Typecore.raise_or_display(ctx, l, p);
					eright;
				case _: throw err;
			}
		}
	}
}