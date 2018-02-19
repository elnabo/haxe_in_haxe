package context.typecore;

import haxe.ds.Option;

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

	public static function find_array_access_raise(ctx:context.Typecore.Typer, a:core.Type.TAbstract, tl:core.Type.TParams, e1:core.Type.TExpr, e2o:Option<core.Type.TExpr>, p:core.Globals.Pos) : {cf:core.Type.TClassField, tf:core.Type.T, r:core.Type.T, e1:core.Type.TExpr, e2o:Option<core.Type.TExpr>} {
		trace("TODO: context.typecore.AbstractCast.find_array_access_raise");
		throw false;
	}

	public static function find_array_access(ctx:context.Typecore.Typer, a:core.Type.TAbstract, tl:core.Type.TParams, e1:core.Type.TExpr, e2o:Option<core.Type.TExpr>, p:core.Globals.Pos) : {cf:core.Type.TClassField, tf:core.Type.T, r:core.Type.T, e1:core.Type.TExpr, e2o:Option<core.Type.TExpr>} {
		return try {
			find_array_access_raise(ctx, a, tl, e1, e2o, p);
		}
		catch (_:ocaml.Not_found) {
			switch (e2o) {
				case None:
					core.Error.error('No @:arrayAccess function accepts argument of ${core.Type.s_type(core.Type.print_context(), e1.etype)}', p);
				case Some(e2):
					core.Error.error('No @:arrayAccess function accepts argument of ${core.Type.s_type(core.Type.print_context(), e1.etype)} and ${core.Type.s_type(core.Type.print_context(), e2.etype)}', p);
			}
		}
	}
}