package context.typecore;

import haxe.ds.ImmutableList;
import haxe.ds.Option;

import ocaml.List;
import ocaml.Ref;

using equals.Equal;

class AbstractCast {

	public static var cast_stack = new Ref<ImmutableList<core.Type.TClassField>>([]);

	public static function make_static_call (ctx:context.Typecore.Typer, c:core.Type.TClass, cf:core.Type.TClassField, a:core.Type.TAbstract, pl, args:ImmutableList<core.Type.TExpr>, t:core.Type.T, p:core.Globals.Pos) : core.Type.TExpr {
		trace("TODO: make_static_call");
		throw false;
	}

	public static function do_check_cast (ctx:context.Typecore.Typer, tleft:core.Type.T, eright:core.Type.TExpr, p:core.Globals.Pos) : core.Type.TExpr {
		function recurse(cf:core.Type.TClassField, f:Void->core.Type.TExpr) : core.Type.TExpr {
			if (cf.equals(ctx.curfield) || List.mem(cf, cast_stack.get())) {
				core.Error.error("Recursive implicit cast", p);
			}
			var r = f();
			cast_stack.set(List.tl(cast_stack.get()));
			return r;
		}
		function find (a:core.Type.TAbstract, tl:core.Type.TParams, f:Void->{cf:core.Type.TClassField, t:core.Type.T}) : core.Type.TExpr {
			var _tmp = f();
			var tcf = _tmp.t; var cf = _tmp.cf;
			return
			if (core.Meta.has(MultiType, a.a_meta)) {
				core.Type.mk_cast(eright, tleft, p);
			}
			else {
				switch (a.a_impl) {
					case Some(c): recurse(cf, function () {
						var ret = make_static_call(ctx, c, cf, a, tl, [eright], tleft, p);
						ret.eexpr = TMeta({name:ImplicitCast, params:[], pos:ret.epos}, ret);
						return ret;
					});
					case None: trace("Shall not be seen"); throw false;
				}
			}
		}
		return
		if (core.Type.type_iseq(tleft, eright.etype)) {
			eright;
		}
		else {
			function loop (tleft:core.Type.T, tright:core.Type.T): core.Type.TExpr {
				return
				switch [core.Type.follow(tleft), core.Type.follow(tright)] {
					case [TAbstract(a1, tl1), TAbstract(a2, tl2)]:
						core.Abstract.find_to_from(find, a1, tl1, a2, tl2, tleft, eright.etype);
					case [TAbstract(a, tl), _]:
						try {
							find(a, tl, function () { return core.Abstract.find_from(a, tl, eright.etype, tleft); });
						}
						catch (_:ocaml.Not_found) {
							function loop2(tcl:core.Type.TParams) {
								return switch (tcl) {
									case tc::tcl:
										if (!core.Type.type_iseq(tc, tleft)) {
											loop(core.Type.apply_params(a.a_params, tl, tc), tright);
										}
										else {
											loop2(tcl);
										}
									case []: throw ocaml.Not_found.instance;
								}
							}
							loop2(a.a_from);
						}
					case [_, TAbstract(a, tl)]:
						try {
							find(a, tl, function () { return core.Abstract.find_to(a, tl, tleft); });
						}
						catch (_:ocaml.Not_found) {
							function loop2(tcl) {
								return switch (tcl:core.Type.TParams) {
									case tc::tcl:
										if (!core.Type.type_iseq(tc, tright)) {
											loop(tleft, core.Type.apply_params(a.a_params, tl, tc));
										}
										else {
											loop2(tcl);
										}
									case []: throw ocaml.Not_found.instance;
								}
							}
							loop2(a.a_to);
						}
					case _: throw ocaml.Not_found.instance;
				}
			}
			loop(tleft, eright.etype);
		}
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