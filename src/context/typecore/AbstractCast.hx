package context.typecore;

import haxe.ds.ImmutableList;
import haxe.ds.Option;

import ocaml.List;
import ocaml.PMap;
import ocaml.Ref;

using ocaml.Cloner;
using equals.Equal;

class AbstractCast {

	public static var cast_stack = new Ref<ImmutableList<core.Type.TClassField>>([]);

	public static function make_static_call (ctx:context.Typecore.Typer, c:core.Type.TClass, cf:core.Type.TClassField, a:core.Type.TAbstract, pl:ImmutableList<core.Type.T>, args:ImmutableList<core.Type.TExpr>, t:core.Type.T, p:core.Globals.Pos) : core.Type.TExpr {
		return
		if (cf.cf_kind.match(Method(MethMacro))) {
			switch (args) {
				case [e]:
					var _tmp = context.Typecore.push_this(ctx, e);
					var e = _tmp.fst; var f = _tmp.snd;
					ctx.with_type_stack = context.Typecore.WithType.WithType(t) :: ctx.with_type_stack;
					var e = switch(ctx.g.do_macro(ctx, MExpr, c.cl_path, cf.cf_name, [e], p)) {
						case Some(e): context.Typecore.type_expr(ctx, e, Value);
						case None: context.Typecore.type_expr(ctx, {expr:EConst(CIdent("null")), pos:p}, Value);
					}
					ctx.with_type_stack = List.tl(ctx.with_type_stack);
					f();
					e;
				case _: trace("Shall not be seen"); Sys.exit(255); throw false;
			}
		}
		else {
			context.Typecore.make_static_call(ctx, c, cf, core.Type.apply_params.bind(a.a_params, pl), args, t, p);
		}
	}

	public static function do_check_cast (ctx:context.Typecore.Typer, tleft:core.Type.T, eright:core.Type.TExpr, p:core.Globals.Pos) : core.Type.TExpr {
		function recurse(cf:core.Type.TClassField, f:Void->core.Type.TExpr) : core.Type.TExpr {
			if (cf == ctx.curfield || List.mem(cf, cast_stack.get())) {
				core.Error.error("Recursive implicit cast", p);
			}
			cast_stack.set(cf::cast_stack.get());
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
						return ret.with({eexpr: core.Type.TExprExpr.TMeta({name:ImplicitCast, params:[], pos:ret.epos}, ret)});
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
					case _:
						throw ocaml.Not_found.instance;
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

	public static function find_multitype_specialization (com:context.Common.Context, a:core.Type.TAbstract, pl:core.Type.TParams, p:core.Globals.Pos) : {fst:core.Type.TClassField, snd:core.Type.T} {
		trace("TODO: find_multitype_specialization");
		throw false;
	}

	public static function handle_abstract_casts (ctx:context.Typecore.Typer, e:core.Type.TExpr) : core.Type.TExpr {
		function loop (ctx:context.Typecore.Typer, e:core.Type.TExpr) : core.Type.TExpr {
			return switch (e.eexpr) {
				case TNew(c={cl_kind:KAbstractImpl(a)}, pl, el):
					if (!core.Meta.has(MultiType, a.a_meta)) {
						/* This must have been a @:generic expansion with a { new } constraint (issue #4364). In this case
							let's construct the underlying type. */
						switch (core.Abstract.get_underlying_type(a, pl)) {
							case t=TInst(c, tl): e.with({{eexpr:core.Type.TExprExpr.TNew(c, tl, el), etype:t}});
							case _: core.Error.error("Cannot construct "+core.Type.s_type(core.Type.print_context(), TAbstract(a, pl)), e.epos);
						}
					}
					else {
						// a TNew of an abstract implementation is only generated if it is a multi type abstract
						var _tmp = find_multitype_specialization(ctx.com, a, pl, e.epos);
						var cf = _tmp.fst; var m = _tmp.snd;
						var e = make_static_call(ctx, c, cf, a, pl, core.Type.mk(TConst(TNull), TAbstract(a, pl), e.epos)::el, m, e.epos);
						e.with({etype:m});
					}
				case TCall({eexpr:TField(_, FStatic({cl_path:{a:[], b:"Std"}}, {cf_name:"string"}))}, [e1]) if ( switch (core.Type.follow(e1.etype)) { case TAbstract({a_impl:Some(_)}, _): true; case _: false; }):
					switch (core.Type.follow(e1.etype)) {
						case TAbstract(a={a_impl:Some(c)}, tl):
							try {
								var cf = PMap.find("toString", c.cl_statics);
								make_static_call(ctx, c, cf, a, tl, [e1], ctx.t.tstring, e.epos);
							}
							catch (_:ocaml.Not_found) {
								e;
							}
						case _:
							trace("Shall not be seen"); throw false;
					}
				case TCall(e1, el):
					try {
						function find_abstract(e:core.Type.TExpr, t:core.Type.T) : {fst:core.Type.TAbstract, snd:core.Type.TParams, trd:core.Type.TExpr} {
							return switch [core.Type.follow(t), e.eexpr] {
								case [TAbstract(a, pl), _] if (core.Meta.has(MultiType, a.a_meta)):
									{fst:a, snd:pl, trd:e};
								case [_, TCast(e1, None)]:
									find_abstract(e1, e1.etype);
								case [_, TLocal({v_extra:Some({expr:Some(e_)})})]:
									switch (core.Type.follow(e_.etype)) {
										case TAbstract(a, pl) if (core.Meta.has(MultiType, a.a_meta)):
											{fst:a, snd:pl, trd:core.Type.mk(TCast(e, None), e_.etype, e.epos)};
										case _: throw ocaml.Not_found.instance;
									}
								case _: throw ocaml.Not_found.instance;
							}
						}
						function find_field (e1:core.Type.TExpr) : core.Type.TExpr {
							return switch (e1.eexpr) {
								case TCast(e2, None):
									e1.with({eexpr:core.Type.TExprExpr.TCast(find_field(e2), None)});
								case TField(e2, fa):
									var _tmp = find_abstract(e2, e2.etype);
									var a = _tmp.fst; var pl = _tmp.snd; var e2 = _tmp.trd;
									var m = core.Abstract.get_underlying_type(a, pl);
									var fname = core.Type.field_name(fa);
									var el = List.map(loop.bind(ctx), el);
									try {
										var fa = core.Type.quick_field(m, fname);
										function get_fun_type(t:core.Type.T) {
											return switch (core.Type.follow(t)) {
												case tf=TFun({ret:tr}): {fst:tf, snd:tr};
												case _: throw ocaml.Not_found.instance;
											}
										}
										var _tmp = switch (fa) {
											case FStatic(_, cf): get_fun_type(cf.cf_type);
											case FInstance(c, tl, cf): get_fun_type(core.Type.apply_params(c.cl_params, tl, cf.cf_type));
											case FAnon(cf): get_fun_type(cf.cf_type);
											case _: throw ocaml.Not_found.instance;
										}
										var tf = _tmp.fst; var tr = _tmp.snd;
										var ef = core.Type.mk(TField(e2.with({etype:m}), fa), tf, e2.epos);
										var ecall = context.Typecore.make_call(ctx, ef, el, tr, e.epos);
										if (!core.Type.type_iseq(ecall.etype, e.etype)) {
											core.Type.mk(core.Type.TExprExpr.TCast(ecall, None), e.etype, e.epos);
										}
										else {
											ecall;
										}
									}
									catch (_:ocaml.Not_found) {
										// quick_field raises Not_found if m is an abstract, we have to replicate the 'using' call here
										switch (core.Type.follow(m)) {
											case TAbstract(a={a_impl:Some(c)}, pl):
												var cf = PMap.find(fname, c.cl_statics);
												make_static_call(ctx, c, cf, a, pl, e2::el, e.etype, e.epos);
											case _: throw ocaml.Not_found.instance;
										}
									}
								case _: throw ocaml.Not_found.instance;
							}
						}
						find_field(e1);
					}
					catch (_:ocaml.Not_found) {
						core.Type.map_expr(loop.bind(ctx), e);
					}
				case _: core.Type.map_expr(loop.bind(ctx), e);
			}
		}
		return loop(ctx, e);
	}
}