package filters;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
import core.Type.TExprExpr;

import ocaml.List;
using equals.Equal;
using ocaml.Cloner;

class JsExceptions {
	public static inline function follow (t:core.Type.T) {
		return core.Abstract.follow_with_abstracts(t);
	}

	public static function is_js_error (c:core.Type.TClass) : Bool {
		return switch (c) {
			case {cl_path:{a:["js"], b:"Error"}}: true;
			case {cl_super: Some({c:csup})}: is_js_error(csup);
			case _: false;
		}
	}

	public static function find_cl (com:context.Common.Context, path:core.Path) : core.Type.TClass {
		return List.find_map(function (mt:core.Type.ModuleType) {
			return switch (mt) {
				case TClassDecl(c) if (c.cl_path.equals(path)): Some(c);
				case _: None;
			}
		}, com.types);
	}

	public static function init (ctx:context.Typecore.Typer) : core.Type.TExpr->core.Type.TExpr {
		var cJsError = find_cl(ctx.com, new core.Path(["js"], "Error"));
		var cHaxeError = find_cl(ctx.com, new core.Path(["js", "_Boot"], "HaxeError"));
		var cStd = find_cl(ctx.com, new core.Path([], "Std"));
		var cBoot = find_cl(ctx.com, new core.Path(["js"], "Boot"));
		var cSyntax = find_cl(ctx.com, new core.Path(["js"], "Syntax"));

		function dynamic_wrap (e:core.Type.TExpr) : core.Type.TExpr {
			var eHaxeError = context.Typecore.make_static_this(cHaxeError, e.epos);
			return core.Texpr.Builder.fcall(eHaxeError, "wrap", [e], TInst(cJsError, []), e.epos);
		}

		function static_wrap (e:core.Type.TExpr) : core.Type.TExpr {
			return e.with({eexpr:TNew(cHaxeError, [], [e]), etype:core.Type.T.TInst(cHaxeError, [])});
		}

		function loop (vrethrow:Option<core.Type.TExpr>, e:core.Type.TExpr) : core.Type.TExpr {
			return switch (e.eexpr) {
				case TThrow(eexc):
					var eexc = loop(vrethrow, eexc);
					var eexc = switch (core.Type.follow(eexc.etype)) {
						case TDynamic(_), TMono(_):
							switch(eexc.eexpr) {
								case TConst((TInt(_)|TFloat(_)|TString(_)|TBool(_)|TNull)): static_wrap(eexc);
								case _: dynamic_wrap(eexc);
							}
						case TInst(c, _) if (is_js_error(c)):
							eexc;
						case _:
							static_wrap(eexc);
					}
					e.with({eexpr:TThrow(eexc)});
				case TCall({eexpr:TField(_, FStatic({cl_path:{a:["js"], b:"Lib"}}, {cf_name:"getOriginalException"}))}, _):
					switch (vrethrow) {
						case Some(erethrowvar): erethrowvar;
						case None: context.Common.abort("js.Lib.getOriginalException can only be called inside a catch block", e.epos);
					}
				case TCall({eexpr:TField(_, FStatic({cl_path:{a:["js"], b:"Lib"}}, {cf_name:"rethrow"}))}, _):
					switch (vrethrow) {
						case Some(erethrowvar): e.with({eexpr:TThrow(erethrowvar)});
						case None: context.Common.abort("js.Lib.rethrow can only be called inside a catch block", e.epos);
					}
				case TTry(etry, catches):
					var etry = loop(vrethrow, etry);

					var catchall_name = switch (catches) { case [{v:v}]: v.v_name; case _: "e"; }
					var vcatchall = core.Type.alloc_var(catchall_name, core.Type.t_dynamic, e.epos);
					var ecatchall = core.Texpr.Builder.make_local(vcatchall, e.epos);
					var erethrow = core.Type.mk(TThrow(ecatchall), core.Type.t_dynamic, e.epos);

					var eSyntax = context.Typecore.make_static_this(cSyntax, e.epos);
					var eHaxeError = context.Typecore.make_static_this(cHaxeError, e.epos);
					var eInstanceof = core.Texpr.Builder.fcall(eSyntax, "instanceof", [ecatchall, eHaxeError], ctx.com.basic.tbool, e.epos);
					var eval = core.Texpr.Builder.field(ecatchall.with({etype:core.Type.T.TInst(cHaxeError, [])}), "val", core.Type.t_dynamic, e.epos);
					var eunwrap = core.Type.mk(TIf(eInstanceof, eval, Some(ecatchall)), core.Type.t_dynamic, e.epos);

					var vunwrapped = core.Type.alloc_var(catchall_name, core.Type.t_dynamic, e.epos);
					vunwrapped.v_meta = ({name:CompilerGenerated, params:[], pos:core.Globals.null_pos} : core.Ast.MetadataEntry) :: vunwrapped.v_meta;
					var eunwrapped = core.Texpr.Builder.make_local(vunwrapped, e.epos);

					var ecatch = List.fold_left(function (acc, c:{v:core.Type.TVar, e:core.Type.TExpr}) {
						var v = c.v; var ecatch = c.e;
						var ecatch = loop(Some(ecatchall), ecatch);
						// it's not really compiler-generated, but it kind of is, since it was used as catch identifier and we add a TVar for it
						v.v_meta = ({name:CompilerGenerated, params:[], pos:core.Globals.null_pos} : core.Ast.MetadataEntry) :: v.v_meta;

						return
						switch (core.Type.follow(v.v_type)) {
							case TDynamic(_):
								ecatch.with({eexpr:TBlock([
									core.Type.mk(TVar(v, Some(eunwrapped)), ctx.com.basic.tvoid, ecatch.epos),
									ecatch
								])});
							case t:
								var etype = core.Texpr.Builder.make_typeexpr(core.Type.module_type_of_type(t), e.epos);
								var args:ImmutableList<core.Type.TExpr> = [eunwrapped, etype];
								var echeck = switch (optimization.Optimizer.api_inline(ctx, cStd, "is", args, e.epos)) {
									case Some(e): e;
									case None:
										var eBoot = core.Texpr.Builder.make_static_this(cBoot, e.epos);
										core.Texpr.Builder.fcall(eBoot, "__instanceof", [eunwrapped, etype], ctx.com.basic.tbool, e.epos);
								}
								var ecatch = ecatch.with({eexpr:TBlock([
									core.Type.mk(TVar(v, Some(eunwrapped)), ctx.com.basic.tvoid, ecatch.epos),
									ecatch
								])});
								core.Type.mk(TIf(echeck, ecatch, Some(acc)), e.etype, e.epos);
						}
					}, erethrow, List.rev(catches));

					ecatch = ecatch.with({
						eexpr:TBlock([
							core.Type.mk(TVar(vunwrapped, Some(eunwrap)), ctx.com.basic.tvoid, e.epos),
							ecatch
						])
					});

					e.with({eexpr:TTry(etry, [{v:vcatchall, e:ecatch}])});
				case _:
					core.Type.map_expr(loop.bind(vrethrow), e);
			}
		}

		return loop.bind(None);
	}

	public static function inject_callstack (com:context.Common.Context, type_filters:ImmutableList<(context.Typecore.Typer, core.Type.ModuleType)->Void>) : ImmutableList<(context.Typecore.Typer, core.Type.ModuleType)->Void> {
		var cCallStack = if (context.Common.has_dce(com)) {
			if (context.Common.has_feature(com, "haxe.CallStack.lastException")) {
				Some(find_cl(com, new core.Path(["haxe"], "CallStack")));
			}
			else {
				None;
			}
		}
		else {
			try {
				Some(find_cl(com, new core.Path(["haxe"], "CallStack")));
			}
			catch (_:ocaml.Not_found) {
				None;
			}
		}
		return
		switch (cCallStack) {
			case Some(cCallStack):
				function loop (e:core.Type.TExpr) : core.Type.TExpr {
					return switch (e.eexpr) {
						case TTry(etry, [{v:v, e:ecatch}]):
							var etry = loop(etry);
							var ecatch = loop(ecatch);

							var eCallStack = core.Texpr.Builder.make_static_this(cCallStack, ecatch.epos);
							var elastException = core.Texpr.Builder.field(eCallStack, "lastException", core.Type.t_dynamic, ecatch.epos);
							var elocal = core.Texpr.Builder.make_local(v, ecatch.epos);
							var eStoreException = core.Type.mk(TBinop(OpAssign, elastException, elocal), ecatch.etype, ecatch.epos);
							var ecatch = core.Type.concat(eStoreException, ecatch);
							e.with({eexpr:TTry(etry, [{v:v, e:ecatch}])});
						case TTry(_,_):
							// this should be handled by the filter above
							trace("Shall not be seen"); std.Sys.exit(255); throw false;
						case _:
							core.Type.map_expr(loop, e);
					}
				}
				List.append(type_filters, [function (ctx, t) { return FiltersCommon.run_expression_filters(ctx, [loop], t); }]);
			case None:
				type_filters;
		}
	}
}