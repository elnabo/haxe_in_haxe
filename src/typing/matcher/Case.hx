package typing.matcher;

import haxe.ds.ImmutableList;
import haxe.ds.Option;

import ocaml.List;
import ocaml.PMap;

typedef T = {
	case_guard: Option<core.Type.TExpr>,
	case_expr: Option<core.Type.TExpr>,
	case_pos: core.Globals.Pos
}

class Case {

	public static function make(ctx:context.Typecore.Typer, t:core.Type.T, el:ImmutableList<core.Ast.Expr>, eg:Option<core.Ast.Expr>, eo_ast:Option<core.Ast.Expr>, with_type:context.Typecore.WithType, p:core.Globals.Pos) : {fst:T, snd:ImmutableList<Any>, trd:typing.matcher.Pattern} {
		function collapse_case (el:ImmutableList<core.Ast.Expr>) : core.Ast.Expr {
			return switch (el) {
				case e::[]:
					e;
				case e::el:
					var e2 = collapse_case(el);
					{expr:EBinop(OpOr,e, e2), pos:e2.pos};
				case []:
					trace("Shall not be seen"); throw false;
			}
		}
		var e = collapse_case(el);
		var monos = List.map(function (_) { return core.Type.mk_mono(); }, ctx.type_params);
		var map = core.Type.apply_params.bind(ctx.type_params, monos);
		var save = context.Typecore.save_locals(ctx);
		var old_types = PMap.fold(function (v:core.Type.TVar, acc:ImmutableList<{fst:core.Type.TVar, snd:core.Type.T}>) {
			var t_old = v.v_type;
			v.v_type = map(v.v_type);
			return {fst:v, snd:t_old}::acc;
		}, ctx.locals, []);
		var old_ret = ctx.ret;
		ctx.ret = map(ctx.ret);
		var pat = typing.matcher.Pattern.make(ctx, map(t), e);
		typing.Matcher.unapply_type_parameters(ctx.type_params, monos);
		var eg = switch (eg) {
			case None: None;
			case Some(e): Some(typing.Typer.type_expr(ctx, e, Value));
		}
		var eo = switch [eo_ast, with_type] {
			case [None, WithType(t)]:
				context.Typecore.unify(ctx, ctx.t.tvoid, t, e.pos);
				None;
			case [None, _]:
				None;
			case [Some(e), WithType(t)]:
				var e = typing.Typer.type_expr(ctx, e, WithType(map(t)));
				var e = context.typecore.AbstractCast.cast_or_unify(ctx, map(t), e, e.epos);
				Some(e);
			case [Some(e), _]:
				var e = typing.Typer.type_expr(ctx, e, with_type);
				Some(e);
		}
		ctx.ret = old_ret;
		List.iter(function (arg) {
			var v = arg.fst; var t = arg.snd;
			v.v_type = t;
		}, old_types);
		save();
		if (ctx.is_display_file && context.Display.is_display_position(p)) {
			switch [eo, eo_ast] {
				case [Some(e), Some(e_ast)]:
					typing.Typer.display_expr(ctx, e_ast, e, with_type, p);
				case [None, None]:
					typing.Typer.display_expr(ctx, {expr:EBlock([]), pos:p}, core.Type.mk(TBlock([]), ctx.t.tvoid, p), with_type, p);
				case _:
					trace("Shall not be seen"); throw false;
			}
		}
		return {fst:{case_guard:eg, case_expr:eo, case_pos:p}, snd:[], trd:pat};
	}

}