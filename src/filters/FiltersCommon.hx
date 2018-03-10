package filters;

import haxe.ds.ImmutableList;
import ocaml.List;

class FiltersCommon {
	public static function is_removable_class (c:core.Type.TClass) : Bool {
		return switch (c.cl_kind) {
			case KGeneric:
				var _tmp = List.exists(function (param) {
					var t = param.t;
					return
					switch (core.Type.follow(t)) {
						case TInst(c,_):
							core.Meta.has(Const, c.cl_meta) || core.Type.has_ctor_constraint(c);
						case _: false;
					}
				}, c.cl_params);
				_tmp = _tmp || switch (c.cl_super) {
					case Some({c:c}): is_removable_class(c);
					case _: false;
				}
				_tmp || core.Meta.has(Remove, c.cl_meta);
			case KTypeParameter(_):
				// this shouldn't happen, have to investigate (see #4092)
				true;
			case _:
				false;
		}
	}

	public static function run_expression_filters (ctx:context.Typecore.Typer, filters:ImmutableList<core.Type.TExpr->core.Type.TExpr>, t:core.Type.ModuleType) : Void {
		function run (e:core.Type.TExpr) {
			return List.fold_left(function (e, f) { return f(e); }, e, filters);
		}
		switch (t) {
			case TClassDecl(c) if (is_removable_class(c)):
			case TClassDecl(c):
				ctx.curclass = c;
				function process_field (f:core.Type.TClassField) {
					ctx.curfield = f;
					switch (f.cf_expr) {
						case Some(e) if (!context.Typecore.is_removable_field(ctx, f)):
							context.typecore.AbstractCast.cast_stack.set(f::context.typecore.AbstractCast.cast_stack.get());
							f.cf_expr = Some(run(e));
							context.typecore.AbstractCast.cast_stack.set(List.tl(context.typecore.AbstractCast.cast_stack.get()));
						case _:
					}
					List.iter(process_field, f.cf_overloads);
				}
				List.iter(process_field, c.cl_ordered_fields);
				List.iter(process_field, c.cl_ordered_statics);
				switch (c.cl_constructor) {
					case None:
					case Some(f): process_field(f);
				}
				switch (c.cl_init) {
					case None:
					case Some(e):
						c.cl_init = Some(run(e));
				}
			case TEnumDecl(_):
			case TTypeDecl(_):
			case TAbstractDecl(_):
		}
	}
}