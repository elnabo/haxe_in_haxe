package typing;

import haxe.ds.ImmutableList;
import ocaml.List;
using ocaml.Cloner;
using equals.Equal;

class Internal_match_failure {
	public static final instance = new Internal_match_failure();
	function new () {}
}

class Matcher {
	public static function s_type (t:core.Type.T) : String {
		return core.Type.s_type(core.Type.print_context(), t);
	}
	public static function s_expr_pretty(e:core.Type.TExpr) {
		return core.Type.s_expr_pretty(false, "", false, s_type, e);
	}

	public static var fake_tuple_type:core.Type.T = TInst(core.Type.mk_class(core.Type.null_module, new core.Path([], "-Tuple"), core.Globals.null_pos, core.Globals.null_pos), []);
	public static function tuple_type (tl:core.Type.TParams) : core.Type.T {
		return core.Type.tfun(tl, fake_tuple_type);
	}

	public static function make_offset_list<T> (left:Int, right:Int, middle:T, other:T) : ImmutableList<T> {
		return List.append(List.make(left, other), List.append([middle], List.make(right, other)));
	}

	public static function type_field_access (ctx:context.Typecore.Typer, ?resume:Bool=false, e:core.Type.TExpr, name:String) : core.Type.TExpr {
		return typing.Typer.acc_get(ctx, typing.Typer.type_field(resume, ctx, e, name, e.epos, MGet), e.epos);
	}

	public static function unapply_type_parameters (params:core.Type.TypeParams, monos:core.Type.TParams) : Void {
		List.iter2(function (arg, t2) {
			var t1 = arg.t;
			switch [t2, core.Type.follow(t2)] {
				case [TMono(m1), TMono(m2)] if (m1.equals(m2)):
					core.Type.unify(t1, t2);
				case _:
			}
		}, params, monos);
	}

	public static function get_general_module_type (ctx:context.Typecore.Typer, mt:core.Type.ModuleType, p:core.Globals.Pos) : core.Type.T {
		function loop(mt:core.Type.ModuleType):String {
			return switch (mt) {
				case TClassDecl(_): "Class";
				case TEnumDecl(_): "Enum";
				case TAbstractDecl(a) if (core.Meta.has(RuntimeValue, a.a_meta)):
					"Class";
				case TTypeDecl(t):
					switch (core.Type.follow(core.Type.monomorphs(t.t_params, t.t_type))) {
						case TInst(c, _): loop(TClassDecl(c));
						case TEnum(en, _): loop(TEnumDecl(en));
						case TAbstract(a, _): loop(TAbstractDecl(a));
						case _: core.Error.error("Cannot use this type as a value", p);
					}
				case _: core.Error.error("Cannot use this type as a value", p);
			}
		}
		return typing.Typeload.load_instance(ctx, {tp:{
			tname:loop(mt),
			tpackage:[],
			tsub:None,
			tparams:[]
		}, pos:core.Globals.null_pos}, true, p);
	}
}