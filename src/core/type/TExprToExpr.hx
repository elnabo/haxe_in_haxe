package core.type;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
import ocaml.List;
import ocaml.PMap;

class TExprToExpr {

	public static function tpath (p:core.Path, mp:core.Path, pl:ImmutableList<core.Ast.TypeHint>) : core.Ast.ComplexType {
		if (mp.b == p.b) {
			return CTPath({
				tpackage: p.a,
				tname: p.b,
				tparams: List.map(function (t) :core.Ast.TypeParamOrConst { return TPType(t); }, pl),
				tsub: None
			});
		}
		else {
			return CTPath({
				tpackage: mp.a,
				tname: mp.b,
				tparams: List.map(function (t) :core.Ast.TypeParamOrConst { return TPType(t); }, pl),
				tsub: Some(p.b)
			});
		}
	}

	public static function convert_type (tf:core.Type.T) : core.Ast.ComplexType {
		return switch (tf) {
			case TMono(r):
				switch (r.get()) {
					case None: throw ocaml.Exit.instance;
					case Some(t): convert_type(t);
				}
			case TInst({cl_private:true, cl_path:{b:name}}, tl), TEnum({e_private:true, e_path:{b:name}}, tl), TType({t_private:true, t_path:{b:name}}, tl), TAbstract({a_private:true, a_path:{b:name}}, tl):
				CTPath({tpackage:[], tname:name, tparams:List.map(function(t) : core.Ast.TypeParamOrConst {
					return TPType(convert_type_(t));
				}, tl), tsub:None});
			case TEnum(e, pl):
				tpath(e.e_path, e.e_module.m_path, List.map(convert_type_, pl));
			case TInst(c={cl_kind:KTypeParameter(_)}, pl):
				tpath(new core.Path([],c.cl_path.b), new core.Path([],c.cl_path.b), List.map(convert_type_, pl));
			case TInst(c, pl):
				tpath(c.cl_path, c.cl_module.m_path, List.map(convert_type_, pl));
			case TType(t, pl):
				// recurse on type-type
				if (t.t_path.b.charAt(0) == "#") {
					convert_type(core.Type.follow(tf));
				}
				else {
					tpath(t.t_path, t.t_module.m_path, List.map(convert_type_, pl));
				}
			case TAbstract(a, pl):
				tpath(a.a_path, a.a_module.m_path, List.map(convert_type_, pl));
			case TFun({args:args, ret:ret}):
				CTFunction(List.map(function (arg) { return convert_type_(arg.t); }, args), convert_type_(ret));
			case TAnon(a):
				switch (a.a_status.get()) {
					case Statics(c): tpath(new core.Path([], "Class"), new core.Path([], "Class"), [{ct:tpath(c.cl_path, c.cl_path, []), pos:core.Globals.null_pos}]);
					case EnumStatics(e): tpath(new core.Path([], "Enum"), new core.Path([], "Enum"), [{ct:tpath(e.e_path, e.e_path, []), pos:core.Globals.null_pos}]);
					case _:
						CTAnonymous(PMap.foldi(function (_, f, acc:ImmutableList<core.Ast.ClassField>) {
							return {
								cff_name: {pack:f.cf_name,pos:core.Globals.null_pos},
								cff_kind: core.Ast.ClassFieldKind.FVar(mk_type_hint(f.cf_type, core.Globals.null_pos), None),
								cff_pos: f.cf_pos,
								cff_doc : f.cf_doc,
								cff_meta: f.cf_meta,
								cff_access: Tl // []
							} :: acc;
						}, a.a_fields, Tl));
				}
			case TDynamic(t2):
				var _path = new core.Path([], "Dynamic");
				tpath(_path, _path, (tf == core.Type.t_dynamic) ? [] : [convert_type_(t2.get())]);
			case TLazy(f): convert_type(core.Type.lazy_type(f));
		}
	}

	public static function convert_type_ (t:core.Type.T) : core.Ast.TypeHint {
		return {ct:convert_type(t), pos:core.Globals.null_pos};
	}

	public static function mk_type_hint (t:core.Type.T, p:core.Globals.Pos) : Option<core.Ast.TypeHint> {
		return switch (core.Type.follow(t)) {
			case TMono(_): None;
			case _:
				try {
					Some({ct:convert_type(t), pos:p});
				}
				catch (_:ocaml.Exit) {
					None;
				}
		}
	}
}