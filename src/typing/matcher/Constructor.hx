package typing.matcher;

import ocaml.List;

class Constructor {
	public static function to_string(con:typing.matcher.constructor.T) : String {
		return switch (con) {
			case ConConst(ct): core.Type.s_const(ct);
			case ConEnum(en, ef): ef.ef_name;
			case ConStatic(c, cf): '${core.Globals.s_type_path(switch (c.cl_kind) { case KAbstractImpl(a): a.a_path; case _: c.cl_path;})}.${cf.cf_name}';
			case ConTypeExpr(mt): core.Globals.s_type_path(core.Type.t_infos(mt).mt_path);
			case ConFields(fields): '{ ${List.join(", ", fields)} }';
			case ConArray(i): '<array ${i}>';
		}
	}
}