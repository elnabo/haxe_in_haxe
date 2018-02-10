package context.common;

class IdentifierType {
	public static function get_name (t:context.common.identifiertype.T) : String {
		return switch (t) {
			case ITLocal(v) : v.v_name;
			case ITMember(_,cf) | ITStatic(_,cf) | ITEnumAbstract(_,cf) : cf.cf_name;
			case ITEnum(_,ef) : ef.ef_name;
			case ITGlobal(_,s,_) : s;
			case ITType(mt) : core.Type.t_infos(mt).mt_path.b;
			case ITPackage(s) : s;
			case ITLiteral(s) : s;
			case ITTimer(s) : s;
		}
	}
}