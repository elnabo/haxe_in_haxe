package core;

class Abstract {

	public static function get_underlying_type (a:core.Type.TAbstract, pl:core.Type.TParams) : core.Type.T {
		trace("TODO: core.Abstract.get_underlying_type");
		throw false;
	}

	public static function follow_with_abstracts (t:core.Type.T) : core.Type.T {
		return switch (core.Type.follow(t)) {
			case TAbstract(a, tl) if (!core.Meta.has(CoreType, a.a_meta)):
				follow_with_abstracts(get_underlying_type(a, tl));
			case t: t;
		}
	}
}