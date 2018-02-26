package typing;

using ocaml.Cloner;

class Matcher {
	public static var fake_tuple_type:core.Type.T = TInst(core.Type.mk_class(core.Type.null_module, new core.Path([], "-Tuple"), core.Globals.null_pos, core.Globals.null_pos), []);
	public static function tuple_type (tl:core.Type.TParams) : core.Type.T {
		return core.Type.tfun(tl, fake_tuple_type.clone());
	}
}