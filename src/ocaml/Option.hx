package ocaml;

class Option {
	public static function map_default<A,B>(f:A->B, x:B, opt:haxe.ds.Option<A>) : B {
		return switch (opt) {
			case Some(v): f(v);
			case None: x;
		}
	}
}