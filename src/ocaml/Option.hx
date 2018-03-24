package ocaml;

class Option {

	public static function may<A> (f:A->Void, opt:haxe.ds.Option<A>) : Void {
		switch (opt) {
			case Some(a): f(a);
			case None:
		}
	}

	public static function map<A,B> (f:A->B, opt:haxe.ds.Option<A>) : haxe.ds.Option<B> {
		return switch (opt) {
			case None: None;
			case Some(v): Some(f(v));
		}
	}

	public static function map_default<A,B>(f:A->B, x:B, opt:haxe.ds.Option<A>) : B {
		return switch (opt) {
			case Some(v): f(v);
			case None: x;
		}
	}

	public static inline function is_some<A>(x:haxe.ds.Option<A>) : Bool {
		return x.match(Some(_));
	}
}