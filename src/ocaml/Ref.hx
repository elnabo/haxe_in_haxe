package ocaml;

class Ref<T> {
	var internal:T;
	public function new (t:T) {
		internal = t;
	}

	public inline function get () : T {
		return internal;
	}

	public function set (t:T) : T {
		return internal = t;
	}
}