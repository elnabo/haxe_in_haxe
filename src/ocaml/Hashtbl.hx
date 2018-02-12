package ocaml;

using equals.Equal;

class Hashtbl {
	public static inline function add<A, B> (map:Map<A,B>, key:A, value:B) : Void {
		map.set(key, value);
	}
	public static inline function replace<A, B> (map:Map<A,B>, key:A, value:B) : Void {
		map.set(key, value);
	}
	public static inline function length<A, B> (map:Map<A,B>) : Int {
		var len = 0;
		for (_ in map) { len++; }
		return len;
	}
	public static function mem<A, B> (map:Map<A,B>, key:A) : Bool {
		// TODO fix map; replace Map<A, B> by haxe.ds.HashMap<A,B>
		for (k in map.keys()) {
			if (key.equals(k)) {
				return true;
			}
		}
		return false;
	}
	public static function find<A, B> (map:Map<A,B>, key:A) : B {
		// TODO fix map; replace Map<A, B> by haxe.ds.HashMap<A,B>
		for (k in map.keys()) {
			if (key.equals(k)) {
				return map.get(k);
			}
		}
		throw ocaml.Not_found.instance;
	}

	public static function iter<A,B> (f:A->B->Void, m:Map<A,B>) {
		// TODO replace Map<A, B> by haxe.ds.HashMap<A,B>
		for (key in m.keys()) {
			f(key, m.get(key));
		}
	}

	public static function fold<A,B,C> (f:A->B->C->C, m:Map<A,B>, into:C) : C {
		for (key in m.keys()) {
			into = f(key, m.get(key), into);
		}
		return into;
	}
}