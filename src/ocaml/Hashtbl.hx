package ocaml;

using equals.Equal;

class Hashtbl {
	public static function mem<A, B> (map:Map<A,B>, key:A) : Bool {
		// TODO fix map; replace Map<A, B> by haxe.ds.HashMap<A,B>
		for (k in map.keys()) {
			if (key.equals(k)) {
				return true;
			}
		}
		return false;
	}
	public static function find<A, B> (map:Map<A,B>, key:A, ?debug:Bool=false) : B {
		// TODO fix map; replace Map<A, B> by haxe.ds.HashMap<A,B>
		for (k in map.keys()) {
			if (key.equals(k)) {
				// if (debug) { 
				// 	trace(key); 
				// 	var tmp = cast map.get(k);
				// 	trace(tmp);
				// }
				return map.get(k);
			}
		}
		throw ocaml.Not_found.instance;
		// if (!map.exists(key)) {
		// 	throw ocaml.Not_found.instance;
		// }
		// return map.get(key);
	}

	public static function iter<A,B> (f:A->B->Void, m:Map<A,B>) {
		// TODO replace Map<A, B> by haxe.ds.HashMap<A,B>
		for (key in m.keys()) {
			f(key, m.get(key));
		}
	}
}