package ocaml;

using equals.Equal;

class PMap {

	public static inline function is_empty<A,B> (map:Map<A,B>) : Bool {
		return map.iterator().hasNext();
	}

	public static function add<A,B> (key:A, value:B, m:Map<A,B>) : Map<A,B> {
		var m = m.copy();
		m.set(key, value);
		return m;
	}
	public static function remove<A,B> (key:A, m:Map<A,B>) : Map<A,B> {
		var m = m.copy();
		m.remove(key);
		return m;
	}

	public static function fold<A,B,C> (f:B->C->C, map:Map<A,B>, c:C) : C {
		var res = c;
		for (value in map) {
			res = f(value, res);
		}
		return res;
	}
	public static function foldi<A,B,C> (f:A->B->C->C, map:Map<A,B>, c:C) : C {
		var res = c;
		for (key in map.keys()) {
			var value = map.get(key);
			res = f(key, value, res);
		}
		return res;
	}

	public static function map<A,B, C:{}> (f:A->B, m:Map<C,A>) : Map<C, B> {
		var res = new Map<C,B>();
		for (key in m.keys()) {
			res.set(key, f(m.get(key)));
		}
		return res;
	}

	public static function iter<A,B> (f:A->B->Void, m:Map<A,B>) : Void {
		for (key in m.keys()) {
			f(key, m.get(key));
		}
	}

	public static inline function exists<A,B> (key:A, m:Map<A,B>) : Bool {
		return mem(key, m);
	}

	public static function mem<A,B> (key:A, m:Map<A,B>) : Bool {
		// TODO fix map
		for (k in m.keys()) {
			if (key.equals(k)) {
				return true;
			}
		}
		return false;
		// throw ocaml.Not_found.instance;
		// return m.exists(key);
	}

	public static function find<A, B> (key:A, m:Map<A,B>) : B {
		// TODO fix map
		// if (m.exists(key)) {
		// 	return m.get(key);
		// }
		for (k in m.keys()) {
			if (key.equals(k)) {
				return m.get(k);
			}
		}
		throw ocaml.Not_found.instance;
	}

}