package ocaml;

using equals.Equal;

class Hashtbl<K,V> {
	var keys:Array<K> = [];
	var values:Array<V> = [];
	public function new() {}
	public static inline function create<K,V> (i:Int) : Hashtbl<K,V> {
		return new Hashtbl<K,V>();
	}

	public function exists(key:K) : Bool {
		for (k in keys) {
			if (k.equals(key)) {
				return true;
			}
		}
		return false;
	}

	public function set(key:K, value:V) : Void {
		if (!exists(key)) {
			keys.push(key);
			values.push(value);
		}
	}

	public function get(key:K) : V {
		for (i in 0...keys.length) {
			if (keys[i].equals(key)) {
				return values[i];
			}
		}
		throw ocaml.Not_found.instance;
	}

	public function delete(key:K) : Void {
		for (i in 0...keys.length) {
			if (keys[i].equals(key)) {
				if (keys.length > 1) {
					keys[i] = keys.pop();
					values[i] = values.pop();
				}
				else {
					keys.pop();
					values.pop();
				}
				return;
			}
		}
	}

	public static inline function add<A, B> (map:Hashtbl<A,B>, key:A, value:B) : Void {
		map.set(key, value);
	}
	public static inline function replace<A, B> (map:Hashtbl<A,B>, key:A, value:B) : Void {
		map.set(key, value);
	}
	public static inline function remove<A, B> (map:Hashtbl<A,B>, key:A) : Void {
		map.delete(key);
	}
	public static inline function length<A, B> (map:Hashtbl<A,B>) : Int {
		return map.keys.length;
	}
	public static inline function mem<A, B> (map:Hashtbl<A,B>, key:A) : Bool {
		// TODO fix map; replace Map<A, B> by haxe.ds.HashMap<A,B> ?
		return map.exists(key);
	}
	public static inline function find<A, B> (map:Hashtbl<A,B>, key:A) : B {
		// TODO fix map; replace Map<A, B> by haxe.ds.HashMap<A,B>
		return map.get(key);
	}

	public static function iter<A,B> (f:A->B->Void, m:Hashtbl<A,B>) {
		// TODO replace Map<A, B> by haxe.ds.HashMap<A,B>
		for (key in m.keys) {
			f(key, m.get(key));
		}
	}

	public static function fold<A,B,C> (f:A->B->C->C, m:Hashtbl<A,B>, into:C) : C {
		for (key in m.keys) {
			into = f(key, m.get(key), into);
		}
		return into;
	}
}