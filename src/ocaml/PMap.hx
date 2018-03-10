package ocaml;

import haxe.ds.ImmutableList;
using equals.Equal;

class PMap<K, V> {
	public final keys:ImmutableList<K>;
	public final values:ImmutableList<V>;
	public function new(keys:ImmutableList<K>, values:ImmutableList<V>) {
		this.keys = keys;
		this.values = values;
	}
	public static inline function empty<K,V> () : PMap<K,V> {
		return new PMap<K,V>(Tl, Tl);
	}

	public static inline function is_empty<K,V> (pmap:PMap<K,V>) : Bool {
		return pmap.keys == Tl;
	}
	public static inline function size<K,V> (pmap:PMap<K,V>) : Int {
		return List.length(pmap.keys);
	}

	public static function add<K,V> (key:K, value:V, pmap:PMap<K,V>) : PMap<K,V> {
		var m = remove(key, pmap);
		return new PMap<K, V>(key::m.keys, value::m.values);
	}
	public static function remove<K,V> (key:K, pmap:PMap<K,V>) : PMap<K,V> {
		var keys = pmap.keys;
		var values = pmap.values;
		var resKeys:ImmutableList<K> = Tl;
		var resValues:ImmutableList<V> = Tl;
		while (true) {
			switch [keys, values] {
				case [Tl, Tl]: break;
				case [Hd(k, kl), Hd(_, vl)] if (key.equals(k)):
					resKeys = List.append(resKeys, kl);
					resValues = List.append(resValues, vl);
					break;
				case [Hd(k, kl), Hd(v, vl)]:
					keys = kl; values = vl;
					resKeys = k :: resKeys;
					resValues = v :: resValues;
				case _: throw "Invalid PMap";
			}
		}
		return new PMap(resKeys, resValues);
	}

	public static function fold<K,V,C> (f:V->C->C, pmap:PMap<K,V>, c:C) : C {
		var values = pmap.values;
		var res = c;
		while (true) {
			switch (values) {
				case Tl: break;
				case Hd(v, tl):
					values = tl;
					res = f(v, res);
			}
		}
		return res;
	}
	public static function foldi<K,V,C> (f:K->V->C->C, pmap:PMap<K,V>, c:C) : C {
		var keys = pmap.keys;
		var values = pmap.values;
		var res = c;
		while (true) {
			switch [keys, values] {
				case [Tl, Tl]: break;
				case [Hd(k, kl), Hd(v, vl)]:
					keys = kl; values = vl;
					res = f(k, v, res);
				case _: throw "Invalid PMap";
			}
		}
		return res;
	}

	public static function map<A, B, C> (f:A->B, pmap:PMap<C,A>) : PMap<C, B> {
		var keys = pmap.keys;
		var values = pmap.values;
		var resKeys:ImmutableList<C> = Tl;
		var resValues:ImmutableList<B> = Tl;
		while (true) {
			switch [keys, values] {
				case [Tl, Tl]: break;
				case [Hd(k, kl), Hd(v, vl)]:
					keys = kl; values = vl;
					resKeys = k :: resKeys;
					resValues = f(v) :: resValues;
				case _: throw "Invalid PMap";
			}
		}
		return new PMap(resKeys, resValues);
	}

	public static function mapi<A, B, C> (f:A->B->C, pmap:PMap<A,B>) : PMap<A, C> {
		var keys = pmap.keys;
		var values = pmap.values;
		var resKeys:ImmutableList<A> = Tl;
		var resValues:ImmutableList<C> = Tl;
		while (true) {
			switch [keys, values] {
				case [Tl, Tl]: break;
				case [Hd(k, kl), Hd(v, vl)]:
					keys = kl; values = vl;
					resKeys = k :: resKeys;
					resValues = f(k, v) :: resValues;
				case _: throw "Invalid PMap";
			}
		}
		return new PMap(resKeys, resValues);
	}

	public static function iter<A,B> (f:A->B->Void, pmap:PMap<A,B>) : Void {
		List.iter2(f, pmap.keys, pmap.values);
	}

	public static function exists<A,B> (key:A, pmap:PMap<A,B>) : Bool {
		var keys = pmap.keys;
		while (true) {
			switch (keys) {
				case Tl: return false;
				case Hd(k, _) if (key.equals(k)): return true;
				case Hd(_, kl): keys = kl;
			}
		}
	}

	public static inline function mem<A,B> (key:A, pmap:PMap<A,B>) : Bool {
		return exists(key, pmap);
	}

	public static function find<A, B> (key:A, pmap:PMap<A,B>) : B {
		var keys = pmap.keys;
		var values = pmap.values;
		while (true) {
			switch [keys, values] {
				case [Tl, Tl]: throw ocaml.Not_found.instance;
				case [Hd(k, _), Hd(v, _)] if (key.equals(k)): return v;
				case [Hd(_, kl), Hd(_, vl)]:
					keys = kl; values = vl;
				case _: throw "Invalid PMap";
			}
		}
	}

	public static function for_all<A, B> (f:(A, B)->Bool, pmap:PMap<A, B>) : Bool {
		var keys = pmap.keys;
		var values = pmap.values;
		while (true) {
			switch [keys, values] {
				case [Tl, Tl]: return true;
				case [Hd(k, _), Hd(v, _)] if (!f(k,v )): return false;
				case [Hd(k, kl), Hd(v, vl)]:
					keys = kl;
					values = vl;
				case _: throw "Invalid PMap";
			}
		}
	}

}