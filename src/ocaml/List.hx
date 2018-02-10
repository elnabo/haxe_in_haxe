package ocaml;

import haxe.ds.Option;
using equals.Equal;

class List {
	public static function equals<T> (a:Array<T>, b:Array<T>) : Bool {
		if (a == null) { return b == null; }
		if (b == null) { return false; }
		if (a.length != b.length) { return false; }
		for (i in 0...a.length) {
			if (a[i] != b[i]) {
				return false;
			}
		}
		return true;
	}

	public static function iter<T> (f:T->Void, l:Array<T>) : Void {
		for (e in l) {
			f(e);
		}
	}

	public static function iter2<A,B> (f:A->B->Void, l1:Array<A>, l2:Array<B>) : Void {
		if (l1.length != l2.length) { throw new Invalid_argument(); }
		for (i in 0...l1.length) {
			f(l1[i], l2[i]);
		}
	}

	public static function for_all<A> (f:A->Bool, l:Array<A>) : Bool {
		var flag = true;
		for (e in l) {
			flag = flag && f(e);
		}
		return flag;
	}
	public static function for_all2<A,B> (f:A->B->Bool, l1:Array<A>, l2:Array<B>) : Bool {
		if (l1.length != l2.length) { throw new Invalid_argument(); }
		var flag = true;
		for (i in 0...l1.length) {
			flag = flag && f(l1[i], l2[i]);
		}
		return flag;
	}

	public static inline function map<A, B> (f:A->B, l:Array<A>) : Array<B> {
		return l.map(f);
	}
	
	public static function rev_map<A, B> (f:A->B, l:Array<A>) : Array<B> {
		var res = [];
		for (e in l) {
			res.unshift(f(e));
		}
		return res;
	}

	public static function map2<A,B,C> (f:A->B->C, l1:Array<A>, l2:Array<B>) : Array<C> {
		if (l1.length != l2.length) { throw new Invalid_argument(); }
		return [ for (i in 0...l1.length) f(l1[i], l2[i]) ];
	}

	public static function filter_map<A,B> (f:A->Option<B>, l:Array<A>): Array<B> {
		var res = [];
		for (e in l) {
			switch (f(e)) {
				case None:
				case Some(b): res.push(b);
			}
		}
		return res;
	}

	public static function mem<T> (a:T, l:Array<T>) : Bool {
		for (e in l) {
			if (e.equals(a)) {
				return true;
			}
		}
		return false;
	}

	// Same as List.mem, but uses physical equality instead of structural equality to compare list elements.
	public static function memq<T> (a:T, l:Array<T>) : Bool {
		return l.indexOf(a) != -1;
	}

	public static function rev<T> (a:Array<T>) : Array<T> {
		var copy = a.copy();
		copy.reverse();
		return copy;
	}

	public static function fold_left<A, B>(f:A->B->A, a:A, l:Array<B>) : A {
		var res = a;
		for (b in l) {
			res = f(a, b);
		}
		return res;
	}

	public static function exists<T> (f:T->Bool, l:Array<T>) : Bool {
		for (e in l) {
			if (f(e)) {
				return true;
			}
		}
		return false;
	}
	public static function find<T> (f:T->Bool, l:Array<T>) : T {
		for (e in l) {
			if (f(e)) {
				return e;
			}
		}
		throw ocaml.Not_found.instance;
	}

	public static function assoc<A, B> (a:A, b:Array<{fst:A, snd:B}>) : B {
		for (e in b) {
			if (a.equals(e.fst)) {
				return e.snd;
			}
		}
		throw ocaml.Not_found.instance;
	}
	
	public static function assq<A, B> (a:A, b:Array<{fst:A, snd:B}>) : B {
		for (e in b) {
			if (a == e.fst) {
				return e.snd;
			}
		}
		throw ocaml.Not_found.instance;
	}

	public static function assoc_typeparams (a:String, l:core.Type.TypeParams ) : core.Type.T {
		for (tp in l) {
			if (a == tp.name) {
				return tp.t;
			}
		}
		throw ocaml.Not_found.instance;
	}
}