package ocaml;

import haxe.ds.Option;
import haxe.ds.ImmutableList;
using equals.Equal;

class List {
	public static function hd<T> (l:ImmutableList<T>) : T {
		return switch (l) {
			case Tl: throw ocaml.Failure.instance;
			case Hd(v, _): v;
		}
	}

	public static function append<T> (a : ImmutableList<T>, b : ImmutableList<T> ) : ImmutableList<T> {
		return switch (a) {
			case Tl: b;
			case Hd(v, tl):
				v::append(tl, b);
		}
	}

	public static function concat<T>(l:ImmutableList<ImmutableList<T>>) : ImmutableList<T> {
		return switch (l) {
			case Tl: Tl;
			case Hd(v, tl): append(v, concat(tl));
		}
	}

	public static function length<T> (l:ImmutableList<T>) : Int {
		return switch (l) {
			case Tl: 0;
			case Hd(_, tl): 1 + length(tl);
		}
	}
	public static function tl<T> (l:ImmutableList<T>, ?pos:haxe.PosInfos) : ImmutableList<T> {
		return switch (l) {
			case Tl: throw ocaml.Failure.instance;
			case Hd(_, tl): tl;
		}
	}

	public static function init<T> (length:Int, f:Int->T): ImmutableList<T> {
		if (length < 0) { throw ocaml.Invalid_argument.instance; }
		var arr = [for (i in 0...length) f(i)];
		return arr;
	}

	public static function make<T> (count:Int, x:T) : ImmutableList<T> {
		return (count <= 0) ? Tl : Hd(x, make(count-1, x));
	}
	public static function join<T> (sep:String, l:ImmutableList<T>) : String {
		var buf = new StringBuf();
		function loop (l:ImmutableList<T>) {
		 	switch (l) {
				case Tl: return;
				case Hd(v, Tl):
					buf.add(v);
				case Hd(v, tl):
					buf.add(v);
					buf.add(sep);
					loop(tl);
			}
		}
		loop(l);
		return buf.toString();
	}

	public static function sort<T> (f:T->T->Int, l:ImmutableList<T>) : ImmutableList<T> {
		var _tmp:Array<T> = l;
		_tmp.sort(f);
		return _tmp;
	}

	public static function iter<T> (f:T->Void, l:ImmutableList<T>) : Void {
		switch (l) {
			case Tl:
			case Hd(v, tl):
				f(v);
				iter(f, tl);
		}
	}

	public static function iter2<A,B> (f:A->B->Void, l1:ImmutableList<A>, l2:ImmutableList<B>) : Void {
		if (length(l1) != length(l2)) { throw new Invalid_argument(); }
		switch [l1, l2] {
			case [Tl, Tl]:
			case [Hd(v1, tl1), Hd(v2, tl2)]:
				f(v1, v2);
			case _: throw new Invalid_argument();
		}
	}

	public static function for_all<T> (f:T->Bool, l:ImmutableList<T>) : Bool {
		return switch (l) {
			case Tl: true;
			case Hd(v, tl): f(v) && for_all(f, tl);
		}
	}
	public static function for_all2<A,B> (f:A->B->Bool, l1:ImmutableList<A>, l2:ImmutableList<B>) : Bool {
		if (length(l1) != length(l2)) { throw new Invalid_argument(); }
		return switch [l1, l2] {
			case [Tl, Tl]: true;
			case [Hd(v1, tl1), Hd(v2, tl2)]:
				if (f(v1, v2)) {
					for_all2(f, tl1, tl2);
				}
				else {
					false;
				}
			case _: throw new Invalid_argument();
		}
	}

	public static inline function map<A, B> (f:A->B, l:ImmutableList<A>) : ImmutableList<B> {
		return switch (l) {
			case Tl: Tl;
			case Hd(v, tl):
				return Hd(f(v), map(f, tl));
		}
	}
	public static inline function mapi<A, B> (f:Int->A->B, l:ImmutableList<A>, ?index:Int=0) : ImmutableList<B> {
		return switch (l) {
			case Tl: Tl;
			case Hd(v, tl):
				return Hd(f(index, v), mapi(f, tl, index+1));
		}
	}

	public static function rev_map<A, B> (f:A->B, l:ImmutableList<A>) : ImmutableList<B> {
		var res = Tl;
		var curr = l;
		while (true) {
			switch (curr) {
				case Tl: break;
				case Hd(v, tl):
					curr = tl;
					res = Hd(f(v), res);
			}
		}
		return res;
	}

	public static function map2<A,B,C> (f:A->B->C, l1:ImmutableList<A>, l2:ImmutableList<B>) : ImmutableList<C> {
		if (length(l1) != length(l2)) { throw new Invalid_argument(); }
		return switch ({f:l1, s:l2}) {
			case {f:Tl, s:Tl}: Tl;
			case {f:Hd(v1, tl1), s:Hd(v2, tl2)}:
				Hd(f(v1, v2), map2(f, tl1, tl2));
			case _: throw new Invalid_argument();
		}
	}

	public static function filter<T> (f:T->Bool, l:ImmutableList<T>) : ImmutableList<T> {
		return switch (l) {
			case Tl: Tl;
			case Hd(v, tl):
				(f(v)) ? v::filter(f, tl) : filter(f, tl);
		}
	}

	public static function find_map<A,B> (f:A->Option<B>, l:ImmutableList<A>): B {
		return switch (l) {
			case Tl: throw ocaml.Not_found.instance;
			case Hd(v, tl):
				switch (f(v)) {
					case None: find_map(f, tl);
					case Some(b): b;
				}
		}
	}
	public static function filter_map<A,B> (f:A->Option<B>, l:ImmutableList<A>): ImmutableList<B> {
		return switch (l) {
			case Tl: Tl;
			case Hd(v, tl):
				switch (f(v)) {
					case None: filter_map(f, tl);
					case Some(b): Hd(b, filter_map(f, tl));
				}
		}
	}

	public static function mem<T> (a:T, l:ImmutableList<T>) : Bool {
		return switch (l) {
			case Tl: false;
			case Hd(v, tl): a.equals(v) || mem(a, tl);
		}
	}

	// Same as List.mem, but uses physical equality instead of structural equality to compare list elements.
	public static function memq<T> (a:T, l:ImmutableList<T>) : Bool {
		return switch (l) {
			case Tl: false;
			case Hd(v, tl): (a == v) || mem(a, tl);
		}
	}

	public static function rev<T> (a:ImmutableList<T>) : ImmutableList<T> {
		var res = Tl;
		var l = a;
		while (true) {
			switch (l) {
				case Tl: break;
				case Hd(v, tl):
					l = tl;
					res = Hd(v, res);
			}
		}
		return res;
	}

	public static function fold_left<A, B>(f:A->B->A, a:A, l:ImmutableList<B>) : A {
		return switch (l) {
			case Tl: a;
			case Hd(v, tl):
				fold_left(f, f(a, v), tl);
		}
	}

	public static function exists<T> (f:T->Bool, l:ImmutableList<T>) : Bool {
		return switch (l) {
			case Tl: false;
			case Hd(v, tl): f(v) || exists(f, tl);
		}
	}
	public static function find<T> (f:T->Bool, l:ImmutableList<T>) : T {
		return switch (l) {
			case Tl: throw ocaml.Not_found.instance;
			case Hd(v, tl): f(v) ? v : find(f, tl);
		}
	}

	public static function assoc<A, B> (a:A, b:ImmutableList<{fst:A, snd:B}>) : B {
		return switch (b) {
			case Tl: throw ocaml.Not_found.instance;
			case Hd(v, tl):
				(a.equals(v.fst)) ? v.snd : assoc(a, tl);
		}
	}

	public static function assq<A, B> (a:A, b:ImmutableList<{fst:A, snd:B}>) : B {
		return switch (b) {
			case Tl: throw ocaml.Not_found.instance;
			case Hd(v, tl):
				(a == v.fst) ? v.snd : assq(a, tl);
		}
	}

	public static function assoc_typeparams (a:String, l:core.Type.TypeParams ) : core.Type.T {
		return switch (l) {
			case Tl: throw ocaml.Not_found.instance;
			case Hd(v, tl):
				(a == v.name) ? v.t : assoc_typeparams(a, tl);
		}
	}
}