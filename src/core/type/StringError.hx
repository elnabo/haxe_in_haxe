package core.type;

import haxe.ds.ImmutableList;
import ocaml.List;

class StringError {
	// Source: http://en.wikibooks.org/wiki/Algorithm_implementation/Strings/Levenshtein_distance
	public static function levenshtein(s1:String, s2:String) : Int {
			var len1 = s1.length;
			var len2 = s2.length;
			var d: Array<Array<Int>> = new Array<Array<Int>>();
			for(i in 0...(len1+1)) {
				d.push([]);
			}
			d[0][0]=0;
		
			for(i in 1...(len1+1)) {
				d[i][0]=i;
			}
			for(i in 1...(len2+1)) {
				d[0][i]=i;
			}

			for(i in 1...(len1+1)) {
				for(j in 1...(len2+1)) {
					d[i][j] = Std.int(Math.min( Math.min(d[i - 1][j] + 1,d[i][j - 1] + 1),
								d[i - 1][j - 1] + (s1.charAt(i - 1) == s2.charAt(j - 1) ? 0 : 1) ));
				}
			}
			return d[len1][len2];
		
	}

	public static function filter_similar<T>(f:T->Int->Bool, cl:ImmutableList<{fst:T, snd:Int}>) : ImmutableList<T> {
		function loop(sl:ImmutableList<{fst:T, snd:Int}>) : ImmutableList<T> {
			return switch (sl) {
				case {fst:x, snd:i}::sl if (f(x, i)):
					x::loop(sl);
				case _: [];
			}
		}
		return loop(cl);
	}

	public static function get_similar(s:String, sl:ImmutableList<String>) : ImmutableList<String> {
		if (sl == Tl) { return []; }
		var cl = List.map(function (s2) { return {fst:s2, snd:levenshtein(s, s2)};}, sl);
		cl = List.sort(function (e1:{fst:String, snd:Int}, e2:{fst:String, snd:Int}) {
			var c1 = e1.snd; var c2 = e2.snd;
			if (c1 == c2) { return 0; }
			return (c1 > c2) ? 1 : -1;
		}, cl);
		return filter_similar(function(s2:String, i:Int) {
			return i <= Std.int(Math.min(s.length, s2.length) /3);
		}, cl);
	}

	public static function string_error_raise(s:String, sl:ImmutableList<String>, msg:String) : String {
		if (sl == Tl) { return msg; }
		var cl = get_similar(s, sl);
		return switch (cl) {
			case []: throw ocaml.Not_found.instance;
			case [s]: '${msg} (Suggestion: ${s})';
			case sl: '${msg} (Suggestion: ${List.join(", ", sl)})';
		}
	}

	public static function string_error(s:String, sl:ImmutableList<String>, msg:String) : String {
		try {
			return string_error_raise(s, sl, msg);
		}
		catch (e:ocaml.Not_found) {
			return msg;
		}
	}
}