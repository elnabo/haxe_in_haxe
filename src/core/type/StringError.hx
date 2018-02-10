package core.type;

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

	public static function filter_similar<T>(f:{fst:T, snd:Int}->Bool, cl:Array<{fst:T, snd:Int}>) : Array<T> {
		var r:Array<T> = [];
		for (e in cl) {
			if (f(e)) {
				r.push(e.fst);
			}
		}
		return r;
	}

	public static function get_similar(s:String, sl:Array<String>) : Array<String> {
		if (sl.length == 0) {
			return [];
		}
		var cl = [for (el in sl) {fst:el, snd:levenshtein(s, el)}];
		cl.sort(function (c1:{fst:String, snd:Int}, c2:{fst:String, snd:Int}) {
			if (c1.snd == c2.snd) { return 0; }
			return (c1.snd > c2.snd) ? 1 : -1;
		});
		return filter_similar(function(el:{fst:String, snd:Int}) {
			return el.snd <= Std.int(Math.min(s.length, el.fst.length) /3);
		}, cl);
	}

	public static function string_error_raise(s:String, sl:Array<String>, msg:String) : String {
		if (sl.length == 0) {
			return msg;
		}
		else {
			var cl = get_similar(s, sl);
			if (cl.length == 0) {
				throw ocaml.Not_found.instance;
			}

			return msg + " (Suggestions: "+sl.join(", ") + ")";
		}
	}

	public static function string_error(s:String, sl:Array<String>, msg:String) : String {
		try {
			return string_error_raise(s, sl, msg);
		}
		catch (e:ocaml.Not_found) {
			return msg;
		}
	}
}