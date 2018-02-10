package core;

class Numeric {

	/*  Taken from OCaml source typing/oprint.ml

		This is a better version of string_of_float which prints without loss of precision
		so that float_of_string (float_repres x) = x for all floats x
	*/
	public static function valid_float_lexeme (s:String) : String {
		var l = s.length;
		function loop (i:Int) : String {
			if (i >= l) {
				return s + ".";
			}
			else {
				return switch (s.charAt(i)) {
					case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '-': loop(i+1);
					case _: s;
				}
			}
		}
		return loop(0);
	}

	public static function float_repres (f) : String {
		return switch(ocaml.FloatUtils.classify_float(f)) {
			case FP_nan: "nan";
			case FP_infinite:
				(f < 0.0) ? "neg_infinity" : "infinity";
			case _:
				var s1 = ocaml.FloatUtils.toString(f, 12);
				var float_val = if (f == Std.parseFloat(s1)) {
					s1;
				}
				else {
					var s2 = ocaml.FloatUtils.toString(f, 15);
					if (f == Std.parseFloat(s2)) {
						s2;
					}
					else {
						ocaml.FloatUtils.toString(f, 18);
					}
				}
				valid_float_lexeme(float_val);
		}
	}

}