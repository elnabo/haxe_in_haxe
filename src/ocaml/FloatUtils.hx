package ocaml;

enum Fpclass {
	FP_normal; // Normal number, none of the below
	FP_subnormal; // Number very close to 0.0, has reduced precision
	FP_zero; // Number is 0.0 or -0.0
	FP_infinite; // Number is positive or negative infinity
	FP_nan; // Not a number: result of an undefined operation
}

class FloatUtils {

	public static final min_int32:Float = -2147483648;
	public static final max_int32:Float = 2147483647;

	public static function float_of_string (f:String) : Float {
		var _f:Null<Float> = Std.parseFloat(f);
		if (_f == null) {
			throw new Failure("float_of_string");
		}
		return _f;
	}

	public static function classify_float(f:Float) : Fpclass {
		if (Math.isNaN(f)) { return FP_nan; }
		if (!Math.isFinite(f)) { return FP_infinite; }
		if (f == 0.0) { return FP_zero; }
		return FP_normal;
	}

	public static function toString(n:Float, precision:Int) : String {
		var str = ''+n;
		var l = str.length;
		var dot = str.indexOf(".");
		if (dot < 0) {
			return str + "." + [for (_ in 0...precision) "0"].join("");
		}
		dot++;
		var diff = (dot+precision) - l;
		if (diff > 0) {
			return str+[for (_ in 0...diff) "0"].join("");
		}
		else if (diff == 0) {
			return str;
		}
		else {
			return str.substr(0, dot+precision);
		}
	}

}