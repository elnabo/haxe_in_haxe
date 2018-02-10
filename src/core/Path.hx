package core;

class Path {

	public var a : Array<String>;
	public var b : String;
	@:structInit
	public function new (a:Array<String>, b:String) {
		this.a = a;
		this.b = b;
	}

	@:op(A == B) static function equals (a:Path, b:Path) : Bool {
		if ((a.b != b.b) || (a.a.length != b.a.length)) { return false; }
	
		for (i in 0...a.a.length) {
			if (a.a[i] != b.a[i]) {
				return false;
			}
		}
		return true;
	}

	@:op(A != B) static function diff (a:Path, b:Path) : Bool {
		return !equals(a, b);
	}

	@:op(A > B) public static function gt (a:Path, b:Path) : Int {
		if (a == b) { return 0; }

		var la = a.a.length;
		var lb = b.a.length;
		for (i in 0...Std.int(Math.min(la, lb))) {
			if (a.a[i] > b.a[i]) {
				return 1;
			}
			if (a.a[i] < b.a[i]) {
				return -1;
			}
		}
		if (la > lb) { return 1; }
		if (la < lb) { return -1; }

		if (a.b > b.b) { return 1; }
		if (a.b < b.b) { return -1; }
		return 0;
	}

	@:op(A < B) static function lt (a:Path, b:Path) : Int {
		return -1*gt(a,b);
	}

	/*
	 * this function is quite weird: it tries to determine whether the given
	 * argument is a .hx file path with slashes or a dotted module path and
	 * based on that it returns path "parts", which are basically a list of
	 * either folders or packages (which are folders too) appended by the module name
	 *
	 * TODO: i started doubting my sanity while writing this comment, let's somehow
	 * refactor this stuff so it doesn't mix up file and module paths and doesn't introduce
	 * the weird "path part" entity.
	*/
	public static function get_path_parts (f:String) : Array<String> {
		var l = f.length;
		if (l > 3 && f.substr(l-3, 3) == ".hx"){
			var ff = f.substr(0, l-3); /// strip the .hx;
			return ~/[\/\\]/g.split(ff);
			// 	let f = String.sub f 0 (l-3) in (* strip the .hx *)
		// 	ExtString.String.nsplit (String.concat "/" (ExtString.String.nsplit f "\\")) "/" (* TODO: wouldn't it be faster to Str.split here? *)
		}
		else {
			return f.split('.');
		}
	}


	public static function parse_path(f:String) : Path {
		var cl = get_path_parts(f).copy();
		var invalid_char = function (x:String) {
			for (i in 1...x.length) {
				var c = x.charCodeAt(i);
				if ( (c>="A".code && c<="Z".code) ||
					 (c>="a".code && c<="z".code) ||
					 (c>="0".code && c<="9".code) ||
					 (c=="_".code) || (c<=".".code)) {
				}
				else {
					var msg = "invalid character: " + x.charAt(i);
					throw "Could not process argument " + f + "\n" + msg;
				}
			}
		};

		var path : Array<String> = [];
		var name : String = null;
		while (true) {
			switch (cl.length) {
				case 0:
					throw "Could not process argument " + f + "\nempty part";
				case 1:
					var x = cl.shift();
					invalid_char(x);
					name = x;
					break;
				default:
					var x = cl.shift();
					if (x.length == 0) {
						throw "Could not process argument " + f + "\nempty part";
					}
					var c = x.charCodeAt(0);
					if (c < "a".code || c > "z".code ) {
						throw "Could not process argument " + f + "\nPackage name must start with a lower case character";
					}
					invalid_char(x);
					path.push(x);
			}
		}
		return new Path(path, name);
	}

	public static function starts_uppercase (x:String) : Bool {
		var c = x.charCodeAt(0);
		return (c == "_".code || (c >= "A".code && c <= "Z".code));
	}

	public static function check_uppercase (x:String) : Void {
		if (x.length == 0) {
			throw "empty part";
		}
		else if (!starts_uppercase(x)) {
			throw "Class name must start with uppercase character";
		}
	}

	public static function parse_type_path (s:String) : Path {
		var path = parse_path(s);
		check_uppercase(path.b);
		return path;
	}

	public static var path_sep : String = core.Globals.is_windows ? "\\" : "/";

	/** Returns absolute path. Doesn't fix path case on Windows. */
	public static function get_full_path (f:String) {
		if (f != null && sys.FileSystem.exists(f)) {
			return sys.FileSystem.absolutePath(f);
		}
		else {
			return f;
		}
	}

	/**
	 * Returns absolute path (on Windows ensures proper case with drive letter upper-cased)
	 * Use for returning positions from IDE support functions
	*/
	public static function get_real_path () : String->String {
		if (core.Globals.is_windows) {
			return function (p:String) : String {
				if (p != null && sys.FileSystem.exists(p) ) {
					return sys.FileSystem.absolutePath(p);
				}
				else {
					return p;
				}
			};
		}
		else {
			return get_full_path;
		}
	}

	/*
	 * Returns absolute path guaranteed to be the same for different letter case.
	 * Use where equality comparison is required, lowercases the path on Windows
	 */
	public static var unique_full_path (get, never) : String->String;
	public static function get_unique_full_path () : String->String {
		if (core.Globals.is_windows) {
			return function(f:String) {
				return get_full_path(f).toLowerCase();
			}
		}
		else {
			return get_full_path;
		}
	}

	public static function add_trailing_slash (p:String) : String {
		// var l = p.length;
		// if (l == 0) {
		// 	return "./";
		// }
		// else {
		// 	return switch (p.charAt(l-1)) {
		// 		case "\\", "/": p;
		// 		default: p + "/";
		// 	};
		// }
		return haxe.io.Path.addTrailingSlash(p);
	}

	public static function mkdir_from_path (p:String) {
		sys.FileSystem.createDirectory(p);
	}
}