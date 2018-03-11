package core;

import haxe.ds.ImmutableList;
import ocaml.List;

using equals.Equal;

class Path {

	public var a : ImmutableList<String>;
	public var b : String;
	public function new (a:ImmutableList<String>, b:String) {
		this.a = a;
		this.b = b;
	}

	public static function compare (a:Path, b:Path) : Int {
		if (a.equals(b)) { return 0; }

		var _a:Array<String> = a.a;
		var _b:Array<String> = b.a;
		var la = _a.length;
		var lb = _a.length;
		for (i in 0...Std.int(Math.min(la, lb))) {
			if (_a[i] > _b[i]) {
				return 1;
			}
			if (_a[i] < _b[i]) {
				return -1;
			}
		}
		if (la > lb) { return 1; }
		if (la < lb) { return -1; }

		if (a.b > b.b) { return 1; }
		if (a.b < b.b) { return -1; }
		return 0;
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
	public static function get_path_parts (f:String) : ImmutableList<String> {
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
		var cl = get_path_parts(f);
		function error (msg) : Path {
			var msg = "Could not process argument "+f+"\n"+msg;
			throw new ocaml.Failure(msg);
		}
		function invalid_char (x:String) {
			for (i in 1...x.length) {
				var c = x.charCodeAt(i);
				if ( (c>="A".code && c<="Z".code) ||
					 (c>="a".code && c<="z".code) ||
					 (c>="0".code && c<="9".code) ||
					 (c=="_".code) || (c<=".".code)) {
				}
				else {
					error("invalid character: " + x.charAt(i));
				}
			}
		}

		function loop (l:ImmutableList<String>) : core.Path {
			return switch (l) {
				case []: error("empty part");
				case [x]:
					invalid_char(x);
					new Path([], x);
				case x::l:
					if (x.length == 0) {
						error("empty part");
					}
					else if (x.charCodeAt(0) < 'a'.code || x.charCodeAt(0) > 'z'.code) {
						error("Package name must start with a lower case character");
					}
					invalid_char(x);
					var path = loop(l);
					new Path(x::path.a, path.b);
			}
		}
		return loop(cl);
	}

	public static function starts_uppercase (x:String) : Bool {
		var c = x.charCodeAt(0);
		return (c == "_".code || (c >= "A".code && c <= "Z".code));
	}

	public static function check_uppercase (x:String) : Void {
		if (x.length == 0) {
			throw new ocaml.Failure("empty part");
		}
		else if (!starts_uppercase(x)) {
			throw new ocaml.Failure("Class name must start with uppercase character");
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

	public static function flat_path (path:Path) : String {
		var p = path.a; var s = path.b;
		// Replace _ with _$ in paths to prevent name collisions.
		function escape (str:String) {
			return str.split("_").join("_$");
		}
		return switch (p) {
			case []: escape(s);
			case _: List.join("_", List.map(escape, p)) + "_" + escape(s);
		}
	}

	public static function mkdir_from_path (p:String) {
		sys.FileSystem.createDirectory(p);
	}
}