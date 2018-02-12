package core;

import haxe.ds.ImmutableList;
import ocaml.List;

class Pos {
	public final pfile : String;
	public final pmin : Int;
	public final pmax : Int;
	public function new (pfile:String, pmin:Int, pmax:Int) {
		this.pfile = pfile;
		this.pmin = pmin;
		this.pmax = pmax;
	}

	@:op(A == B) static function equals (a:Pos, b:Pos) : Bool {
		return a.pmin == b.pmin && a.pmax == b.pmax && a.pfile == b.pfile;
	}

	@:op(A != B) static function diff (a:Pos, b:Pos) : Bool {
		return a.pmin != b.pmin || a.pmax != b.pmax || a.pfile != b.pfile;
	}
}

enum Platform {
	Cross;
	Js;
	Lua;
	Neko;
	Flash;
	Php;
	Cpp;
	Cs;
	Java;
	Python;
	Hl;
	Eval;
}

class Globals {
	public static var version = 4000;
	public static var version_major = version / 1000;
	public static var version_minor = (version % 1000) / 100;
	public static var version_revision = (version % 100);

	public static var macro_platform:Platform = Neko;

	// TODO: how to do that for bytecode target
	#if js
	public static var is_windows = false;
	#else
	public static var is_windows = std.Sys.systemName() == "Windows" || std.Sys.systemName() == "Cygwin";
	#end

	public static var platforms:ImmutableList<Platform> = [Js, Lua, Neko, Flash, Php, Cpp, Cs, Java, Python, Hl, Eval];

	public static function platform_name (p:Platform) : String {
		return switch(p) {
			case Cross : "cross";
			case Js : "js";
			case Lua : "lua";
			case Neko : "neko";
			case Flash : "flash";
			case Php : "php";
			case Cpp : "cpp";
			case Cs : "cs";
			case Java : "java";
			case Python : "python";
			case Hl : "hl";
			case Eval : "eval";
		};
	}

	public static function platform_list_help (list:ImmutableList<Platform>) : String {
		return switch (list) {
			case []: "";
			case [p]: " (" + platform_name(p) + " only)";
			case pl: " (for " + List.join(",", List.map(platform_name, pl)) + ")";
		};
	}

	public static var null_pos(get, never):Pos;
	static inline function get_null_pos () {
		return new Pos("?", -1, -1);
	}
	// public static final null_pos = new Pos("?", -1, -1);

	public static function s_type_path (p:Path) {
		return switch (p.a) {
			case []: p.b;
			case _: List.join(".",p.a) + "." + p.b;
		}
	}
}