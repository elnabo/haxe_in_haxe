package context;

import core.Globals;
using equals.Equal;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
using ocaml.Cloner;
using ocaml.DynArray;
using ocaml.Hashtbl;
using ocaml.List;
using ocaml.PMap;

class Abort {
	public var s:String;
	public var pos:core.Globals.Pos;
	public function new (s:String, pos:core.Globals.Pos) {
		this.s = s;
		this.pos = pos;
	}
}

enum PackageRule {
	Forbidden;
	Directory (s:String);
	Remap (s:String);
}

typedef Stats = {
	s_files_parsed : ocaml.Ref<Int>,
	s_classes_built : ocaml.Ref<Int>,
	s_methods_typed : ocaml.Ref<Int>,
	s_macros_called : ocaml.Ref<Int>
}

enum CompilerMessage {
	CMInfo(s:String, pos:core.Globals.Pos);
	CMWarning(s:String, pos:core.Globals.Pos);
	CMError(s:String, pos:core.Globals.Pos);
}

/*
	The capture policy tells which handling we make of captured locals
	(the locals which are referenced in local functions)

	See details/implementation in Codegen.captured_vars
*/
enum Capture_policy {
	/* do nothing, let the platform handle it */
	CPNone;
	/* wrap all captured variables into a single-element array to allow modifications */
	CPWrapRef;
	/* similar to wrap ref, but will only apply to the locals that are declared in loops */
	CPLoopVars;
}

typedef Platform_config = {
	/** has a static type system, with not-nullable basic types (Int/Float/Bool) */
	pf_static : Bool,
	/** has access to the "sys" package */
	pf_sys : Bool,
	/** captured variables handling (see before) */
	pf_capture_policy : Capture_policy,
	/** when calling a method with optional args, do we replace the missing args with "null" constants */
	pf_pad_nulls : Bool,
	/** add a final return to methods not having one already - prevent some compiler warnings */
	pf_add_final_return : Bool,
	/** does the platform natively support overloaded functions */
	pf_overload : Bool,
	/** can the platform use default values for non-nullable arguments */
	pf_can_skip_non_nullable_argument : Bool,
	/** type paths that are reserved on the platform */
	pf_reserved_type_paths : ImmutableList<core.Path>
}

typedef CompilerCallback = {
	after_typing : ImmutableList<ImmutableList<core.Type.ModuleType>->Void>,
	before_dce : ImmutableList<Void->Void>,
	after_generation : ImmutableList<Void->Void>
}

typedef SharedDisplayInformation = {
	import_positions : Map<core.Globals.Pos, {b:ocaml.Ref<Bool>, l:ImmutableList<core.Ast.PlacedName>}>,
	diagnostics_messages : ImmutableList<{
		s:String,
		pos:core.Globals.Pos,
		t:context.displaytypes.diagnosticsseverity.T
	}>,
	type_hints : Map<core.Globals.Pos, core.Type.T>,
	document_symbols : ImmutableList<{
		s:String,
		l:DynArray<context.displaytypes.symbolinformation.T>
	}>,
	removable_code : ImmutableList<{s:String, p1:core.Globals.Pos, p2:core.Globals.Pos}>
}

typedef DisplayInformation = {
	unresolved_identifiers : ImmutableList<{
		s:String,
		pos:core.Globals.Pos,
		l:ImmutableList<{
			fst:String,
			snd:context.common.identifiertype.T
		}>
	}>,
	interface_field_implementations : ImmutableList<{
		c1:core.Type.TClass,
		cf1:core.Type.TClassField,
		c2:core.Type.TClass,
		cf2:Option<core.Type.TClassField>
	}>
}

// This information is shared between normal and macro context.
typedef SharedContext = {
	shared_display_information : SharedDisplayInformation
}

@:structInit
class Context {
	public var version : Int;
	public var args : ImmutableList<String>;
	public var shared : SharedContext;
	public var display_information : DisplayInformation;
	public var sys_args : ImmutableList<String>;
	public var display : context.common.DisplayMode.Settings;
	public var debug: Bool;
	public var verbose : Bool;
	public var foptimize : Bool;
	public var platform : core.Globals.Platform;
	public var config : Platform_config;
	public var std_path : ImmutableList<String>;
	public var class_path : ImmutableList<String>;
	public var main_class : Option<core.Path>;
	public var package_rules : Map<String, PackageRule>;
	public var error : String -> core.Globals.Pos -> Void;
	public var warning : String -> core.Globals.Pos -> Void;
	public var load_extern_type : ImmutableList<core.Path->core.Globals.Pos->Option<{s:String, pack:core.Ast.Package}>>; // allow finding types which are not in source
	public var callbacks : CompilerCallback;
	public var defines : core.Define;
	public var print : String -> Void;
	public var get_macros : Void -> Option<Context>;
	public var run_command : String -> Int;
	public var file_lookup_cache : Map<String, Option<String>>;
	public var parser_cache : Map<String, ImmutableList<core.Ast.TypeDecl>>;
	public var cached_macros : Map<
		{path:core.Path, s:String},
		{
			l:ImmutableList<{s:String, b:Bool, t:core.Type.T}>,
			t:core.Type.T,
			c:core.Type.TClass,
			cf:core.Type.TClassField
		}
	>;
	public var stored_typed_exprs : Map<Int, core.Type.TExpr>;
	// output
	public var file : String;
	public var flash_version : Float;
	public var features : Map<String, Bool>;
	public var modules : ImmutableList<core.Type.ModuleDef>;
	public var main : Option<core.Type.TExpr>;
	public var types : ImmutableList<core.Type.ModuleType>;
	public var resources : Map<String, String>;
	public var neko_libs : ImmutableList<String>;
	public var include_files : ImmutableList<{a:String, b:String}>;
	// ocaml type: (string * (unit -> Swf.swf) * (unit -> ((string list * string),As3hl.hl_class) Hashtbl.t)) list;
	public var swf_libs : ImmutableList<Dynamic>;
	// ocaml type: (string * bool * (unit -> unit) * (unit -> (path list)) * (path -> ((JData.jclass * string * string) option))) list; (* (path,std,close,all_files,lookup) *)
	public var java_libs : ImmutableList<Dynamic>;
	// ocaml type: (string * bool * (unit -> path list) * (path -> IlData.ilclass option)) list; (* (path,std,all_files,lookup) *)
	public var net_libs : ImmutableList<Dynamic>;
	public var net_std : ImmutableList<String>;
	public var net_path_map : Map<core.Path, {
		a1:ImmutableList<String>,
		a2:ImmutableList<String>,
		s:String
	}>;
	public var c_args : ImmutableList<String>;
	public var js_gen : Option<Void->Void>;
	// typing
	public var basic : core.Type.BasicTypes;
	public var memory_marker : ImmutableList<Float>;
}

class Common {

	// Define
	public static function defined (com:Context, s:core.Define.StrictDefined) : Bool {
		return core.Define.defined(com.defines, s);
	}

	public static function defined_value (com:Context, v:core.Define.StrictDefined) {
		return core.Define.defined_value(com.defines, v);
	}

	public static function define_value (com:Context, k:core.Define.StrictDefined, v:String) {
		core.Define.define_value(com.defines, k, v);
	}

	public static function define (com:Context, v:core.Define.StrictDefined) {
		core.Define.define(com.defines, v);
	}

	public static function raw_define (com:Context, v:String) {
		core.Define.raw_define(com.defines, v);
	}

	public static var display_default = new ocaml.Ref<context.common.displaymode.T>(DMNone);

	public static var stats = { // all int pointers
		s_files_parsed: new ocaml.Ref(0),
		s_classes_built: new ocaml.Ref(0),
		s_methods_typed: new ocaml.Ref(0),
		s_macros_called: new ocaml.Ref(0)
	};

	public static var default_config(get, never):Platform_config;
	static function get_default_config () : Platform_config {
		return {
			pf_static : true,
			pf_sys : true,
			pf_capture_policy : CPNone,
			pf_pad_nulls : false,
			pf_add_final_return : false,
			pf_overload : false,
			pf_can_skip_non_nullable_argument : true,
			pf_reserved_type_paths : []
		};
	}

	public static function compiler_message_string (msg:Dynamic) : String {
		var str:String;
		var pos:core.Globals.Pos;
		switch (msg) {
			case CMInfo(s,p), CMError(s,p):
				str = s;
				pos = p;
			case CMWarning(s,p):
				str = "Warning : " + s;
				pos = p;
		}
		if (pos.equals(core.Globals.null_pos)) {
			return str;
		}
		else {
			function error_printer (file:String, line:Int) : String {
				return '${file}:${line}';
			}
			var epos = syntax.Lexer.get_error_pos(error_printer, pos);
			str = str.split("\n").join("\n"+epos+" : ");
			return '${epos} : ${str}';
		}
	}

	public static function create(version:Int, s_version:Void->String, args:ImmutableList<String>) : Context {

		var m:core.Type.T = core.Type.mk_mono();
		var defines = new Map<String, String>();
		defines.set("true", "1");
		defines.set("source-header", "Generated by Haxe "+s_version());
		if (display_default.get() !=  DMNone) {
			defines.set("display","1");
		}
		// let defines =
			// PMap.add "true" "1" (
			// PMap.add "source-header" ("Generated by Haxe " ^ s_version) (
			// if !display_default <> DisplayMode.DMNone then PMap.add "display" "1" PMap.empty else PMap.empty))

		// in
		var ctx:Context = {
			version : version,
			args : args,
			shared : {
				shared_display_information : {
					import_positions : new Map<core.Globals.Pos, {b:ocaml.Ref<Bool>, l:ImmutableList<core.Ast.PlacedName>}>(),
					diagnostics_messages : [],
					type_hints : new Map<core.Globals.Pos, core.Type.T>(),
					document_symbols : [],
					removable_code : []
				}
			},
			display_information : {
				unresolved_identifiers : [],
				interface_field_implementations : [],
			},
			sys_args : args,
			debug : false,
			display : context.common.DisplayMode.create(display_default.get()),
			verbose : false,
			foptimize : true,
			features : new Map<String, Bool>(),
			platform : Cross,
			config : default_config,
			print : function (s:String) { std.Sys.println(s); },
			run_command : function (s:String) { return std.Sys.command(s); },
			std_path : [],
			class_path : [],
			main_class : None,
			package_rules : new Map<String, PackageRule>(),
			file : "",
			types : [],
			callbacks : create_callbacks(),
			modules : [],
			main : None,
			flash_version : 10.,
			resources : new Map<String, String>(),
			swf_libs : [],
			java_libs : [],
			net_libs : [],
			net_std : [],
			net_path_map : new Map<core.Path, {
				a1:ImmutableList<String>,
				a2:ImmutableList<String>,
				s:String
			}>(),
			c_args : [],
			neko_libs : [],
			include_files : [],
			js_gen : None,
			load_extern_type : [],
			defines : {values:defines, defines_signature:None},
			get_macros : function () { return None; },
			warning : function (s:String, pos:core.Globals.Pos) { trace("Shall not be seen"); throw false; },
			error : function (s:String, pos:core.Globals.Pos) { trace("Shall not be seen"); throw false; },
			basic : {
				tvoid : m,
				tint : m,
				tfloat : m,
				tbool : m,
				tnull : function (t:core.Type.T) { trace("Shall not be seen"); throw false; },
				tstring : m,
				tarray : function (t:core.Type.T) { trace("Shall not be seen"); throw false; }
			},
			file_lookup_cache : new Map<String, Option<String>>(),
			stored_typed_exprs : new Map<Int, core.Type.TExpr>(),
			cached_macros : new Map<
				{path:core.Path, s:String},
				{
					l:ImmutableList<{s:String, b:Bool, t:core.Type.T}>,
					t:core.Type.T,
					c:core.Type.TClass,
					cf:core.Type.TClassField
				}
			>(),
			memory_marker : memory_marker(),
			parser_cache : new Map<String, ImmutableList<core.Ast.TypeDecl>>()
		};

		return ctx;
	}

	public static function create_callbacks () : CompilerCallback {
		return {
			after_typing : [],
			before_dce : [],
			after_generation : []
		};
	}

	public static function log (com:Context, str:String) {
		if (com.verbose) {
			// com.print(str + "\n");
			com.print(str);
		}
	}

	public static function clone (com:Context) : Context {
		var t = com.basic.clone();
		var clone = com.clone();
		clone.basic = t;
		clone.main_class = None;
		clone.features = new Map<String, Bool>();
		clone.file_lookup_cache = new Map<String, Option<String>>();
		clone.callbacks = create_callbacks();
		clone.display_information = {
			unresolved_identifiers:[],
			interface_field_implementations: []
		};
		clone.defines = {
			values: com.defines.values,
			defines_signature: com.defines.defines_signature
		};
		return clone;
	}

	public static function file_time (file:String) : Float {
		try {
			return sys.FileSystem.stat(file).ctime.getTime();
		}
		catch (_:Any) {
			return 0.0;
		}
	}

	public static function file_extension(file:String) {
		var split = file.split(".");
		if (split.length == 0) { return ""; }
		return split[split.length - 1].toLowerCase();
	}

	static var flash_versions_internal: ImmutableList<{a:Float, b:String}> = Tl;
	public static var flash_versions(get, never) : ImmutableList<{a:Float, b:String}>;
	static function get_flash_versions () : ImmutableList<{a:Float, b:String}> {
		if (flash_versions_internal == Tl) {
			flash_versions_internal = [for (v in [9.,10.,10.1,10.2,10.3,11.,11.1,11.2,11.3,11.4,11.5,11.6,11.7,11.8,11.9,12.0,13.0,14.0,15.0,16.0,17.0]) {
				var maj = Std.int(v);
				var min = Std.int(v * 10) % 10;
				{a:v, b:maj + "" + ((min == 0) ? "" : "_") + maj}
			}];
		}
		return flash_versions_internal;
	}

	public static function get_config (com:Context) : Platform_config {
		var defined = function (f:core.Define.StrictDefined) : Bool {
			return com.defines.values.exists(core.Define.infos(f).a);
		};
		return switch (com.platform) {
			case Cross:
				default_config;
			case Js:
				var config = default_config;
				config.pf_static = false;
				config.pf_sys = false;
				config.pf_capture_policy = CPLoopVars;
				config.pf_reserved_type_paths = [new core.Path([],"Object"), new core.Path([],"Error")];
				config;
			case Lua:
				var config = default_config;
				config.pf_static = false;
				config.pf_capture_policy = CPLoopVars;
				config;
			case Neko:
				var config = default_config;
				config.pf_static = false;
				config.pf_pad_nulls = true;
				config;
			case Flash:
				if (defined(As3)) {
					var config = default_config;
					config.pf_sys = false;
					config.pf_capture_policy = CPLoopVars;
					config.pf_add_final_return = true;
					config.pf_can_skip_non_nullable_argument = false;
					config;
				}
				else {
					var config = default_config;
					config.pf_sys = false;
					config.pf_capture_policy = CPLoopVars;
					config.pf_can_skip_non_nullable_argument = false;
					config.pf_reserved_type_paths = [new core.Path([],"Object"), new core.Path([],"Error")];
					config;
				}
			case Php:
				var config = default_config;
				config.pf_static = false;
				config;
			case Cpp:
				var config = default_config;
				config.pf_capture_policy = CPWrapRef;
				config.pf_pad_nulls = true;
				config.pf_add_final_return = true;
				config;
			case Cs:
				var config = default_config;
				config.pf_capture_policy = CPWrapRef;
				config.pf_pad_nulls = true;
				config.pf_overload = true;
				config;
			case Java:
				var config = default_config;
				config.pf_capture_policy = CPWrapRef;
				config.pf_pad_nulls = true;
				config.pf_overload = true;
				config;
			case Python:
				var config = default_config;
				config.pf_static = false;
				config.pf_capture_policy = CPLoopVars;
				config;
			case Hl:
				var config = default_config;
				config.pf_capture_policy = CPWrapRef;
				config.pf_pad_nulls = true;
				config.pf_can_skip_non_nullable_argument = false;
				config;
			case Eval:
				var config = default_config;
				config.pf_static = false;
				config.pf_pad_nulls = true;
				config;
		};
	}

	public static function memory_marker () : ImmutableList<Float> {
		return [std.Sys.time()];
	}

	public static function init_platform (com:Context, pf:core.Globals.Platform) {
		com.platform = pf;
		var name = core.Globals.platform_name(pf);
		function forbid(acc, p) {
			return if (p == name || PMap.mem(p, acc)) {
				acc;
			}
			else {
				PMap.add(p, Forbidden, acc);
			}
		}
		com.package_rules = List.fold_left(forbid, com.package_rules, List.map(core.Globals.platform_name, core.Globals.platforms));
		com.config = get_config(com);
		if (com.config.pf_static) {
			define(com, Static);
		}
		if (com.config.pf_sys) {
			define(com, Sys);
		}
		else {
			com.package_rules.set("sys", Forbidden);
		}
		raw_define(com, name);
	}

	public static function has_dce (com:Context) : Bool {
		try {
			return defined_value(com, Dce) != "no";
		}
		catch (_:ocaml.Not_found) {
			return false;
		}
	}

	public static function platform (ctx:Context, p:Platform) : Bool {
		return ctx.platform == p;
	}

	public static function find_file (ctx:Context, f:String) : String {
		return try {
			var v = Hashtbl.find(ctx.file_lookup_cache,f);
			switch (v) {
				case None: throw ocaml.Exit.instance;
				case Some(f): f;
			}
		}
		catch (_:ocaml.Exit) {
			throw ocaml.Not_found.instance;
		}
		catch (_:ocaml.Not_found) {
			function loop(had_empty:Bool, l:ImmutableList<String>) {
				return switch (l) {
					case [] if (had_empty): throw ocaml.Not_found.instance;
					case []: loop(true, [""]);
					case p::l:
						var l:ImmutableList<String> = l;
						var file = p + f;
						if (sys.FileSystem.exists(file)) {
							return try {
								var ext = file.lastIndexOf(".");
								if (ext == -1) { throw ocaml.Not_found.instance; }
								var file_pf = file.substr(0, ext+1) + core.Globals.platform_name(ctx.platform) + file.substr(ext, (file.length - ext));
								if (!defined(ctx, CoreApi) && sys.FileSystem.exists(file_pf)) {
									file_pf;
								}
								else {
									file;
								}
							}
							catch (_:ocaml.Not_found) {
								file;
							}
						}
						else {
							loop(had_empty || p == "", l);
						}

				}
			}
			var r = try {
				Some(loop(false, ctx.class_path));
			}
			catch (_:ocaml.Not_found) {
				None;
			}
			Hashtbl.add(ctx.file_lookup_cache,f, r);
			switch (r) {
				case None: throw ocaml.Not_found.instance;
				case Some(s): s;
			}
		}
	}

	public static function add_diagnostics_message (com:Context, s:String, p:core.Globals.Pos, sev:context.displaytypes.diagnosticsseverity.T) {
		var di = com.shared.shared_display_information;
		di.diagnostics_messages = ({s:s, pos:p, t:sev}):: di.diagnostics_messages;
	}
}