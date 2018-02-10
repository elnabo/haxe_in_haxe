package context;

import core.Globals;
// import core.Define.StrictDefined;

import haxe.ds.Option;
using ocaml.Cloner;

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
enum CapturePolicy {
	/* do nothing, let the platform handle it */
	CPNone;
	/* wrap all captured variables into a single-element array to allow modifications */
	CPWrapRef;
	/* similar to wrap ref, but will only apply to the locals that are declared in loops */
	CPLoopVars;
}

typedef PlatformConfig = {
	/** has a static type system, with not-nullable basic types (Int/Float/Bool) */
	pf_static : Bool,
	/** has access to the "sys" package */
	pf_sys : Bool,
	/** captured variables handling (see before) */
	pf_capture_policy : CapturePolicy,
	/** when calling a method with optional args, do we replace the missing args with "null" constants */
	pf_pad_nulls : Bool,
	/** add a final return to methods not having one already - prevent some compiler warnings */
	pf_add_final_return : Bool,
	/** does the platform natively support overloaded functions */
	pf_overload : Bool,
	/** can the platform use default values for non-nullable arguments */
	pf_can_skip_non_nullable_argument : Bool,
	/** type paths that are reserved on the platform */
	pf_reserved_type_paths : Array<core.Path>
}

typedef CompilerCallback = {
	after_typing : Array<Array<core.Type.ModuleType>->Void>,
	before_dce : Array<Void->Void>,
	after_generation : Array<Void->Void>
}

typedef SharedDisplayInformation = {
	import_positions : Map<core.Globals.Pos, {b:ocaml.Ref<Bool>, l:Array<core.Ast.PlacedName>}>,
	diagnostics_messages : Array<{
		s:String,
		pos:core.Globals.Pos,
		t:context.displaytypes.diagnosticsseverity.T
	}>,
	type_hints : Map<core.Globals.Pos, core.Type.T>,
	document_symbols : Array<{
		s:String,
		l:Array<context.displaytypes.symbolinformation.T>
	}>,
	removable_code : Array<{s:String, p1:core.Globals.Pos, p2:core.Globals.Pos}>
}

typedef DisplayInformation = {
	unresolved_identifiers : Array<{
		s:String,
		pos:core.Globals.Pos,
		l:Array<{
			fst:String,
			snd:context.common.identifiertype.T
		}>
	}>,
	interface_field_implementations : Array<{
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
	public var args : Array<String>;
	public var shared : SharedContext;
	public var display_information : DisplayInformation;
	public var sys_args : Array<String>;
	public var display : context.common.DisplayMode.Settings;
	public var debug: Bool;
	public var verbose : Bool;
	public var foptimize : Bool;
	public var platform : core.Globals.Platform;
	public var config : PlatformConfig;
	public var std_path : Array<String>;
	public var class_path : Array<String>;
	public var main_class : Option<core.Path>;
	public var package_rules : Map<String, PackageRule>;
	public var error : String -> core.Globals.Pos -> Void;
	public var warning : String -> core.Globals.Pos -> Void;
	public var load_extern_type : Array<core.Path->core.Globals.Pos->Option<{s:String, pack:core.Ast.Package}>>; // allow finding types which are not in source
	public var callbacks : CompilerCallback;
	public var defines : core.Define;
	public var print : String -> Void;
	public var get_macros : Void -> Option<Context>;
	public var run_command : String -> Int;
	public var file_lookup_cache : Map<String, Option<String>>;
	public var parser_cache : Map<String, Array<core.Ast.TypeDecl>>;
	public var cached_macros : Map<
		{path:core.Path, s:String},
		{
			l:Array<{s:String, b:Bool, t:core.Type.T}>,
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
	public var modules : Array<core.Type.ModuleDef>;
	public var main : Option<core.Type.TExpr>;
	public var types : Array<core.Type.ModuleType>;
	public var resources : Map<String, String>;
	public var neko_libs : Array<String>;
	public var include_files : Array<{a:String, b:String}>;
	// ocaml type: (string * (unit -> Swf.swf) * (unit -> ((string list * string),As3hl.hl_class) Hashtbl.t)) list;
	public var swf_libs : Array<Dynamic>;
	// ocaml type: (string * bool * (unit -> unit) * (unit -> (path list)) * (path -> ((JData.jclass * string * string) option))) list; (* (path,std,close,all_files,lookup) *)
	public var java_libs : Array<Dynamic>;
	// ocaml type: (string * bool * (unit -> path list) * (path -> IlData.ilclass option)) list; (* (path,std,all_files,lookup) *)
	public var net_libs : Array<Dynamic>;
	public var net_std : Array<String>;
	public var net_path_map : Map<core.Path, {
		a1:Array<String>,
		a2:Array<String>,
		s:String
	}>;
	public var c_args : Array<String>;
	public var js_gen : Option<Void->Void>;
	// typing
	public var basic : core.Type.BasicTypes;
	public var memory_marker : Array<Float>;
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

	public static var default_config(get, never):PlatformConfig;
	static function get_default_config () : PlatformConfig {
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
		if (pos.pfile == core.Globals.null_pos.pfile &&
			pos.pmax == core.Globals.null_pos.pmax &&
			pos.pmin == core.Globals.null_pos.pmin) {
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

	public static function create(version:Int, s_version:Void->String, args:Array<String>) : Context {
		
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
					import_positions : new Map<core.Globals.Pos, {b:ocaml.Ref<Bool>, l:Array<core.Ast.PlacedName>}>(),
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
				a1:Array<String>,
				a2:Array<String>,
				s:String
			}>(),
			c_args : [],
			neko_libs : [],
			include_files : [],
			js_gen : None,
			load_extern_type : [],
			defines : {values:defines, defines_signature:None},
			get_macros : function () { return None; },
			warning : function (s:String, pos:core.Globals.Pos) { throw false; },
			error : function (s:String, pos:core.Globals.Pos) { throw false; },
			basic : {
				tvoid : m,
				tint : m,
				tfloat : m,
				tbool : m,
				tnull : function (t:core.Type.T) { throw false; return null; },
				tstring : m,
				tarray : function (t:core.Type.T) { throw false; return null; }
			},
			file_lookup_cache : new Map<String, Option<String>>(),
			stored_typed_exprs : new Map<Int, core.Type.TExpr>(),
			cached_macros : new Map<
				{path:core.Path, s:String},
				{
					l:Array<{s:String, b:Bool, t:core.Type.T}>,
					t:core.Type.T,
					c:core.Type.TClass,
					cf:core.Type.TClassField
				}
			>(),
			memory_marker : memory_marker(),
			parser_cache : new Map<String, Array<core.Ast.TypeDecl>>()
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

	static var flash_versions_internal = [9.,10.,10.1,10.2,10.3,11.,11.1,11.2,11.3,11.4,11.5,11.6,11.7,11.8,11.9,12.0,13.0,14.0,15.0,16.0,17.0];
	public static var flash_versions(get, never) : Array<{a:Float, b:String}>;
	static function get_flash_versions () : Array<{a:Float, b:String}> {
		return [ for (v in flash_versions_internal)
			{
				var maj = Std.int(v);
				var min = Std.int(v * 10) % 10;
				{a:v, b:maj + "" + ((min == 0) ? "" : "_") + maj}
			}
		];
	}

	public static function get_config (com:Context) : PlatformConfig {
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

	public static function memory_marker () : Array<Float> {
		return [std.Sys.time()];
	}

	public static function init_platform (com:Context, pf:core.Globals.Platform) {
		com.platform = pf;
		var name = core.Globals.platform_name(pf);
		var acc = new Map<String, PackageRule>();
		for (p in core.Globals.platforms) {
			var pn = core.Globals.platform_name(p);
			if (pn == name || acc.exists(pn)) {

			}
			else {
				acc.set(pn, Forbidden);
			}
		}
		com.package_rules = acc;
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

	public static function find_file (ctx:Context, f:String) : String {
		var v = ctx.file_lookup_cache.get(f);
		if (v != null) {
			switch (v) {
				case None:
				case Some(s): return s;
			}
		}

		var had_empty = false;
		var acc = ctx.class_path.copy();

		var r : Option<String> = None;
		try {
			while (true) {
				if (acc.length == 0) {
					if (had_empty) { throw ocaml.Not_found.instance; }
					else {
						had_empty = true;
						acc = [""];
					}
				}
				else {
					var p = acc.shift();
					var file = p + f;
					if (sys.FileSystem.exists(file)) {
						var ext = file.lastIndexOf(".");
						if (ext == -1) {
							r = Some(file);
							break;
						}
						var file_pf = file.substr(0, ext+1) + core.Globals.platform_name(ctx.platform) + file.substr(ext, (file.length - ext));
						if (!defined(ctx, CoreApi) && sys.FileSystem.exists(file_pf)) {
							r = Some(file_pf);
							break;
						}
						else {
							r = Some(file);
							break;
						}
					}
					else {
						had_empty = (had_empty || p =="");
					}
				}
			}
		}
		catch (e:ocaml.Not_found) {
			 r = None;
		}

		ctx.file_lookup_cache.set(f, r);
		switch (r) {
			case None: throw ocaml.Not_found.instance;
			case Some(s): return s;
		}
	}

	public static function add_diagnostics_message (com:Context, s:String, p:core.Globals.Pos, sev:context.displaytypes.diagnosticsseverity.T) {
		var di = com.shared.shared_display_information;
		di.diagnostics_messages.unshift({s:s, pos:p, t:sev});
	}
}