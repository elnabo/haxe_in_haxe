package core;

import haxe.ds.ImmutableList;
import ocaml.List;
import ocaml.PMap;
import ocaml.Ref;

enum StrictDefined {
	AbsolutePath;
	AdvancedTelemetry;
	AnnotateSource;
	// Analyzer;
	As3;
	CheckXmlProxy;
	CoreApi;
	CoreApiSerialize;
	Cppia;
	NoCppiaAst;
	Dce;
	DceDebug;
	Debug;
	Display;
	DisplayStdin;
	DllExport;
	DllImport;
	DocGen;
	Dump;
	DumpDependencies;
	DumpIgnoreVarIds;
	DynamicInterfaceClosures;
	EraseGenerics;
	EvalDebugger;
	EvalStack;
	EvalTimes;
	FastCast;
	Fdb;
	FileExtension;
	FlashStrict;
	FlashUseStage;
	ForceLibCheck;
	ForceNativeProperty;
	FormatWarning;
	GencommonDebug;
	Haxe3Compat;
	HaxeBoot;
	HaxeVer;
	HxcppApiLevel;
	HxcppGcGenerational;
	HxcppDebugger;
	IncludePrefix;
	Interp;
	JavaVer;
	JqueryVer;
	JsClassic;
	JsEs;
	JsUnflatten;
	JsSourceMap;
	JsEnumsAsObjects;
	SourceMap;
	KeepOldOutput;
	LoopUnrollMaxCost;
	LuaVer;
	LuaJit;
	Macro;
	MacroDebug;
	MacroTimes;
	NekoSource;
	NekoV1;
	NetworkSandbox;
	NetVer;
	NetTarget;
	NoCompilation;
	NoCOpt;
	NoDeprecationWarnings;
	NoFlashOverride;
	NoDebug;
	NoInline;
	NoOpt;
	NoRoot;
	NoSwfCompress;
	NoTraces;
	Objc;
	OldConstructorInline;
	OldErrorFormat;
	PhpLib;
	PhpFront;
	PhpPrefix;
	PythonVersion;
	RealPosition;
	ReplaceFiles;
	Scriptable;
	ShallowExpose;
	SourceHeader;
	SourceMapContent;
	Static;
	Swc;
	SwfCompressLevel;
	SwfDebugPassword;
	SwfDirectBlit;
	SwfGpu;
	SwfMetadata;
	SwfPreloaderFrame;
	SwfProtected;
	SwfScriptTimeout;
	SwfUseDoAbc;
	Sys;
	Unsafe;
	UseNekoc;
	UseRttiDoc;
	Vcproj;
	WarnVarShadowing;
	NoMacroCache;
	Last; // must be last
}

enum DefineParameter {
	HasParam (s:String);
	Platform (p:core.Globals.Platform);
	Platforms (l:ImmutableList<core.Globals.Platform>);
}

@:structInit
class Define {
	public var values : Map<String,String>;
	public var defines_signature : haxe.ds.Option<String>;

	// public function new (values:Map<String,String>, defines_signature:haxe.ds.Option<String>) {
	// 	this.values = values;
	// 	this.defines_signature = defines_signature;
	// }


	public static function infos (k:StrictDefined) : {a:String, b:{a:String, b:ImmutableList<DefineParameter>}} {
		return switch (k) {
			case AbsolutePath:
				{a:"absolute_path",
				 b:{a:"Print absolute file path in trace output", b:[]}
				};
			case AdvancedTelemetry:
				{a:"advanced-telemetry",
				 b:{a:"Allow the SWF to be measured with Monocle tool", b:[Platform(Flash)]}
				};
			case AnnotateSource:
				{a:"annotate_source",
				 b:{a:"Add additional comments to generated source code", b:[Platform(Cpp)]}
				};
			// | Analyzer -> "analyzer",("Use static analyzer for optimization (experimental)") *)
			case As3:
				{a:"as3",
				 b:{a:"Defined when outputting flash9 as3 source code", b:[]}
				};
			case CheckXmlProxy:
				{a:"check_xml_proxy",
				 b:{a:"Check the used fields of the xml proxy", b:[]}
				};
			case CoreApi:
				{a:"core_api",
				 b:{a:"Defined in the core api context", b:[]}
				};
			case CoreApiSerialize:
				{a:"core_api_serialize",
				 b:{a:"Mark some generated core api classes with the Serializable attribute on C#",b:[Platform(Cs)]}
				};
			case Cppia:
				{a:"cppia",b:{a:"Generate cpp instruction assembly",b:[]}};
			case NoCppiaAst:
				{a:"nocppiaast", b:{a:"Use legacy cppia generation",b:[]}};
			case Dce:
				{a:"dce",
				 b:{a:"<mode:std|full|no> Set the dead code elimination mode (default std)",b:[]}
				};
			case DceDebug:
				{a:"dce_debug",b:{a:"Show DCE log",b:[]}};
			case Debug:
				{a:"debug",b:{a:"Activated when compiling with -debug",b:[]}};
			case Display:
				{a:"display",b:{a:"Activated during completion",b:[]}};
			case DisplayStdin:
				{a:"display_stdin",
				 b:{a:"Read the contents of a file specified in --display from standard input",b:[]}
				};
			case DllExport:
				{a:"dll_export",
				 b:{a:"GenCPP experimental linking",b:[Platform(Cpp)]}
				};
			case DllImport:
				{a:"dll_import",
				 b:{a:"Handle Haxe-generated .NET dll imports",b:[Platform(Cs)]}
				};
			case DocGen:
				{a:"doc_gen",
				 b:{a:"Do not perform any removal/change in order to correctly generate documentation",b:[]}
				};
			case Dump:
				{a:"dump",
				b:{a:"<mode:pretty|record|legacy> Dump typed AST in dump subdirectory using specified mode or non-prettified default",b:[]}
				};
			case DumpDependencies:
				{a:"dump_dependencies",
				 b:{a:"Dump the classes dependencies in a dump subdirectory",b:[]}
				};
			case DumpIgnoreVarIds:
				{a:"dump_ignore_var_ids",
				 b:{a:"Remove variable IDs from non-pretty dumps (helps with diff)",b:[]}
				};
			case DynamicInterfaceClosures:
				{a:"dynamic_interface_closures",
				 b:{a:"Use slow path for interface closures to save space", b:[Platform(Cpp)]}
				};
			case EraseGenerics:
				{a:"erase_generics",
				 b:{a:"Erase generic classes on C#", b:[Platform(Cs)]}
				};
			case EvalDebugger:
				{a:"eval_debugger",
				 b:{a:"Support debugger in macro/interp mode. Allows host:port value to open a socket. Implies eval_stack.", b:[]}
				};
			case EvalStack:
				{a:"eval_stack",
				 b:{a:"Record stack information in macro/interp mode",b:[]}
				};
			case EvalTimes:
				{a:"eval_times",
				 b:{a:"Record per-method execution times in macro/interp mode. Implies eval_stack.",b:[]}
				};
			case FastCast:
				{a:"fast_cast",
				 b:{a:"Enables an experimental casts cleanup on C# and Java",b:[Platforms([Cs,Java])]}
				};
			case Fdb:
				{a:"fdb",
				 b:{a:"Enable full flash debug infos for FDB interactive debugging",b:[Platform(Flash)]}
				};
			case FileExtension:
				{a:"file_extension",
				 b:{a:"Output filename extension for cpp source code", b:[Platform(Cpp)]}
				};
			case FlashStrict:
				{a:"flash_strict",
				 b:{a:"More strict typing for flash target", b:[Platform(Flash)]}
				};
			case FlashUseStage:
				{a:"flash_use_stage",
				 b:{a:"Keep the SWF library initial stage", b:[Platform(Flash)]}
				};
			// force_lib_check is only here as a debug facility - compiler checking allows errors to be found more easily
			case ForceLibCheck:
				{a:"force_lib_check",
				 b:{a:"Force the compiler to check -net-lib and -java-lib added classes (internal)",b:[Platforms([Cs,Java])]}
				};
			case ForceNativeProperty:
				{a:"force_native_property",
				 b:{a:"Tag all properties with :nativeProperty metadata for 3.1 compatibility", b:[Platform(Cpp)]}
				};
			case FormatWarning:
				{a:"format_warning",
				 b:{a:"Print a warning for each formatted string, for 2.x compatibility", b:[]}
				};
			case GencommonDebug:
				{a:"gencommon_debug",
				 b:{a:"GenCommon internal", b:[Platforms([Cs,Java])]}
				};
			case Haxe3Compat:
				{a:"haxe3compat",
				 b:{a:"Gives warnings about transition from Haxe 3.x to Haxe 4.0", b:[]}
				};
			case HaxeBoot:
				{a:"haxe_boot",
				 b:{a:"Given the name 'haxe' to the flash boot class instead of a generated name", b:[Platform(Flash)]}
				};
			case HaxeVer:
				{a:"haxe_ver", b:{a:"The current Haxe version value",b:[]}};
			case HxcppApiLevel:
				{a:"hxcpp_api_level",
				 b:{a:"Provided to allow compatibility between hxcpp versions",b:[Platform(Cpp)]}
				};
			case HxcppGcGenerational:
				{a:"HXCPP_GC_GENERATIONAL",b:{a:"Experimental Garbage Collector", b:[Platform(Cpp)]}};
			case HxcppDebugger:
				{a:"HXCPP_DEBUGGER",
				 b:{a:"Include additional information for HXCPP_DEBUGGER", b:[Platform(Cpp)]}
				};
			case IncludePrefix:
				{a:"include_prefix",
				 b:{a:"prepend path to generated include files", b:[Platform(Cpp)]}
				};
			case Interp:
				{a:"interp", b:{a:"The code is compiled to be run with --interp", b:[]}};
			case JavaVer:
				{a:"java_ver",
				 b:{a:"<version:5-7> Sets the Java version to be targeted", b:[Platform(Java)]}
				};
			case JqueryVer:
				{a:"jquery_ver",
				 b:{a:"The jQuery version supported by js.jquery.*. The version is encoded as an integer. e.g. 1.11.3 is encoded as 11103", b:[Platform(Js)]}
				};
			case JsClassic:
				{a:"js_classic",
				 b:{a:"Don't use a function wrapper and strict mode in JS output", b:[Platform(Js)]}
				};
			case JsEs:
				{a:"js_es",
				 b:{a:"Generate JS compliant with given ES standard version (default 5)", b:[Platform(Js), HasParam("version number")]}
				};
			case JsEnumsAsObjects:
				{a:"js_enums_as_objects",
				 b:{a:"Generate enum representation as object instead of as array", b:[Platform(Js)]}
				};
			case JsUnflatten:
				{a:"js_unflatten",
				 b:{a:"Generate nested objects for packages and types", b:[Platform(Js)]}
				};
			case JsSourceMap:
				{a:"js_source_map",
				 b:{a:"Generate JavaScript source map even in non-debug mode", b:[Platform(Js)]}
				};
			case SourceMap:
				{a:"source_map",
				 b:{a:"Generate source map for compiled files (Currently supported for php only)", b:[Platform(Php)]}
				};
			case KeepOldOutput:
				{a:"keep_old_output",
				 b:{a:"Keep old source files in the output directory (for C#/Java)", b:[Platforms([Cs,Java])]}
				};
			case LoopUnrollMaxCost:
				{a:"loop_unroll_max_cost",
				 b:{a:"Maximum cost (number of expressions * iterations) before loop unrolling is canceled (default 250)", b:[]}
				};
			case LuaJit:
				{a:"lua_jit",
				 b:{a:"Enable the jit compiler for lua (version 5.2 only)", b:[Platform(Lua)]}
				};
			case LuaVer:
				{a:"lua_ver", b:{a:"The lua version to target", b:[Platform(Lua)]}};
			case Macro:
				{a:"macro", b:{a:"Defined when code is compiled in the macro context", b:[]}};
			case MacroDebug:
				{a:"macro_debug", b:{a:"Show warnings for potential macro problems (e.g. macro-in-macro calls)", b:[]}};
			case MacroTimes:
				{a:"macro_times", b:{a:"Display per-macro timing when used with --times", b:[]}};
			case NetVer:
				{a:"net_ver", b:{a:"<version:20-45> Sets the .NET version to be targeted", b:[Platform(Cs)]}};
			case NetTarget:
				{a:"net_target", b:{a:"<name> Sets the .NET target. Defaults to \"net\". xbox, micro (Micro Framework), compact (Compact Framework) are some valid values", b:[Platform(Cs)]}};
			case NekoSource:
				{a:"neko_source", b:{a:"Output neko source instead of bytecode", b:[Platform(Neko)]}};
			case NekoV1:
				{a:"neko_v1", b:{a:"Keep Neko 1.x compatibility", b:[Platform(Neko)]}};
			case NetworkSandbox:
				{a:"network-sandbox", b:{a:"Use local network sandbox instead of local file access one", b:[Platform(Flash)]}};
			case NoCompilation:
				{a:"no-compilation", b:{a:"Disable final compilation", b:[Platforms([Cs,Java,Cpp,Hl])]}};
			case NoCOpt:
				{a:"no_copt", b:{a:"Disable completion optimization (for debug purposes)", b:[]}};
			case NoDebug:
				{a:"no_debug", b:{a:"Remove all debug macros from cpp output", b:[]}};
			case NoDeprecationWarnings:
				{a:"no-deprecation-warnings", b:{a:"Do not warn if fields annotated with @:deprecated are used", b:[]}};
			case NoFlashOverride:
				{a:"no-flash-override", b:{a:"Change overrides on some basic classes into HX suffixed methods, flash only", b:[Platform(Flash)]}};
			case NoOpt:
				{a:"no_opt", b:{a:"Disable optimizations", b:[]}};
			case NoInline:
				{a:"no_inline", b:{a:"Disable inlining", b:[]}};
			case NoRoot:
				{a:"no_root", b:{a:"Generate top-level types into haxe.root namespace", b:[Platform(Cs)]}};
			case NoMacroCache:
				{a:"no_macro_cache", b:{a:"Disable macro context caching", b:[]}};
			case NoSwfCompress:
				{a:"no_swf_compress", b:{a:"Disable SWF output compression", b:[Platform(Flash)]}};
			case NoTraces:
				{a:"no_traces", b:{a:"Disable all trace calls", b:[]}};
			case Objc:
				{a:"objc", b:{a:"Sets the hxcpp output to objective-c++ classes. Must be defined for interop", b:[Platform(Cpp)]}};
			case OldConstructorInline:
				{a:"old-constructor-inline", b:{a:"Use old constructor inlining logic (from haxe 3.4.2) instead of the reworked version.", b:[]}};
			case OldErrorFormat:
				{a:"old-error-format", b:{a:"Use Haxe 3.x zero-based column error messages instead of new one-based format.", b:[]}};
			case PhpPrefix:
				{a:"php_prefix", b:{a:"Root namespace for generated php classes. E.g. if compiled with`-D php-prefix=some.sub`, then all classes will be generated in `\\some\\sub` namespace.", b:[Platform(Php)]}};
			case PhpLib:
				{a:"php_lib", b:{a:"Select the name for the php lib folder.", b:[Platform(Php)]}};
			case PhpFront:
				{a:"php_front", b:{a:"Select the name for the php front file (by default: `index.php`).", b:[Platform(Php)]}};
			case PythonVersion:
				{a:"python_version", b:{a:"The python version to target (default 3.3)", b:[Platform(Python)]}};
			case RealPosition:
				{a:"real_position", b:{a:"Disables Haxe source mapping when targetting C#, removes position comments in Java and Php output", b:[Platforms([Cs,Java,Php])]}};
			case ReplaceFiles:
				{a:"replace_files", b:{a:"GenCommon internal", b:[Platforms([Java,Cs])]}};
			case Scriptable:
				{a:"scriptable", b:{a:"GenCPP internal", b:[Platform(Cpp)]}};
			case ShallowExpose:
				{a:"shallow-expose", b:{a:"Expose types to surrounding scope of Haxe generated closure without writing to window object", b:[Platform(Js)]}};
			case SourceHeader:
				{a:"source-header", b:{a:"Print value as comment on top of generated files, use '' value to disable", b:[]}};
			case SourceMapContent:
				{a:"source-map-content", b:{a:"Include the hx sources as part of the JS source map", b:[Platform(Js)]}};
			case Static:
				{a:"static", b:{a:"Defined if the current target is static", b:[]}};
			case Swc:
				{a:"swc", b:{a:"Output a SWC instead of a SWF", b:[Platform(Flash)]}};
			case SwfCompressLevel:
				{a:"swf_compress_level", b:{a:"<level:1-9> Set the amount of compression for the SWF output", b:[Platform(Flash)]}};
			case SwfDebugPassword:
				{a:"swf_debug_password", b:{a:"Set a password for debugging", b:[Platform(Flash)]}};
			case SwfDirectBlit:
				{a:"swf_direct_blit", b:{a:"Use hardware acceleration to blit graphics", b:[Platform(Flash)]}};
			case SwfGpu:
				{a:"swf_gpu", b:{a:"Use GPU compositing features when drawing graphics", b:[Platform(Flash)]}};
			case SwfMetadata:
				{a:"swf_metadata", b:{a:"<file> Include contents of <file> as metadata in the swf", b:[Platform(Flash)]}};
			case SwfPreloaderFrame:
				{a:"swf_preloader_frame", b:{a:"Insert empty first frame in swf", b:[Platform(Flash)]}};
			case SwfProtected:
				{a:"swf_protected", b:{a:"Compile Haxe private as protected in the SWF instead of public", b:[Platform(Flash)]}};
			case SwfScriptTimeout:
				{a:"swf_script_timeout", b:{a:"Maximum ActionScript processing time before script stuck dialog box displays (in seconds)", b:[Platform(Flash)]}};
			case SwfUseDoAbc:
				{a:"swf_use_doabc", b:{a:"Use DoAbc swf-tag instead of DoAbcDefine", b:[Platform(Flash)]}};
			case Sys:
				{a:"sys", b:{a:"Defined for all system platforms", b:[]}};
			case Unsafe:
				{a:"unsafe", b:{a:"Allow unsafe code when targeting C#", b:[Platform(Cs)]}};
			case UseNekoc:
				{a:"use_nekoc", b:{a:"Use nekoc compiler instead of internal one", b:[Platform(Neko)]}};
			case UseRttiDoc:
				{a:"use_rtti_doc", b:{a:"Allows access to documentation during compilation", b:[]}};
			case Vcproj:
				{a:"vcproj", b:{a:"GenCPP internal", b:[Platform(Cpp)]}};
			case WarnVarShadowing:
				{a:"warn_var_shadowing", b:{a:"Warn about shadowing variable declarations", b:[]}};
			case Last:
				trace("Shall not be seen"); throw false;
		}
	}

	public static function get_documentation_list () : {a:ImmutableList<{a:String,b:String}>, b:Int} {
		var m = 0;
		var acc = [];

		for (d in std.Type.allEnums(StrictDefined)) {
			var i = infos(d);
			var t = i.a;
			var doc = i.b.a;
			var flags = i.b.b;
			var pfs:ImmutableList<core.Globals.Platform> = [];
			List.iter(function (f) {
				switch (f) {
					case HasParam(s): // TODO
					case Platform (p): pfs = p :: pfs;
					case Platforms(pl): pfs = List.append(pl,pfs);
				}
				var pfs_string = core.Globals.platform_list_help(List.rev(pfs));
				if (t.length > m) {
					m = t.length;
				}
				acc.push({a:t.split("_").join("-"), b: doc+pfs_string});
			}, flags);
		}

		haxe.ds.ArraySort.sort(acc, function (x:{a:String, b:String}, y:{a:String, b:String}) {
			if (x.a == y.a) { return 0;}
			if (x.a > y.a) { return 1;}
			return -1;
		});
		return {a:acc, b:m};
	}

	public static function raw_defined (ctx:Define, v:String) : Bool {
		return PMap.mem(v, ctx.values);
	}
	public static function defined (ctx:Define, v:StrictDefined) : Bool {
		return raw_defined(ctx, infos(v).a);
	}

	public static function raw_defined_value (ctx:Define, k:String) : String {
		return PMap.find(k, ctx.values);
	}

	public static function defined_value (ctx:Define, v:StrictDefined) {
		return raw_defined_value(ctx, infos(v).a);
	}

	public static function raw_define (ctx:Define, v:String) {
		var i = v.indexOf("=");

		var k:String;
		if (i == -1) {
			k = v;
			v = "1";
		}
		else {
			k = v.substr(0, i);
			v = v.substr(i+1);
		}

		var k = k.split("-").join("_");
		ctx.values = PMap.add(k, v, ctx.values);
		ctx.defines_signature = None;
	}

	public static function define_value (ctx:Define, k:StrictDefined, v:String) {
		raw_define(ctx, infos(k).a + "=" + v);
	}

	public static function define (ctx:Define, v:StrictDefined) {
		raw_define(ctx, infos(v).a);
	}

	public static function get_signature (def:core.Define) : String {
		return switch (def.defines_signature) {
			case Some(s) : s;
			case None:
				function f(k:String, v:String, acc:ImmutableList<String>) : ImmutableList<String> {
					// don't make much difference between these special compilation flags
					return switch (k.split("-").join("_")) {
						// If we add something here that might be used in conditional compilation it should be added to
						// Parser.parse_macro_ident as well (issue #5682).
						case "display", "use_rtti_doc", "macro_times", "display_details", "no_copt", "display_stdin": acc;
						case _:
							(k + "=" + v) :: acc;
					}
				}
				var defines = PMap.foldi(f, def.values, []);
				var str = List.join("@", List.sort(function (a, b) {return Reflect.compare(a,b);}, defines));
				var s = haxe.crypto.Md5.encode(str);
				def.defines_signature = Some(s);
				s;
		}
	}
}
