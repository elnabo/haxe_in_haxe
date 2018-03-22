package compiler;

import generators.*;

import ocaml.Arg;
import ocaml.Hashtbl;
import ocaml.List;
import ocaml.PMap;
import ocaml.Ref;
// import context.Common;
import core.Define;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
import sys.io.Process;

using equals.Equal;
// using ocaml.List;

using StringTools;

enum ArgType {
	Float(s:String);
	Unit; // none
	String(s:String);
}

class Abort {
	public static final instance = new Abort();
	public function new () {};
}

class Failure {
	public var s:String;
	public function new (s:String) {
		this.s = s;
	}
}

class Initialize {
	public static function set_platform (com:context.Common.Context, pf:core.Globals.Platform, file:String) {
		if (com.platform != Cross) {
			throw new ocaml.Failure("Multiple targets");
		}
		context.Common.init_platform(com,pf);
		com.file = file;
		if (pf == Flash && haxe.io.Path.extension(file) == "swc") {
			context.Common.define(com,Swc);
		}
	}

	public static function initialize_target (ctx:Server.Context, com:context.Common.Context, classes:ImmutableList<core.Path>) : String {
		function add_std(dir:String) : Void {
			var p1 = List.filter(function(s:String) { return !List.mem(s,com.std_path); }, com.class_path);
			var p2 = List.map(function (p) { return p + dir + "/_std/"; }, com.std_path);
			com.class_path = List.append(p1, List.append(p2,com.std_path));
		}
		return switch (com.platform) {
			case Cross:
				// no platform selected
				set_platform(com, Cross, "");
				"?";
			case Flash:
				function loop (l:ImmutableList<{a:Float, b:String}>) {
					switch (l) {
						case []:
						case {a:v}::_ if (v > com.flash_version):
						case {a:v, b:def}::l:
							var l:ImmutableList<{a:Float, b:String}> = l;
							context.Common.raw_define(com, "flash"+def);
							loop(l);
					}
				}
				loop(context.Common.flash_versions);
				context.Common.raw_define(com, "flash");
				com.package_rules = PMap.remove("flash", com.package_rules);
				add_std("flash");
				"swf";
			case Neko:
				add_std("neko");
				"n";
			case Js:
				if (! PMap.exists(core.Define.infos(JqueryVer).a, com.defines.values)) {
					context.Common.define_value(com, JqueryVer, "11204");
				}
				var es_version:Int = try {
					var es_version_value = Std.parseInt(context.Common.defined_value(com, JsEs));
					if (es_version_value != null) {
						es_version_value;
					}
					else {
						0;
					}
				}
				catch (e:ocaml.Not_found) {
					context.Common.define_value(com, JsEs, "5");
					5;
				}
				if (es_version < 3 || es_version == 4) {
					// we don't support ancient and there's no 4th
					throw new ocaml.Failure("Invalid -D js-es value");
				}

				if (es_version >= 5) {
					context.Common.raw_define(com, "js-es5"); // backward-compatibility
				}

				add_std("js");
				"js";
			case Lua:
				add_std("lua");
				"lua";
			case Php:
				add_std("php");
				"php";
			case Cpp:
				context.Common.define_value(com, HxcppApiLevel, "332");
				add_std("cpp");
				if (context.Common.defined(com, Cppia)) {
					classes = core.Path.parse_path("cpp.cppia.HostClasses") :: classes;
				}
				"cpp";
			case Cs:
				var old_flush = ctx.flush;
				ctx.flush = function () {
					com.net_libs = [];
					old_flush();
				};
				trace("TODO finish initialize_target for CS");
				// Dotnet.before_generate com;
				add_std("cs");
				"cs";
			case Java:
				var old_flush = ctx.flush;
				ctx.flush = function () {
					// List.iter (fun (_,_,close,_,_) -> close()) com.java_libs;
					com.java_libs = [];
					old_flush();
				};
				trace("TODO finish initialize_target for Java");
				// Java.before_generate com;
				add_std("java");
				"java";
			case Python:
				add_std("python");
				if (!context.Common.defined(com, PythonVersion)) {
					context.Common.define_value(com, PythonVersion, "3.3");
				}
				"python";
			case Hl:
				add_std("hl");
				"hl";
			case Eval:
				add_std("eval");
				"eval";
		};
	}
}

class Main {

	public static var executable_path : String = std.Sys.programPath();

	public static var reserved_flags = [
	"cross","js","lua","neko","flash","php","cpp","cs","java","python",
	"as3","swc","macro","sys","static"
	];

	public static function message (ctx:compiler.Server.Context,  msg:context.Common.CompilerMessage) {
		ctx.messages = msg :: ctx.messages;
	}

	public static var deprecated : ImmutableList<{fst:String, snd:String}>= [];

	public static function limit_string (s:String, offset:Int) : String {
		var rest = 80 - offset;
		var words = s.split(" ");

		var res : Array<String> = [];
		var i = 0;
		for (word in words) {
			if (word.length + i + 1 > rest) {
				var str = "\n" + "".rpad(" ", offset);
				res.push(str);
				res.push(word);
				i = word.length;
			}
			else {
				res.push((i == 0) ? "" : " ");
				res.push(word);
				i = i + 1 + word.length;
			}
		}
		return res.join("");
	}

	public static function error(ctx:Server.Context, msg:String, p:core.Globals.Pos) {
		var msg =try {
			List.assoc(msg, deprecated);
		}
		catch (_:ocaml.Not_found) { msg; }
		message(ctx, CMError(msg,p));
		ctx.has_error = true;
	}

	public static function delete_file(f:String) {
		trace("debug: deleting file "+f);
		// try {
		// 	sys.FileSystem.deleteFile(f);
		// }
		// catch (_:Dynamic) {}
	}

	public static function expand_env (?h:Option<Map<String, String>>=null, path:String) : String {
		if (h == null) { h = None; }
		// let r = Str.regexp "%\\([A-Za-z0-9_]+\\)%" in
		var r = new EReg("%([A-Za-z0-9_]+)%","");
		return r.map(path, function(e) {
			var key = e.matched(1);
			var env = std.Sys.getEnv(key);
			if (env == null) {
				switch (h) {
					case Some(map) : env = map.get(key);
					case None:
				}
				if (env == null) {
					env = "%" + key + "%";
				}
			}
			return env;
		});
		// let r = Str.regexp "%\\([A-Za-z0-9_]+\\)%" in
		// Str.global_substitute r (fun s ->
		// 	let key = Str.matched_group 1 s in
		// 	try
		// 		Sys.getenv key
		// 	with Not_found -> try
		// 		match h with
		// 		| None -> raise Not_found
		// 		| Some h -> Hashtbl.find h key
		// 	with Not_found ->
		// 		"%" ^ key ^ "%"
		// ) path
	}

	public static function add_libs (com:context.Common.Context, libs:ImmutableList<String>) : ImmutableList<String> {
		return [];
		// let call_haxelib() =
		// 	let t = Timer.timer ["haxelib"] in
		// 	let cmd = "haxelib path " ^ String.concat " " libs in
		// 	let pin, pout, perr = Unix.open_process_full cmd (Unix.environment()) in
		// 	let lines = Std.input_list pin in
		// 	let err = Std.input_list perr in
		// 	let ret = Unix.close_process_full (pin,pout,perr) in
		// 	if ret <> Unix.WEXITED 0 then failwith (match lines, err with
		// 		| [], [] -> "Failed to call haxelib (command not found ?)"
		// 		| [], [s] when ExtString.String.ends_with (ExtString.String.strip s) "Module not found : path" -> "The haxelib command has been strip'ed, please install it again"
		// 		| _ -> String.concat "\n" (lines@err));
		// 	t();
		// 	lines
		// in
		// match libs with
		// | [] -> []
		// | _ ->
		// 	let lines = match CompilationServer.get() with
		// 		| Some cs ->
		// 			(try
		// 				(* if we are compiling, really call haxelib since library path might have changed *)
		// 				if not com.display.dms_display then raise Not_found;
		// 				CompilationServer.find_haxelib cs libs
		// 			with Not_found ->
		// 				let lines = call_haxelib() in
		// 				CompilationServer.cache_haxelib cs libs lines;
		// 				lines)
		// 		| _ -> call_haxelib()
		// 	in
		// 	let extra_args = ref [] in
		// 	let lines = List.fold_left (fun acc l ->
		// 		let l = ExtString.String.strip l in
		// 		if l = "" then acc else
		// 		if l.[0] <> '-' then l :: acc else
		// 		match (try ExtString.String.split l " " with _ -> l, "") with
		// 		| ("-L",dir) ->
		// 			com.neko_libs <- String.sub l 3 (String.length l - 3) :: com.neko_libs;
		// 			acc
		// 		| param, value ->
		// 			extra_args := param :: !extra_args;
		// 			if value <> "" then extra_args := value :: !extra_args;
		// 			acc
		// 	) [] lines in
		// 	com.class_path <- lines @ com.class_path;
		// 	List.rev !extra_args
	}

	public static function run_command (ctx:Server.Context, cmd:String) : Int {
		var h = new Map<String, String>();
		h.set("__file__", ctx.com.file);
		h.set("__platform__", core.Globals.platform_name(ctx.com.platform));
		var t = core.Timer.timer(["command"]);
		cmd = expand_env(Some(h), cmd);
		var len = cmd.length;
		if (len > 3 && cmd.substr(0,3) == "cd ") {
			std.Sys.setCwd(cmd.substr(3));
			return 0;
		}
		else {
			var binary_string = function (s:String) {
				return (core.Globals.is_windows) ? s.split("\r\n").join("\n") : s;
			};

			// var env = std.Sys.environment();
			// var p = new Process(cmd, [for (key in env.keys()) key+"="+env.get(key)]);
			var p = new Process(cmd);

			var sout = binary_string(p.stdout.readAll().toString());
			var serr = binary_string(p.stderr.readAll().toString());
			var result = p.exitCode(true);
			p.close();

			if (serr != "") {
				var m = context.Common.CompilerMessage.CMError(
					(serr.charAt(serr.length - 1) == "\n")
						? serr.substr(0, serr.length - 1)
						: serr, core.Globals.null_pos
				);
				ctx.messages = m :: ctx.messages;
			}
			if (sout != "") {
				ctx.com.print(sout + "\n");
			}

			t();
			return result;
		}
	}

	public static function generate (tctx:context.Typecore.Typer, ext:String, xml_out:Option<String>, interp:Bool, swf_header:Option<{width:Int, height:Int, fps:Float, color:Int}>) : Void {
		var com = tctx.com;
		/* check file extension. In case of wrong commandline, we don't want
		to accidentaly delete a source file. */
		if (context.Common.file_extension(com.file) == ext) {
			delete_file(com.file);
		}

		if (com.platform == Flash || com.platform == Cpp || com.platform == Hl) {
			List.iter(codegen.Codegen.fix_overrides.bind(com), com.types);
		}

		if (context.Common.defined(com, Dump)) {
			codegen.Codegen.Dump.dump_types(com);
		}
		if (context.Common.defined(com, DumpDependencies)) {
			codegen.Codegen.Dump.dump_dependencies(com);
			if (!tctx.in_macro) {
				switch (tctx.g.macros) {
					case None:
					case Some({t:ctx}):
						codegen.Codegen.Dump.dump_dependencies(Some("macro"), ctx.com);
				}
			}
		}
		switch (com.platform) {
			case Neko, Hl, Eval if (interp):
			case Cpp if (context.Common.defined(com, Cppia)):
			case Cpp, Cs, Java, Php: core.Path.mkdir_from_path(com.file+"/.");
			case _: core.Path.mkdir_from_path(com.file);
		}

		if (interp) {
			var t = core.Timer.timer(["interp"]);
			try {
				typing.MacroContext.interpret(tctx);
				t();
			}
			catch (e:Any) {
				t();
				throw e;
			}
		}
		else if (com.platform == Cross) {}
		else {
			var _tmp = switch (com.platform) {
				case Flash if (context.Common.defined(com, As3)):
					{generate:Genas3.generate, name:"AS3"};
				case Flash:
					{generate:Genswf.generate.bind(swf_header), name:"swf"};
				case Neko:
					{generate:Genneko.generate, name:"neko"};
				case Js:
					{generate:Genjs.generate, name:"js"};
				case Lua:
					{generate:Genlua.generate, name:"lua"};
				case Php:
					{generate:Genphp7.generate, name:"php"};
				case Cpp:
					{generate:Gencpp.generate, name:"cpp"};
				case Cs:
					{generate:Gencs.generate, name:"cs"};
				case Java:
					{generate:Genjava.generate, name:"java"};
				case Python:
					{generate:Genpy.generate, name:"python"};
				case Hl:
					{generate:Genhl.generate, name:"hl"};
				case Eval:
					{generate:function (_) { typing.MacroContext.interpret(tctx); }, name:"eval"};
				case Cross:
					trace("Shall not be seen"); std.Sys.exit(255); throw false;
			}
			var generate = _tmp.generate; var name = _tmp.name;
			context.Common.log(com, "Generating "+name+": "+com.file);
			var t = core.Timer.timer(["generate", name]);
			generate(com);
			t();
		}
	}

	public static function get_std_class_paths () : ImmutableList<String> {
		var p = std.Sys.getEnv("HAXE_STD_PATH");
		if (p != null) {
			var parts = ~/[;:]/.split(p);
			var looped : Array<String> = [];
			while (parts.length > 0) {
				if (parts.length > 1) {
					var drive = parts.shift();
					var c = (drive.length == 1) ? drive.charCodeAt(0) : -1;
					if (drive.length == 1 && ((c >= "a".code && c <= "z".code) || (c >= "A".code && c <= "Z".code))) {
						var path = parts.shift();
						looped.push(drive + ":" + path);
					}
					else {
						looped.push(drive);
					}
				}
				else {
					looped.push(parts.shift());
				}
			}
			return [for (l in looped) core.Path.add_trailing_slash(l)];
		}
		else {
			var base_path = core.Path.get_real_path()(executable_path);
			if (std.Sys.systemName() != "Windows") { // if Sys.os_type = "Unix" then
				var prefix_path = haxe.io.Path.directory(base_path);
				var lib_path = haxe.io.Path.join([prefix_path, "lib"]);
				var share_path = haxe.io.Path.join([prefix_path, "share"]);
				return [
					core.Path.add_trailing_slash(haxe.io.Path.join([lib_path, "haxe/std"])),
					core.Path.add_trailing_slash(haxe.io.Path.join([lib_path, "haxe/extraLibs"])),
					core.Path.add_trailing_slash(haxe.io.Path.join([share_path, "haxe/std"])),
					core.Path.add_trailing_slash(haxe.io.Path.join([share_path, "haxe/extraLibs"])),
					core.Path.add_trailing_slash(haxe.io.Path.join([base_path, "std"])),
					core.Path.add_trailing_slash(haxe.io.Path.join([base_path, "extraLibs"])),
				];
			}
			else {
				return [
					core.Path.add_trailing_slash(haxe.io.Path.join([base_path, "std"])),
					core.Path.add_trailing_slash(haxe.io.Path.join([base_path, "extraLibs"]))
				];
			}
		}
	}

	public static function process_params(create:ImmutableList<String>->compiler.Server.Context, pl:ImmutableList<String>) {
		var each_params = new Ref<ImmutableList<String>>([]);
		function loop (acc:ImmutableList<String>, l:ImmutableList<String>) : Void {
			switch (l) {
				case []:
					var ctx = create(List.append(each_params.get(), List.rev(acc)));
					init(ctx);
					ctx.flush();
				case "--next"::l if (acc == Tl): // skip empty --next
					loop([], l);
				case "--next"::l:
					var ctx = create(List.append(each_params.get(), List.rev(acc)));
					ctx.has_next = true;
					init(ctx);
					ctx.flush();
					loop([], l);
				case "--each"::l:
					each_params.set(List.rev(acc));
					loop([], l);
				case "--cwd"::(dir::l):
					// we need to change it immediately since it will affect hxml loading
					try {
						std.Sys.setCwd(dir);
					}
					catch (_:Dynamic) {
						throw new ocaml.Arg.Bad("Invalid directory: " + dir);
					}
					loop(acc, l);
				case "--connect"::(hp::l):
					switch (context.common.CompilationServer.get()) {
						case None:
							var host : String;
							var port : Int;

							try {
								var split = hp.indexOf(":");
								if (split >= 0) {
									host = hp.substr(0, split);
									port = Std.parseInt(hp.substr(split + 1));
								}
								else {
									host = "127.0.0.1";
									port = Std.parseInt(hp);
								}
							}
							catch (_:Dynamic) {
								throw new ocaml.Arg.Bad("Invalid port");
							}
							compiler.Server.do_connect(host, port, List.append(acc, List.rev(l)));
						case Some(_):
							// already connected : skip
							loop(acc, l);
					}
				case "--run"::(cl::args):
					var acc = List.append([cl, "--main", "--interp"], acc);
					var ctx = create(List.append(each_params.get(), List.rev(acc)));
					ctx.com.sys_args = args;
					init(ctx);
					ctx.flush();
				case arg::l:
					var splited:ImmutableList<String> = arg.split(".");
					switch (List.rev(splited)) {
						case "hxml"::_ if (switch (acc) { case "-cmd"::_: false; case _: true;}):
							var acc = acc;
							var l:ImmutableList<String> = l;
							try {
								l = List.append(compiler.Server.parse_hxml(arg), l);
							}
							catch (_:ocaml.Not_found) {
								acc = (arg + " (file not found)")::acc;
							}
							loop(acc, l);
						case _: loop(arg::acc, l);
					}

			}
		}
		// put --display in front if it was last parameter
		var pl = switch (List.rev(pl)) {
			case file :: ("--display" :: pl) if (file != "memory"):
				"--display" :: (file :: List.rev(pl));
			case "use_rtti_doc" :: ("-D" :: (file :: ("--display" :: pl))):
				 "--display" :: (file :: List.rev(pl));
			case _: pl;
		}
		loop([], pl);
	}

	public static function init (ctx:compiler.Server.Context) {
		var end = (std.Sys.systemName() == "Windows") ? ".exe" : "";
		var usage = 'Haxe Compiler ${Server.s_version()} - (C)2005-2018 Haxe Foundation\n Usage : haxe${end} -main <class> [-swf|-js|-neko|-php|-cpp|-cppia|-as3|-cs|-java|-python|-hl|-lua] <output> [options]\n Options :';
		var com = ctx.com;
		var classes:ImmutableList<core.Path> = [new core.Path([],"Std")];
		try {
			var xml_out = None;
			var swf_header = None;
			var cmds = [];
			var config_macros: ImmutableList<String> = [];
			var cp_libs = new Ref<ImmutableList<String>>([]);
			var added_libs = new Hashtbl<String, Bool>();
			var no_output = false;
			var did_something = false;
			var force_typing = false;
			var pre_compilation = [];
			var interp = false;
			var swf_version = false;

			context.Common.define_value(com, HaxeVer, core.Globals.version_major + "." + core.Globals.version_minor + (""+core.Globals.version_revision).lpad("0",2));
			context.Common.raw_define(com, "haxe3");
			context.Common.define_value(com, Dce, "std");
			com.warning = function(msg:String, p:core.Globals.Pos) : Void {
				message(ctx, CMWarning(msg, p));
			};
			com.error = error.bind(ctx);
			if (context.common.CompilationServer.runs()) {
				com.run_command = run_command.bind(ctx);
			}
			syntax.Parser.display_error = function (e:syntax.parser.ErrorMsg, p:core.Globals.Pos) {
				com.error(syntax.Parser.error_msg(e), p);
			};

			syntax.Parser.use_doc = context.Common.display_default.get() == DMNone || context.common.CompilationServer.runs();
			com.class_path = get_std_class_paths();
			com.std_path = List.filter(function (p:String) : Bool {
					return p.endsWith("std/") || p.endsWith("std\\");
			}, com.class_path);
			var define = function (f:core.Define.StrictDefined) : Void->Void {
				// Arg.Unit (fun () -> context.Common.define(com, f) in
				return function () { context.Common.define(com, f); };
			}
			var process_ref = new Ref<ImmutableList<String>->Void>(function (args) {});
			var process_libs = function () {
				var libs = List.filter( function (l:String) {
					return !Hashtbl.mem(added_libs,l);
				}, List.rev(cp_libs.get()));
				cp_libs.set([]);
				List.iter(function (l) {
					Hashtbl.add(added_libs, l, true);
				}, libs);
				// immediately process the arguments to insert them at the place -lib was defined
				switch (add_libs(com, libs)) {
					case []:
					case args: process_ref.get()(args);
				}
			};
			var arg_delays: Array<Void->Void> = [];
			var basic_args_spec : Array<{arg:String, spec:ocaml.Arg.Spec, doc:String}>;
			basic_args_spec = [
				{arg:"-cp", spec:S_String(function (path:String) {
					process_libs();
					com.class_path = haxe.io.Path.addTrailingSlash(path)::com.class_path;
				}), doc:"<path> : add a directory to find source files"},
				{arg:"-js", spec:S_String(Initialize.set_platform.bind(com).bind(Js)), doc:"<file> : compile code to JavaScript file"},
				{arg:"-lua", spec:S_String(Initialize.set_platform.bind(com).bind(Lua)), doc:"<file> : compile code to Lua file"},
				{arg:"-swf", spec:S_String(Initialize.set_platform.bind(com).bind(Flash)), doc:"<file> : compile code to Flash SWF file"},
				{arg:"-as3", spec:S_String( function(dir:String) {
					Initialize.set_platform(com, Flash, dir);
					context.Common.define(com, As3);
					context.Common.define(com, NoInline);
				}), doc:"<directory> : generate AS3 code into target directory"},
				{arg:"-neko", spec:S_String(Initialize.set_platform.bind(com).bind(Neko)), doc:"<file> : compile code to Neko Binary"},
				{arg:"-php", spec:S_String(function (dir:String) {
					classes = (new core.Path(["php"], "Boot")) :: classes;
					Initialize.set_platform(com,Php,dir);
				}), doc:"<directory> : generate PHP code into target directory"},
				{arg:"-cpp", spec:S_String(Initialize.set_platform.bind(com).bind(Cpp)), doc:"<directory> : generate C++ code into target directory"},
				{arg:"-cppia", spec:S_String(function (file:String) {
					Initialize.set_platform(com,Cpp,file);
					context.Common.define(com,Cppia);
				}), doc:"<file> : generate Cppia code into target file"},
				{arg:"-cs", spec:S_String(function(dir:String) {
					cp_libs.set("hxcs" :: cp_libs.get());
					Initialize.set_platform(com,Cs,dir);
				}), doc:"<directory> : generate C# code into target directory"},
				{arg:"-java", spec:S_String(function(dir:String) {
					cp_libs.set("hxjava"::cp_libs.get());
					Initialize.set_platform(com,Java,dir);
				}), doc:"<directory> : generate Java code into target directory"},
				{arg:"-python", spec:S_String( Initialize.set_platform.bind(com).bind(Python)), doc:"<file> : generate Python code as target file"},
				{arg:"-hl", spec:S_String( Initialize.set_platform.bind(com).bind(Hl))
				, doc:"<file> : compile HL code as target file"},
				{arg:"-xml", spec:S_String( function(file:String) {
					syntax.Parser.use_doc = true;
					xml_out = Some(file);
				}), doc:"<file> : generate XML types description"},
				{arg:"-main", spec:S_String( function(cl:String) {
					if (com.main_class != None) {
						throw new ocaml.Arg.Bad("Multiple -main");
					}
					var cpath = core.Path.parse_type_path(cl);
					com.main_class = Some(cpath);
					classes = cpath :: classes;
				}), doc:"<class> : select startup class"},
				{arg:"-lib", spec:S_String( function(l:String) {
					cp_libs.set(l::cp_libs.get());
					context.Common.raw_define(com, l);
				}), doc:"<library[:version]> : use a haxelib library"},
				{arg:"-D", spec:S_String( function(v:String) {
					switch(v) {
						case "no_copt", "no-copt":
							com.foptimize = false;
						case "use_rtti_doc", "use-rtti-doc":
							syntax.Parser.use_doc = true;
						default:
							if (reserved_flags.indexOf(v) != -1) {
								throw new ocaml.Arg.Bad(v + " is a reserved compiler flag and cannot be defined from command line");
							}
					}
					context.Common.raw_define(com, v);
				}), doc:"<var[=value]> : define a conditional compilation flag"},
				{arg:"-v", spec:S_Unit(function() {
					com.verbose = true;
				}), doc:": turn on verbose mode"},
				{arg:"-debug", spec: S_Unit(function () {
					context.Common.define(com,Debug);
					com.debug = true;
				}), doc: ": add debug information to the compiled code"}
			];

			var adv_args_spec = [
				{arg:"-dce", spec:S_String( function(mode:String) {
					switch(mode) {
						case "std", "full", "no":
						default:
							throw new ocaml.Arg.Bad("Invalid DCE mode, expected std | full | no");
					}
					context.Common.define_value(com, Dce, mode);
				}), doc:"[std|full|no] : set the dead code elimination mode (default std)"},
				{arg:"-swf-version", spec:S_Float(function(v:Float) {
					if (!swf_version || com.flash_version < v) {
					 com.flash_version = v;
					}
					swf_version = true;
				}), doc:"<version> : change the SWF version"},
				{arg:"-swf-header", spec:S_String( function(h:String) {
					try {
						var split = h.split(":");
						swf_header = Some(switch (split.length) {
							case 3:
								var width = Std.parseInt(split.shift());
								var height = Std.parseInt(split.shift());
								var fps = Std.parseFloat(split.shift());
								if (width == null || height == null) {
									throw new Failure("int_of_string");
								}
								if (fps == Math.NaN) {
									throw new Failure("float_of_string");
								}
								{width:width, height:height, fps:fps, color:0xFFFFFF};
							case 4:
								var width = Std.parseInt(split.shift());
								var height = Std.parseInt(split.shift());
								var fps = Std.parseFloat(split.shift());
								var color = Std.parseInt(split.shift());
								if (width == null || height == null || color == null) {
									throw new Failure("int_of_string");
								}
								if (fps == Math.NaN) {
									throw new Failure("float_of_string");
								}
								{width:width, height:height, fps:fps, color:color};
							default: throw new ocaml.Exit();
						});
					}
					catch (_:Any) {
						throw new ocaml.Arg.Bad("Invalid SWF header format, expected width:height:fps[:color]");
					}
				}),doc:"<header> : define SWF header (width:height:fps:color)"},
				{arg:"-swf-lib", spec:S_String( function(file:String) {
					process_libs(); // linked swf order matters, and lib might reference swf as well
					trace("TODO: -swf-libs");
					// SwfLoader.add_swf_lib com file false
				}), doc:"<file> : add the SWF library to the compiled SWF"},
				{arg:"-swf-lib-extern", spec:S_String( function(file:String) {
					trace("TODO: -swf-lib-extern");
					// SwfLoader.add_swf_lib com file true
				}), doc:"<file> : use the SWF library for type checking"},
				{arg:"-java-lib", spec:S_String( function(file:String){
					trace("TODO: -java-lib");
					// let std = file = "lib/hxjava-std.jar" in
					// arg_delays := (fun () -> Java.add_java_lib com file std) :: !arg_delays;
				}),doc:"<file> : add an external JAR or class directory library"},
				{arg:"-net-lib", spec:S_String(function(file:String){
					trace("TODO: -net-lib");
					// let file, is_std = match ExtString.String.nsplit file "@" with
					// 	| [file] ->
					// 		file,false
					// 	| [file;"std"] ->
					// 		file,true
					// 	| _ -> raise Exit
					// in
					// arg_delays := (fun () -> Dotnet.add_net_lib com file is_std) :: !arg_delays;
				}), doc:"<file>[@std] : add an external .NET DLL file"},
				{arg:"-net-std", spec:S_String( function(file:String) {
					trace("TODO: -net-std");
					// Dotnet.add_net_std com file
				}), doc:"<file> : add a root std .NET DLL search path"},
				{arg:"-c-arg", spec:S_String( function(arg:String) {
					com.c_args = arg :: com.c_args;
				}), doc:"<arg> : pass option <arg> to the native Java/C# compiler"},
				{arg:"-x", spec:S_String( function(file:String) {
					var neko_file = file + ".n";
					Initialize.set_platform(com, Neko, neko_file);
					if (com.main_class == None ) {
						var cpath = core.Path.parse_type_path(file);
						com.main_class = Some(cpath);
						classes = cpath :: classes;
					}
					cmds.unshift("neko " + neko_file);
				}), doc:"<file> : shortcut for compiling and executing a neko file"},
				{arg:"-resource", spec:S_String( function(res:String) {
					var split = res.split("@");
					var file:String;
					var name:String;
					switch (split.length) {
						case 2: file = split[0]; name = split[1];
						case 1: file = split[0]; name = file;
						default: throw new ocaml.Arg.Bad("Invalid Resource format, expected file@name");
					}
					try {
						file = context.Common.find_file(com, file);
					}
					catch (e:ocaml.Not_found) {
					}
					var data:String = null;
					if (sys.FileSystem.exists(file)) {
						var s_bytes = sys.io.File.getBytes(file);
						if (s_bytes.length > 12000000) {
							throw new ocaml.Failure("Resource '" + file + "' excess the maximum size of 12MB");
						}
						data = s_bytes.toString();
					}
					else {
						throw new ocaml.Failure("Resource file not found : " + file);
					}
					if (com.resources.exists(name)) {
						throw new ocaml.Failure("Duplicate resource name " + name);
					}
					com.resources.set(name, data);
				}), doc:"<file>[@name] : add a named resource file"},
				{arg:"-prompt", spec:S_Unit(function () {
					compiler.Server.prompt = true;
				}),doc:": prompt on error"},
				{arg:"-cmd", spec:S_String( function(cmd:String) {
					cmds.unshift(compiler.DisplayOutput.unquote(cmd));
				}), doc:": run the specified command after successful compilation"},
				{arg:"--flash-strict", spec:S_Unit(function() { define(FlashStrict); }), doc:": more type strict flash API"},
				{arg:"--no-traces", spec:S_Unit(function() {define(NoTraces);}), doc:": don't compile trace calls in the program"},
				{arg:"--gen-hx-classes", spec:S_Unit(function() {
					force_typing = true;
					pre_compilation.unshift( function() {
						List.iter(function (lib) {
							// (fun (_,_,extract) -> Hashtbl.iter (fun n _ -> classes := n :: !classes) (extract()))
							trace("TODO gen-hx-classes swf");
						}, com.swf_libs);
						List.iter(function (lib) {
							// (fun (_,std,_,all_files,_) ->
							// 	if not std then
							// 		List.iter (fun path -> if path <> (["java";"lang"],"String") then classes := path :: !classes) (all_files())
							// )
							trace("TODO gen-hx-classes java");
						}, com.java_libs);
						List.iter(function (lib) {
							// (fun (_,std,all_files,_) ->
							// 	if not std then
							// 		List.iter (fun path -> classes := path :: !classes) (all_files())
							// )
							trace("TODO gen-hx-classes net");
						}, com.net_libs);
					});
					xml_out = Some("hx");
				}), doc:": generate hx headers for all input classes"},
				{arg:"--next", spec:S_Unit(function() { trace("Shall not be seen"); throw false; }), doc:": separate several haxe compilations"},
				{arg:"--each", spec:S_Unit(function() { trace("Shall not be seen"); throw false; }), doc:": append preceding parameters to all haxe compilations separated by --next"},
				{arg:"--display", spec:S_String( function(file_pos:String) {
					DisplayOutput.handle_display_argument(com,file_pos,pre_compilation,did_something);
				}),doc:": display code tips"},
				{arg:"--no-output", spec:S_Unit(function() {no_output = true; }), doc:": compiles but does not generate any file"},
				{arg:"--times", spec:S_Unit(function() { compiler.Server.measure_times = true; }), doc:": measure compilation times"},
				{arg:"--no-inline", spec:S_Unit(function() { define(NoInline); }), doc:": disable inlining"},
				{arg:"--no-opt", spec:S_Unit(function() {
					com.foptimize = false;
					context.Common.define(com, NoOpt);
				}), doc:": disable code optimizations"},
				{arg:"--remap", spec:S_String( function(s:String) {
					var split = s.split(":");
					if (split.length == 1) {
						throw new ocaml.Arg.Bad("Invalid remap format, expected source:target");
					}
					com.package_rules = PMap.add(split.shift(), context.Common.PackageRule.Remap(split.join(":")), com.package_rules);
				}), doc:"<package:target> : remap a package to another one"},
				{arg:"--interp", spec:S_Unit(function() {
					context.Common.define(com,Interp);
					Initialize.set_platform(com, core.Globals.macro_platform, "");
					interp = true;
				}), doc:": interpret the program using internal macro system"},
				{arg:"--macro", spec:S_String( function(e:String) {
					force_typing = true;
					config_macros = e :: config_macros;
				}), doc:" : call the given macro before typing anything else"},
				{arg:"--wait", spec:S_String( function(hp:String) {
					var accept = switch(hp) {
						case "stdio":
							compiler.Server.init_wait_stdio();
						default:
							var host = "127.0.0.1";
							var port : Null<Int>;
							if (hp.indexOf(":") != -1) {
								var split = hp.split(":");
								host = split[0];
								port = Std.parseInt(split[1]);
							}
							else {
								port = Std.parseInt(hp);
							}
							if (port == null) {
								throw new ocaml.Arg.Bad("Invalid port");
							}
							compiler.Server.init_wait_socket(com.verbose, host, port);
					};
					compiler.Server.wait_loop(process_params, com.verbose, accept);
				}), doc:"[[host:]port]|stdio] : wait on the given port (or use standard i/o) for commands to run)"},
				{arg:"--connect", spec:S_String( function(_:String) {
					trace("Shall not be seen"); throw false;
				}), doc:"<[host:]port> : connect on the given port and run commands there)"},
				{arg:"--cwd", spec:S_String( function(dir:String) {
					trace("Shall not be seen"); throw false;
				}),doc:"<dir> : set current working directory"},
				{arg:"-version", spec:S_Unit(function() {
					message(ctx, CMInfo(compiler.Server.s_version(),core.Globals.null_pos));
					did_something = true;
				}),doc:": print version and exit"},
				{arg:"--help-defines", spec:S_Unit(function() {
					var define_info = core.Define.get_documentation_list();
					var all = define_info.a;
					var max_length = define_info.b;

					var all_string = List.map(function (a:{a:String, b:String}) {
						var n = a.a; var doc = a.b;
						return " "+n.lpad(" ", max_length) + limit_string(doc, max_length + 3);
					}, all);
					List.iter(function (msg) { ctx.com.print(msg+"\n"); }, all_string);
					did_something = true;
				}), doc:": print help for all compiler specific defines"},
				{arg:"--help-metas", spec:S_Unit(function() {
					var meta_info = core.Meta.get_documentation_list();
					var all = meta_info.a;
					var max_length = meta_info.b;

					var all_string = List.map(function (a:{a:String, b:String}) {
						var n = a.a; var doc = a.b;
						return " "+n.lpad(" ", max_length) + limit_string(doc, max_length + 3);
					}, all);
					List.iter(function (msg) { ctx.com.print(msg+"\n"); }, all_string);
					did_something = true;
				}), doc:": print help for all compiler metadatas"}
			];

			var args_callback = function(cl:String) {
				var p = core.Path.parse_path(cl);
				if (core.Path.starts_uppercase(p.b)) {
					classes = p::classes;
				}
				else {
					force_typing = true;
					config_macros = ("include('" + cl +"', true, null, null, true)") :: config_macros;
				}
			};


			var all_args_spec = basic_args_spec.concat(adv_args_spec);
			var process = function (args:ImmutableList<String>) {
				var current = 0;
				try {
					var argv = ""::List.map(function (a:String) { return expand_env(a); }, args);
					current = Arg.parse_argv(current, argv, all_args_spec, args_callback, usage);
					List.iter(function (fn) { fn(); }, arg_delays);
				}
				catch (exc:ocaml.Arg.Bad) {
					var r = ~/unknown option `([-A-Za-z]+)'/;
					try {
						r.match(exc.s);
						var s = r.matched(1);
						var sl = [for (aas in all_args_spec) aas.arg];
						var msg_string = core.type.StringError.string_error_raise(s, sl, "Invalid command: "+s);
						throw new ocaml.Arg.Bad(msg_string);
					}
					catch (e:ocaml.Not_found) {
						throw exc;
					}
				}
				arg_delays = [];
			};
			process_ref.set(process);
			process(ctx.com.args);
			process_libs();
			if (com.display.dms_display) {
				com.warning = (com.display.dms_error_policy == EPCollect)
					? function(s:String, p:core.Globals.Pos) {
						context.Common.add_diagnostics_message(com, s, p, Warning);
					}
					: function (msg:String, p:core.Globals.Pos) {
						message(ctx, CMWarning(msg, p));
					};
				com.error = error.bind(ctx);
			}
			syntax.Lexer.old_format.set(context.Common.defined(com, OldErrorFormat));
			if (syntax.Lexer.old_format.get() && syntax.Parser.do_resume()) {
				var p = syntax.Parser.resume_display.get();
				// convert byte position to utf8 position
				var real_path = core.Path.get_real_path()(p.pfile);
				if (sys.FileSystem.exists(real_path)) {
					var content = sys.io.File.getBytes(real_path).toString();
					var pos = haxe.Utf8.length(content.substr(0, p.pmin));
					syntax.Parser.resume_display.set(new core.Globals.Pos(p.pfile, pos, pos));
				}
			}
			DisplayOutput.process_display_file(com, classes);
			var ext = Initialize.initialize_target(ctx, com, classes);
			// if we are at the last compilation step, allow all packages accesses - in case of macros or opening another project file
			if (com.display.dms_display) {
				if (!ctx.has_next) {
					com.package_rules = PMap.foldi(function (p:String, r:context.Common.PackageRule, acc) {
						return switch (r) {
							case Forbidden:
								acc;
							case _: PMap.add(p, r, acc);
						}
					}, com.package_rules, PMap.empty());
				}
			}
			com.config = context.Common.get_config(com); // make sure to adapt all flags changes defined after platform
			List.iter(function (f) { f(); }, List.rev(pre_compilation));
			var _tmp:ImmutableList<core.Path> = new core.Path([], "Std")::[];
			if (classes.equals(_tmp) && !force_typing) {
				var empty_func = S_Unit(function () {});
				var help_spec = basic_args_spec.concat([
					{arg:"-help", spec: empty_func, doc:": show extended help information"},
					{arg:"--help", spec: empty_func, doc:": show extended help information"},
					{arg:"--help-defines", spec: empty_func, doc:": print help for all compiler specific defines"},
					{arg:"--help-metas", spec: empty_func, doc:": print help for all compiler metadatas"},
					{arg:"<dot-path>", spec: empty_func, doc:": compile the module specified by dot-path"}
				]);
				if (cmds.length == 0 && !did_something) {
					std.Sys.println(ocaml.Arg.usage_string(help_spec, usage));
				}
			}
			else {
				ctx.setup();
				context.Common.log(com, "Classpath : " + List.join(";",com.class_path));
				var _tmp = PMap.foldi(function (k:String, v:String, acc:ImmutableList<String>) {
					var value = switch(v) {
						case "1":k;
						case _: k+"="+v;
					}
					return value::acc;
				}, com.defines.values, Tl);
				context.Common.log(com, "Defines : " + List.join(";",_tmp));
				var t = core.Timer.timer(["typing"]);
				context.Typecore.type_expr_ref.set(function (ctx, e, with_type) {
					return typing.Typer.type_expr(ctx, e, with_type);
				});
				var tctx = typing.Typer.create(com);
				List.iter(typing.MacroContext.call_init_macro.bind(tctx), List.rev(config_macros));
				List.iter(function (cpath) { tctx.g.do_load_module(tctx, cpath, core.Globals.null_pos); }, List.rev(classes));
				typing.Typer.finalize(tctx);
				t();
				if (!ctx.com.display.dms_display && ctx.has_error) {
					throw new Abort();
				}
				if (ctx.com.display.dms_exit_during_typing) {
					if (ctx.has_next || ctx.has_error) {
						throw new Abort();
					}
					throw new ocaml.Failure("No completion point was found");
				}

				var t = core.Timer.timer(["filters"]);
				var generated = typing.Typer.generate(tctx);
				com.main = generated.main;
				com.types = generated.types;
				com.modules = generated.modules;
				compiler.DisplayOutput.process_global_display_mode(com, tctx);
				if (!context.Common.defined(com, NoDeprecationWarnings)) {
					context.display.DeprecationCheck.run(com);
				}
				filters.Filters.run(com, tctx, generated.main);
				t();
				if (ctx.has_error) {
					throw new Abort();
				}
				switch (xml_out) {
					case None:
					case Some(file):
						if (file == "hx") {
							trace("TODO: create Genxml.generate_hx");
							// Genxml.generate_hx(com);
						}
						else {
							context.Common.log(com, "Generating xml : "+file);
							core.Path.mkdir_from_path(file);
							trace("TODO: create Genxml.generate");
							// Genxml.generate(com, file);
						}
				}
				if (!no_output) {
					generate(tctx, ext, xml_out, interp, swf_header);
				}
			}
			// Sys.catch_break false;
			List.iter(function (f) { f(); }, List.rev(com.callbacks.after_generation));
			if (!no_output) {
				var sdmc = cmds.copy();
				sdmc.reverse();
				for (element in sdmc) {
					var r = run_command(ctx, element);
					if (r != 0) {
						throw new ocaml.Failure("Command failed with error " + r);
					}
				}
			}
		}
		catch (e:Abort) {
		}
		catch (e:core.Error) {
			error(ctx, core.Error.error_msg(e.msg), e.pos);
		}
		catch (e:core.Error.Fatal_error) {
			error(ctx, e.s, e.pos);
		}
		catch (e:context.Common.Abort) {
			error(ctx, e.s, e.pos);
		}
		catch (e:syntax.lexer.Error) {
			error(ctx, syntax.Lexer.error_msg(e.error_msg), e.pos);
		}
		catch (e:syntax.parser.Error) {
			error(ctx, syntax.Parser.error_msg(e.error_msg), e.pos);
		}
		catch (e:syntax.parser.TypePath) {
			var fields = None;
			try {
				switch (e.c) {
					case None:
						fields = compiler.displayoutput.TypePathHandler.complete_type_path(com, e.p);
					case Some(v):
						fields = compiler.displayoutput.TypePathHandler.complete_type_path_inner(com, e.p, v.c, v.cur_package, e.is_import);
				}
			}
			catch (ee:context.Common.Abort) {
				error(ctx, ee.s, ee.pos);
			}
			switch (fields) {
				case None:
				case Some(v):
					DisplayOutput.DOException.Completion(DisplayOutput.print_fields(v));
			}
		}
		catch (e:context.Typecore.Forbid_package) {
			if (context.Common.display_default.get() != DMNone && ctx.has_next) {
				ctx.has_error = false;
				ctx.messages = [];
			}
			else {
				var pack = e.a.pack; var m = e.a.m; var p = e.a.p; var pl = e.pl; var pf = e.pf;
				var string_value = "You cannot access the "+pack+ " package while "+ ( (pf == "macro") ? "in a macro" : "targeting " + pf) + " (for "+core.Globals.s_type_path(m)+")";
				error(ctx, string_value, p);
				List.iter(error.bind(ctx, "    referenced here"), List.rev(pl));
			}
		}
		catch (e:macros.hlmacro.Error) {
			var msg = e.s;
			switch (e.p) {
				case p::l:
					message(ctx, CMError(msg, p));
					List.iter(function (p) {
						message(ctx, CMError("Called from", p));
					}, l);
					error(ctx, "Aborted", core.Globals.null_pos);
				case _: throw e;
			}
		}
		catch (e:typing.Typeload.GenericException) {
			error(ctx, e.s, e.p);
		}
		catch (e:ocaml.Arg.Bad) {
			error(ctx, "Error: "+e.s, core.Globals.null_pos);
		}
		catch (e:ocaml.Arg.Help) {
			message(ctx, CMInfo(e.s, core.Globals.null_pos));
		}
		catch (e:ocaml.Failure) {
			if (!Server.is_debug_run()) {
				error(ctx, "Error: "+e.msg, core.Globals.null_pos);
			}
			else {
				throw e;
			}
		}
		catch (e:context.Display.DisplayException) {
			switch (e) {
				case DisplayPackage(pack):
					throw DisplayOutput.DOException.Completion(List.join(".", pack));
				case DisplayFields(fields):
					var fields = List.map(function (element) {
						return {
							name:element.name,
							kind:element.kind,
							doc:switch(element.doc) {
									case Some(v):v;
									case None: "";
								}
						};
					}, fields);
					fields = if (Server.measure_times) {
						core.Timer.close_times();
						List.append(List.map(function (element) {
							return {
								name: "@TIME "+element.a,
								kind: context.Display.DisplayFieldKind.FKTimer(element.b),
								doc: ""
							};
						}, DisplayOutput.get_timer_fields(Server.start_time)), fields);
					}
					else {
						fields;
					}
					throw DisplayOutput.DOException.Completion(DisplayOutput.print_fields(fields));
				case DisplayType(t, p, doc):
					var doc_string = switch (doc) {
						case Some(v): v;
						case None: DisplayOutput.find_doc(t);
					}
					throw DisplayOutput.DOException.Completion(DisplayOutput.print_type(t, p, doc_string));
				case DisplaySignatures(signatures, display_arg):
					if (ctx.com.display.dms_kind == DMSignature) {
						throw DisplayOutput.DOException.Completion(DisplayOutput.print_signature(signatures,display_arg));
					}
					else {
						throw DisplayOutput.DOException.Completion(DisplayOutput.print_signatures(signatures));
					}
				case DisplayPosition(pl):
					throw DisplayOutput.DOException.Completion(DisplayOutput.print_positions(pl));
				case DisplayToplevel(il):
					var il = if (Server.measure_times) {
						core.Timer.close_times();
						List.append(List.map(function (element) {
							return context.common.identifiertype.T.ITTimer("@TIME "+element.a + ": "+element.b);
						}, DisplayOutput.get_timer_fields(Server.start_time)), il);
					}
					else {
						il;
					}
					throw DisplayOutput.DOException.Completion(DisplayOutput.print_toplevel(il));
				case ModuleSymbols(s), Diagnostics(s), Statistics(s), Metadata(s):
					throw DisplayOutput.DOException.Completion(s);
			}
		}
		catch (e:macros.eval.Sys_exit) {
			ctx.flush();
			if (Server.measure_times) {
				var f = function (s:String) {
					var err = std.Sys.stderr();
					err.writeString(s);
					err.writeString("\n");
					err.flush();
					err.close();
				};
				core.Timer.report_times(f);
				std.Sys.exit(e.i);
			}
		}
		// catch (e:generators.Hlinterp.Sys_exit) {
		// 	ctx.flush();
		// 	if (Server.measure_times) {
		// 		var f = function (s:String) {
		// 			var err = std.Sys.stderr();
		// 			err.writeString(s);
		// 			err.writeString("\n");
		// 			err.flush();
		// 			err.close();
		// 		};
		// 		core.Timer.report_times(f);
		// 		std.Sys.exit(e.i);
		// 	}
		// }
		catch (e:Dynamic) {
			trace("Exception  Dynamic", e);
			var orp = std.Sys.getEnv("OCAMLRUNPARAM");
			if ((orp == null || (orp != "b" && context.common.CompilationServer.runs())) && !Server.is_debug_run()) {
				error(ctx, ""+e, core.Globals.null_pos);
			}
			else {
				throw e;
			}
		}
	}

	public static function main () {
		{
			context.Typecore.make_call_ref.set(typing.Typer.make_call);
			context.Typecore.match_expr_ref.set(typing.matcher.Match.match_expr);
			context.Typecore.find_array_access_raise_ref.set(context.typecore.AbstractCast.find_array_access_raise);
		}
		var other = core.Timer.timer(["other"]);
		typing.MacroContext.setup();
		var args = std.Sys.args();

		try {
			trace("TODO: HAXE_COMPILATION_SERVER handling code");
			throw ocaml.Not_found.instance;
		}
		catch (_:ocaml.Not_found) {
			try {
				process_params(Server.createContext, args);
			}
			catch (c:DisplayOutput.DOException) {
				switch (c) {
					case Completion(s):
						var stderr = std.Sys.stderr();
						stderr.writeString(s+"\n");
						stderr.flush();
						stderr.close();
						std.Sys.exit(0);
					default:
				}

			}
			catch (a:ocaml.Arg.Bad) {
				var stderr = std.Sys.stderr();
				stderr.writeString("Error: "+a.s+"\n");
				stderr.flush();
				stderr.close();
				std.Sys.exit(1);
			}
		}
		other();
		if (compiler.Server.measure_times) {
			// core.Timer.report_times(function (s:String) {
			// 	var stderr = std.Sys.stderr();
			// 	stderr.writeString(s+"\n");
			// 	stderr.flush();
			// 	stderr.close();
			// });
		}
	}

}