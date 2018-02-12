package compiler;

import haxe.ds.ImmutableList;
import ocaml.List;

enum DOException {
	Completion(s:String);
}

typedef DOField = {name:String, kind:context.Display.DisplayFieldKind, doc:String}

class DisplayOutput {

	public static inline function htmlescape (s:String) : String{
		return StringTools.htmlEscape(s);
	}

	public static function get_timer_fields (start_time:Float) : ImmutableList<{a:String, b:String}> {
		var tot = 0.0;
		for (value in core.Timer.htimers) {
			tot += value.total;
		}
		var td = core.Timer.get_time() - start_time;
		var fields = [{a:"@TOTAL", b:Std.int(td)+ "." + (Std.int(td*1000)%1000) + "s"}];
		if (tot > 0.0) {
			for (value in core.Timer.htimers) {
				fields.unshift({
					a:value.id.join("."),
					b:Std.int(value.total)+ "." + (Std.int(value.total*1000)%1000) + "s (" + Std.int(value.total * 100.0 / tot) + "%)"
				});
			}
		}
		return fields;
	}

	public static function print_fields (fields:ImmutableList<DOField>) {
		var b = new StringBuf();
		b.add("<list>\n");
		var f = List.sort(function (a:DOField, b:DOField){
			var av = context.Display.display_field_kind_index(a.kind);
			var bv = context.Display.display_field_kind_index(b.kind);
			if (av == bv) { 
				if (a.name == b.name) { return 0; }
				return (a.name > b.name) ? 1 : -1; 
			}
			return (av > bv) ? 1 : -1;

		}, fields);
		List.iter(function (element:DOField) {
			var s_kind:String;
			var t:String;
			switch (element.kind) {
				case FKVar(s) : 
					s_kind = "var";
					t = core.Type.s_type(core.Type.print_context(), s);
				case FKMethod(s):
					s_kind = "method";
					t = core.Type.s_type(core.Type.print_context(), s);
				case FKType(s):
					s_kind = "type";
					t = core.Type.s_type(core.Type.print_context(), s);
				case FKPackage:
					s_kind = "package";
					t = "";
				case FKModule:
					s_kind = "type"; // is "type" in ocaml maybe should be "module"?
					t = "";
				case FKMetadata:
					s_kind = "metadata";
					t = "";
				case FKTimer(s):
					s_kind = "timer";
					t = s;
			}
			b.add('<i n="');
			b.add(element.name);
			b.add('" k="');
			b.add(s_kind);
			b.add('"><t>');
			b.add(htmlescape(t));
			b.add('</t><d>');
			b.add(htmlescape(element.doc));
			b.add('</d></i>\n');
		}, f);
		b.add("</list>\n");
		return b.toString();
	}

	public static function print_toplevel (il:ImmutableList<context.common.identifiertype.T>) : String {
		trace("TDOO: compiler.DisplayOutput.print_toplevel");
		return null;
	}

	public static function print_type (t:core.Type.T, p:core.Globals.Pos, doc:String) : String {
		trace("TODO finish compiler.DisplayOutput.print_type");
		var b = new StringBuf();
		var null_pos = core.Globals.null_pos;
		if (p.pfile == null_pos.pfile && p.pmin == null_pos.pmin && p.pmax == null_pos.pmax) {
			b.add("<type");
		}
		else {

		}
		return b.toString();

		// let b = Buffer.create 0 in
		// if p = null_pos then
		// 	Buffer.add_string b "<type"
		// else begin
		// 	let error_printer file line = Printf.sprintf "%s:%d:" (Path.unique_full_path file) line in
		// 	let epos = Lexer.get_error_pos error_printer p in
		// 	Buffer.add_string b ("<type p=\"" ^ (htmlescape epos) ^ "\"")
		// end;
		// Buffer.add_string b (maybe_print_doc doc);
		// Buffer.add_string b ">\n";
		// Buffer.add_string b (htmlescape (s_type (print_context()) t));
		// Buffer.add_string b "\n</type>\n";
		// Buffer.contents b
	}

	public static function print_signatures (tl:ImmutableList<{sig:core.Type.TSignature, doc:core.Ast.Documentation}>) : String {
		var b = new StringBuf();
		List.iter(function (element:{sig:core.Type.TSignature, doc:core.Ast.Documentation}) {
			b.add("<type");
			switch (element.doc) {
				case None:
				case Some(v):
					b.add(' d="');
					b.add(htmlescape(v));
					b.add('"');
			}
			b.add(">\n");
			b.add(htmlescape(core.Type.s_type(core.Type.print_context(), TFun(element.sig))));
			b.add("\n</type>\n");
		}, tl);
		return b.toString();
	}

	public static function print_positions(pl:ImmutableList<core.Globals.Pos>) : String {
		var b = new StringBuf();
		var error_printer = function(file:String, line:Int) : String {
			return core.Path.get_real_path()(file) + ":" + line + ":";
		};
		b.add("<list>\n");
		List.iter(function (p) {
			b.add("<pos>");
			b.add(syntax.Lexer.get_error_pos(error_printer, p));
			b.add("</pos>");
		}, pl);
		b.add("</list>");
		return b.toString();
	}

	public static function print_signature (tl:ImmutableList<{sig:core.Type.TSignature, doc:core.Ast.Documentation}>, display_arg:Int) : String {
		trace("TODO compiler.DisplayOutput.print_signature");
		return null;
	}

	public static function unquote(v:String) : String {
		var len = v.length;
		if (len > 0 && v.charAt(0) == '"' && v.charAt(len - 1) == '"') {
			return v.substr(1, len - 2);
		}
		else {
			return v;
		}
	}

	public static function handle_display_argument (com:context.Common.Context, file_pos:String, pre_compilation:Dynamic, did_something:Bool) {
		trace("TODO: compiler.DisplayOutput.handle_display_argument");
		// match file_pos with
		// | "classes" ->
		// 	pre_compilation := (fun() -> raise (Parser.TypePath (["."],None,true))) :: !pre_compilation;
		// | "keywords" ->
		// 	raise (Completion (print_keywords ()))
		// | "memory" ->
		// 	did_something := true;
		// 	(try display_memory com with e -> prerr_endline (Printexc.get_backtrace ()));
		// | "diagnostics" ->
		// 	Common.define com Define.NoCOpt;
		// 	com.display <- DisplayMode.create (DMDiagnostics true);
		// 	Common.display_default := DMDiagnostics true;
		// | _ ->
		// 	let file, pos = try ExtString.String.split file_pos "@" with _ -> failwith ("Invalid format: " ^ file_pos) in
		// 	let file = unquote file in
		// 	let pos, smode = try ExtString.String.split pos "@" with _ -> pos,"" in
		// 	let mode = match smode with
		// 		| "position" ->
		// 			Common.define com Define.NoCOpt;
		// 			DMPosition
		// 		| "usage" ->
		// 			Common.define com Define.NoCOpt;
		// 			DMUsage false
		// 		(*| "rename" ->
		// 			Common.define com Define.NoCOpt;
		// 			DMUsage true*)
		// 		| "package" ->
		// 			DMPackage
		// 		| "type" ->
		// 			Common.define com Define.NoCOpt;
		// 			DMType
		// 		| "toplevel" ->
		// 			Common.define com Define.NoCOpt;
		// 			DMToplevel
		// 		| "module-symbols" ->
		// 			Common.define com Define.NoCOpt;
		// 			DMModuleSymbols None;
		// 		| "diagnostics" ->
		// 			Common.define com Define.NoCOpt;
		// 			DMDiagnostics false;
		// 		| "statistics" ->
		// 			Common.define com Define.NoCOpt;
		// 			DMStatistics
		// 		| "signature" ->
		// 			DMSignature
		// 		| "" ->
		// 			DMField
		// 		| _ ->
		// 			let smode,arg = try ExtString.String.split smode "@" with _ -> pos,"" in
		// 			match smode with
		// 				| "resolve" ->
		// 					DMResolve arg
		// 				| "workspace-symbols" ->
		// 					Common.define com Define.NoCOpt;
		// 					DMModuleSymbols (Some arg)
		// 				| _ ->
		// 					DMField
		// 	in
		// 	let pos = try int_of_string pos with _ -> failwith ("Invalid format: "  ^ pos) in
		// 	com.display <- DisplayMode.create mode;
		// 	Common.display_default := mode;
		// 	Common.define_value com Define.Display (if smode <> "" then smode else "1");
		// 	Parser.use_doc := true;
		// 	Parser.resume_display := {
		// 		pfile = Path.unique_full_path file;
		// 		pmin = pos;
		// 		pmax = pos;
		// }
	}

	public static function process_display_file (com:context.Common.Context, classes:ImmutableList<core.Path> ){
		trace("TODO: compiler.DisplayOutput.process_display_file");
		// let get_module_path_from_file_path com spath =
		// 	let rec loop = function
		// 		| [] -> None
		// 		| cp :: l ->
		// 			let cp = (if cp = "" then "./" else cp) in
		// 			let c = Path.add_trailing_slash (Path.get_real_path cp) in
		// 			let clen = String.length c in
		// 			if clen < String.length spath && String.sub spath 0 clen = c then begin
		// 				let path = String.sub spath clen (String.length spath - clen) in
		// 				(try
		// 					let path = Path.parse_path path in
		// 					(match loop l with
		// 					| Some x as r when String.length (s_type_path x) < String.length (s_type_path path) -> r
		// 					| _ -> Some path)
		// 				with _ -> loop l)
		// 			end else
		// 				loop l
		// 	in
		// 	loop com.class_path
		// in
		// match com.display.dms_display_file_policy with
		// 	| DFPNo ->
		// 		()
		// 	| dfp ->
		// 		if dfp = DFPOnly then begin
		// 			classes := [];
		// 			com.main_class <- None;
		// 		end;
		// 		let real = Path.get_real_path (!Parser.resume_display).pfile in
		// 		(match get_module_path_from_file_path com real with
		// 		| Some path ->
		// 			if com.display.dms_kind = DMPackage then raise (DisplayPackage (fst path));
		// 			classes := path :: !classes
		// 		| None ->
		// 			if not (Sys.file_exists real) then failwith "Display file does not exist";
		// 			(match List.rev (ExtString.String.nsplit real Path.path_sep) with
		// 			| file :: _ when file.[0] >= 'a' && file.[0] <= 'z' -> failwith ("Display file '" ^ file ^ "' should not start with a lowercase letter")
		// 			| _ -> ());
		// 			failwith "Display file was not found in class path"
		// 		);
		// 		Common.log com ("Display file : " ^ real);
		// 		Common.log com ("Classes found : ["  ^ (String.concat "," (List.map s_type_path !classes)) ^ "]")
	}

	public static function process_global_display_mode (com:context.Common.Context, tctx:context.Typecore.Typer) : Void {
		trace("TODO: compiler.DisplayOutput.process_global_display_mode");
	}

	public static function find_doc (t:core.Type.T) : String {
		trace("TODO: compiler.DisplayOutput.find_doc");
		return null;
	}

}