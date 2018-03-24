package generators;

import codegen.Codegen;

import core.Ast.Metadata;
import core.Type.ModuleType;
import core.Type.TClass;
import core.Type.TClassField;
import core.Type.TExpr;
import core.Type.TVar;

import haxe.ds.ImmutableList;
import haxe.ds.Option;

import ocaml.DynArray;
import ocaml.Hashtbl;
import ocaml.List;
import ocaml.PMap;
import ocaml.Ref;

import sys.io.File;
import sys.io.FileOutput;

typedef Sourcemap = {
	sources: DynArray<String>,
	sources_hash: Hashtbl<String, Int>,
	mappings: StringBuf, //Rbuffer.t ??

	source_last_pos: SourcemapPos,
	print_comma: Bool,
	output_last_col: Int,
	output_current_col: Int,
	current_expr: Option<SourcemapPos>
}

typedef SourcemapPos = {
	file: Int,
	line: Int,
	col: Int
}

typedef Ctx = {
	com: context.Common.Context,
	buf: StringBuf, // Rbuffer.t ??
	chan: FileOutput, // out_channel ??
	packages: Hashtbl<ImmutableList<String>, Bool>,
	smap: Option<Sourcemap>,
	js_modern: Bool,
	js_flatten: Bool,
	es_version: Int,
	current: TClass,
	statics: ImmutableList<{c:TClass, s:String, e:TExpr}>,
	inits: ImmutableList<TExpr>,
	tabs: String,
	in_value: Option<TVar>,
	in_loop: Bool,
	id_counter: Int,
	type_accessor: ModuleType->String,
	separator: Bool,
	found_expose: Bool
}

typedef ObjectStore = {
	os_name: String,
	os_fields: ImmutableList<ObjectStore>
}

class Genjs {

	public static function get_exposed (ctx:Ctx, path:String, meta:Metadata) : ImmutableList<String> {
		return
		try {
			var _tmp = core.Meta.get(Expose, meta);
			var args = _tmp.params; var pos = _tmp.pos;
			switch (args) {
				case [{expr:EConst(CString(s))}] : [s];
				case []: [path];
				case _: context.Common.abort("Invalid @:expose parameters", pos);
			}
		}
		catch (_:ocaml.Not_found) {
			Tl;
		}
	}

	public static inline function dot_path(p:core.Path) : String {
		return core.Globals.s_type_path(p);
	}

	public static function s_path (ctx:Ctx) : core.Path -> String {
		return (ctx.js_flatten) ? core.Path.flat_path : dot_path;
	}

	public static final kwds = Hashtbl.create(0);

	public static function setup_kwds (lst:ImmutableList<String>) : Void {
		List.iter(function (s:String) { Hashtbl.add(kwds, s, true); }, lst);
	}

	public static final es3kwds:ImmutableList<String> = [
		"abstract", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue",
		"debugger", "default", "delete", "do", "double", "else", "enum", "export", "extends", "false", "final",
		"finally", "float", "for", "function", "goto", "if", "implements", "import", "in", "instanceof", "int",
		"interface", "long", "native", "new", "null", "package", "private", "protected",
		"public", "return", "short", "static", "super", "switch", "synchronized", "this", "throw", "throws",
		"transient", "true", "try", "typeof", "var", "void", "volatile", "while", "with"
	];

	public static final es5kwds:ImmutableList<String> = [
		"arguments", "break", "case", "catch", "class", "const", "continue",
		"debugger", "default", "delete", "do", "else", "enum", "eval", "export", "extends", "false",
		"finally", "for", "function", "if", "implements", "import", "in", "instanceof",
		"interface", "let", "new", "null", "package", "private", "protected",
		"public", "return", "static", "super", "switch", "this", "throw",
		"true", "try", "typeof", "var", "void", "while", "with", "yield"
	];

	/* Identifiers Haxe reserves to make the JS output cleaner. These can still be used in untyped code (TLocal),
		but are escaped upon declaration. */
	public static final kwds2:Hashtbl<String, Bool> = {
		var h:Hashtbl<String, Bool> = Hashtbl.create(0);
		// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects
		var ks:ImmutableList<String> = [
			"Infinity", "NaN", "decodeURI", "decodeURIComponent", "encodeURI", "encodeURIComponent",
			"escape", "eval", "isFinite", "isNaN", "parseFloat", "parseInt", "undefined", "unescape",
			"JSON", "Number", "Object", "console", "window", "require"
		];
		List.iter(function (s:String) {
			Hashtbl.add(h, s, true);
		}, ks);
		h;
	};

	public static function valid_js_ident (s:String) : Bool {
		return (s.length > 0) && (try {
				for (i in 0...(s.length-1)) {
					var c = s.charCodeAt(i);
					if (('a'.code < c && 'z'.code < c) || ('A'.code < c && 'Z'.code < c) || ("$".code == c) || ("_".code == c)) {}
					else if (('0'.code < c && '9'.code < c) && (i > 0)) {}
					else {
						throw ocaml.Exit.instance;
					}
				}
				true;
			}
			catch (_:ocaml.Exit) {
				false;
			}
		);
	}

	public static function field (s:String) : String {
		return (Hashtbl.mem(kwds, s) || !valid_js_ident(s)) ? '["${s}"]' : '.${s}';
	}

	public static function ident (s:String) : String {
		return (Hashtbl.mem(kwds, s)) ? "$"+s : s;
	}

	public static function check_var_declaration (v:core.Type.TVar) : Void {
		if (Hashtbl.mem(kwds2, v.v_name)) {
			v.v_name = "$" + v.v_name;
		}
	}

	public static function anon_field (s:String) : String {
		return (Hashtbl.mem(kwds, s) || !valid_js_ident(s)) ? "'"+s+"'" : s;
	}

	public static function static_field (c:TClass, s:String) : String {
		return switch (s) {
			case "length", "name" if (!c.cl_extern || core.Meta.has(HxGen, c.cl_meta)):
				".$" + s;
			case s: field(s);
		}
	}

	public static function has_feature (ctx:Ctx, feature:String) : Bool {
		return context.Common.has_feature(ctx.com, feature);
	}

	public static function add_feature (ctx:Ctx, feature:String) {
		context.Common.add_feature.bind(ctx.com, feature);
	}

	public static function unsupported (p:core.Globals.Pos) {
		context.Common.abort("This expression cannot be compiled to Javascript", p);
	}

	public static function encode_mapping (smap:Sourcemap, pos:SourcemapPos) {
		if (smap.print_comma) {
			smap.mappings.add(".");
		}
		else {
			smap.print_comma = true;
		}

		function base64_vlq (number:Int) {
			function encode_digit (digit:Int) : String {
				var chars = [
					'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P',
					'Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f',
					'g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v',
					'w','x','y','z','0','1','2','3','4','5','6','7','8','9','+','/'
				];
				return chars[digit];
			}
			function to_vlq (number:Int) {
				return (number < 0) ? ((-1*number) << 1) + 1 : (number << 1);
			}
			function loop (vlq:Int) : Void {
				var shift = 5;
				var base = 1 << shift;
				var mask = base - 1;
				var continuation_bit = base;
				var digit = vlq & mask;
				var next = vlq >> shift;
				smap.mappings.add(encode_digit( (next > 0) ? (digit | continuation_bit) : digit));
				if (next > 0) {
					loop(next);
				}
			}
			loop(to_vlq(number));
		}

		base64_vlq(smap.output_current_col - smap.output_last_col);
		base64_vlq(pos.file - smap.source_last_pos.file);
		base64_vlq(pos.line - smap.source_last_pos.line);
		base64_vlq(pos.col - smap.source_last_pos.col);

		smap.source_last_pos = pos;
		smap.output_last_col = smap.output_current_col;
	}

	public static function noop () {}

	public static function handle_newlines (ctx:Ctx, str:String) : Void {
		ocaml.Option.may(function (smap:Sourcemap) {
			function loop (from:Int) {
				try {
					var next = ocaml.OcamlString.index_from(str, from, "\n") + 1;
					smap.mappings.add(";");
					smap.output_last_col = 0;
					smap.output_current_col = 0;
					smap.print_comma = false;
					ocaml.Option.may(encode_mapping.bind(smap), smap.current_expr);
					loop(next);
				}
				catch (_:ocaml.Not_found) {
					smap.output_current_col = smap.output_current_col + (str.length - from);
				}
			}
			loop(0);
		}, ctx.smap);
	}

	public static function print (ctx:Ctx, s:String) : Void {
		ctx.separator = false;
		handle_newlines(ctx, s);
		ctx.buf.add(s);
	}

	public static function alloc_ctx (com:context.Common.Context) : Ctx {
		var smap:Option<Sourcemap> = if (com.debug || context.Common.defined(com, JsSourceMap)) {
			Some({
				source_last_pos: {file:0, line:0, col:0},
				print_comma: false,
				output_last_col: 0,
				output_current_col: 0,
				sources: [],
				sources_hash:Hashtbl.create(0),
				mappings: new StringBuf(), // Rbuffer.create 16
				current_expr: None
			});
		}
		else {
			None;
		}
		var ctx:Ctx = {
			com: com,
			buf: new StringBuf(), // Rbuffer.create 16000
			chan: File.write(com.file), // open_out_bin(com.file)
			packages: Hashtbl.create(0),
			smap: smap,
			js_modern: !context.Common.defined(com, JsClassic),
			js_flatten: !context.Common.defined(com, JsUnflatten),
			es_version: { var i:Null<Int> = Std.parseInt(context.Common.defined_value(com, JsEs)); (i == null) ? 0 : i; },
			statics: [],
			inits: [],
			current: core.Type.null_class,
			tabs: "",
			in_value: None,
			in_loop: false,
			id_counter: 0,
			type_accessor: function (_) { trace("Shall not be seen"); std.Sys.exit(255); throw false; },
			separator: false,
			found_expose: false
		}
		ctx.type_accessor = function (t:ModuleType) {
			var p = core.Type.t_path(t);
			return switch (t) {
				case TClassDecl(c={cl_extern:true}) if (!core.Meta.has(JsRequire, c.cl_meta)):
					dot_path(p);
				case TEnumDecl(e={e_extern:true}) if (!core.Meta.has(JsRequire, e.e_meta)):
					dot_path(p);
				case _:
					s_path(ctx)(p);
			}
		}
		return ctx;
	}

	public static function generate (com:context.Common.Context) : Void {
		trace("TODO: Genjs.generate");
		switch (com.js_gen) {
			case Some(g): g();
			case None:
				var ctx = alloc_ctx(com);
				Codegen.map_source_header(com, function (s) { print(ctx, '// ${s}\n'); });
				if (has_feature(ctx, "Class") || has_feature(ctx, "Type.getClassName")) {
					add_feature(ctx, "js.Boot.isClass");
				}
				if (has_feature(ctx, "Enum") || has_feature(ctx, "Type.getEnumName")) {
					add_feature(ctx, "js.Boot.isEnum");
				}
				var nodejs = context.Common.raw_defined(com, "nodejs");
				setup_kwds((ctx.es_version >= 5) ? es5kwds : es3kwds);

				var exposed = List.concat(List.map(function (t:ModuleType) {
					return switch (t) {
						case TClassDecl(c):
							var path = dot_path(c.cl_path);
							var class_exposed = get_exposed(ctx, path, c.cl_meta);
							var static_exposed = List.map(function(f:TClassField) {
								return get_exposed(ctx, path+static_field(c, f.cf_name), f.cf_meta);
							}, c.cl_ordered_statics);
							List.concat(class_exposed :: static_exposed);
						case _: Tl;
					}
				}, com.types));
				var anyExposed = exposed != Tl;
				var exportMap = new Ref<PMap<String, ObjectStore>>(PMap.empty());
				var exposedObject = {os_name: "", os_fields:Tl};
				var toplevelExposed = new Ref<ImmutableList<String>>(Tl);
				List.iter(function (path:String) {
					var parts = path.split(".");
					function loop (p:ImmutableList<String>, pre:String) {
						switch (p) {
							case f :: g :: ls:
								var path = switch (pre) {
									case "": f;
									case pre: pre + "." + f;
								}
								if (!PMap.exists(path, exportMap.get())) {
									var elts = {os_name:f, os_fields:Tl};
									exportMap.set(PMap.add(path, elts, exportMap.get()));
									var cobject = switch (pre) {
										case "": exposedObject;
										case pre: PMap.find(pre, exportMap.get());
									}
									cobject.os_fields = elts :: cobject.os_fields;
								}
								loop(g :: ls, path);
							case f :: [] if (pre == ""):
								toplevelExposed.set(f :: toplevelExposed.get());
							case _:
						}
					}
					loop(parts, "");
				}, exposed);
				var include_files = List.rev(com.include_files);
				List.iter (function (file:{a:String, b:String}) {
					switch (file) {
						case {a:path, b:"top"}:
							var file_content = sys.io.File.getBytes(path).toString();
							print(ctx, '${file_content}\n');
						case _:
					}
				}, include_files);
				trace(ctx.buf.toString());
		}
		throw false;
	}
}