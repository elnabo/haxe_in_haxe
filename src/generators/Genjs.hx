package generators;

import codegen.Codegen;

import core.Type.ModuleType;
import core.Type.TClass;
import core.Type.TExpr;
import core.Type.TVar;

import haxe.ds.ImmutableList;
import haxe.ds.Option;

import ocaml.DynArray;
import ocaml.Hashtbl;

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

	public static inline function dot_path(p:core.Path) : String {
		return core.Globals.s_type_path(p);
	}

	public static function s_path (ctx:Ctx) : core.Path -> String {
		return (ctx.js_flatten) ? core.Path.flat_path : dot_path;
	}

	public static function handle_newlines (ctx:Ctx, str:String) : Void {
		trace("TODO: Genjs.handle_newlines");
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
		}
		throw false;
	}
}