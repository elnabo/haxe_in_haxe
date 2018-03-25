package generators;

import codegen.Codegen;

import core.Path;
import core.Ast.Metadata;
import core.Globals.Pos;
import core.Type.ModuleType;
import core.Type.TClass;
import core.Type.TClassField;
import core.Type.TConstant;
import core.Type.TEnum;
import core.Type.TExpr;
import core.Type.TExprExpr;
import core.Type.TFunc;
import core.Type.TObjectField;
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

using equals.Equal;
using ocaml.Cloner;

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

	public static inline function dot_path(p:Path) : String {
		return core.Globals.s_type_path(p);
	}

	public static function s_path (ctx:Ctx) : Path -> String {
		return (ctx.js_flatten) ?Path.flat_path : dot_path;
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

	public static function unsupported (p:Pos) {
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

	public static function add_mapping_ (smap:Sourcemap, pos:Pos) : Void->Void {
		return
		if (pos.pmin < 0) { noop; }
		else {
			var file = try {
				Hashtbl.find(smap.sources_hash, pos.pfile);
			}
			catch (_:ocaml.Not_found) {
				var length = smap.sources.length;
				Hashtbl.replace(smap.sources_hash, pos.pfile, length);
				smap.sources.push(pos.pfile);
				length;
			}

			var pos:SourcemapPos = {
				var tmp = syntax.Lexer.find_pos(pos);
				var line = tmp.fst; var col = tmp.snd;
				{file: file, line: line, col: col};
			};

			if (smap.source_last_pos.diff(pos)) {
				var old_current_expr = smap.current_expr;
				smap.current_expr = Some(pos);
				encode_mapping(smap, pos);
				function () { smap.current_expr = old_current_expr; };
			}
			else {
				noop;
			}
		}
	}

	public static function add_mapping (ctx:Ctx, e:TExpr) : Void->Void {
		return ocaml.Option.map_default(function (smap:Sourcemap) { return add_mapping_(smap, e.epos); }, noop, ctx.smap);
	}

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

	public static function flush (ctx:Ctx) {
		ctx.chan.writeString(ctx.buf.toString());
		ctx.buf = new StringBuf();
	}

	public static function spr (ctx:Ctx, s:String) : Void {
		ctx.separator = false;
		handle_newlines(ctx, s);
		ctx.buf.add(s);
	}

	public static function print (ctx:Ctx, s:String) : Void {
		ctx.separator = false;
		handle_newlines(ctx, s);
		ctx.buf.add(s);
	}

	public static function write_mappings (ctx:Ctx, smap:Sourcemap) : Void {
		trace("TODO: write_mappings");
		throw false;
	}

	public static function newline (ctx:Ctx) : Void {
		switch (ctx.buf.toString().charAt(ctx.buf.length - 1)) {
			case "}", "{", ":", ";" if (!ctx.separator):
				print(ctx, '\n${ctx.tabs}');
			case _:
				print(ctx, ';\n${ctx.tabs}');
		}
	}

	public static function newprop (ctx:Ctx) : Void {
		switch (ctx.buf.toString().charAt(ctx.buf.length - 1)) {
			case "{":
				print(ctx, '\n${ctx.tabs}');
			case _:
				print(ctx, '\n${ctx.tabs},');
		}
	}

	public static function semicolon (ctx:Ctx) : Void {
		switch (ctx.buf.toString().charAt(ctx.buf.length - 1)) {
			case "}" if (!ctx.separator):
			case _:
				spr(ctx, ";");
		}
	}

	public static function concat<A> (ctx:Ctx, s:String, f:A->Void, l:ImmutableList<A>) {
		switch (l) {
			case []:
			case [x]: f(x);
			case x :: l:
				f(x);
				spr(ctx, s);
				concat(ctx, s, f, l);
		}
	}

	public static function fun_block (ctx:Ctx, f:TFunc, p:Pos) {
		var e = List.fold_left(function (e:TExpr, tmp) {
			var a = tmp.v; var c = tmp.c;
			return switch (c) {
				case None, Some(TNull): e;
				case Some(c): core.Type.concat(core.Texpr.set_default(ctx.com.basic, a, c, p), e);
			}
		}, f.tf_expr, f.tf_args);
		return e;
	}

	public static function open_block (ctx:Ctx) : Void->Void {
		var oldt = ctx.tabs;
		ctx.tabs = "\t"+ctx.tabs;
		return function () { ctx.tabs = oldt; };
	}

	public static function needs_switch_break (e:TExpr) : Bool {
		return switch(e.eexpr) {
			case TBlock([]): true;
			case TBlock(el): needs_switch_break(List.hd(List.rev(el)));
			case TMeta({name:LoopLabel}, {eexpr:TBreak}),
				TContinue, TReturn(_), TThrow(_): false;
			case _: true;
		}
	}

	public static function this_ (ctx:Ctx) : String {
		return switch (ctx.in_value) {
			case None: "this";
			case Some(_): "$this";
		}
	}

	public static function is_dynamic_iterator (ctx:Ctx, e:TExpr) : Bool {
		function check(x:TExpr) : Bool {
			function loop (t:core.Type.T) : Bool {
				return switch (core.Type.follow(t)) {
					case TInst({cl_path:{a:Tl, b:"Array"}}, _),
						TInst({cl_kind:KTypeParameter(_)}, _),
						TAnon(_), TDynamic(_), TMono(_):
						true;
					case TAbstract(a, tl) if (!core.Meta.has(CoreType, a.a_meta)):
						loop(core.Abstract.get_underlying_type(a, tl));
					case _:
						false;
				}
			}
			return has_feature(ctx, "HxOverrides.iter") && loop(x.etype);
		}
		return switch (e.eexpr) {
			case TField(x, f) if (core.Type.field_name(f) == "iterator"):
				check(x);
			case _:
				false;
		}
	}

	public static function gen_constant (ctx:Ctx, p:Any, c:TConstant) : Void {
		switch (c) {
			case TInt(i): print(ctx, '${i}');
			case TFloat(s): spr(ctx, s);
			case TString(s): print(ctx, '"${core.Ast.s_escape(s)}"');
			case TBool(b): spr(ctx, (b) ? "true" : "false");
			case TNull: spr(ctx, "null");
			case TThis: spr(ctx, this_(ctx));
			case TSuper: trace("Shall not be seen"); std.Sys.exit(255); throw false;
		}
	}

	public static inline function print_deprecation_message (com:context.Common.Context, msg:String, p:Pos) : Void {
		com.warning(msg, p);
	}

	public static function gen_call (ctx:Ctx, e:TExpr, el:ImmutableList<TExpr>, in_value) : Void {
		switch [e.eexpr, el] {
			case [TConst(TSuper), params]:
				switch (ctx.current.cl_super) {
					case None: context.Common.abort("Missing api.setCurrentClass", e.epos);
					case Some({c:c}):
						print(ctx, '${ctx.type_accessor(TClassDecl(c))}.call(${this_(ctx)}');
						List.iter(function (p:TExpr) { print(ctx, ","); gen_value(ctx, p); }, params);
						spr(ctx, ")");
				}
			case [TField({eexpr:TConst(TSuper)}, f), params]:
				switch (ctx.current.cl_super) {
					case None: context.Common.abort("Missing api.setCurrentClass", e.epos);
					case Some({c:c}):
						var name = core.Type.field_name(f);
						print(ctx, '${ctx.type_accessor(TClassDecl(c))}.prototype${field(name)}.call(${this_(ctx)}');
						List.iter(function (p:TExpr) { print(ctx, ","); gen_value(ctx, p); }, params);
						spr(ctx, ")");
				}
			case [TCall(x,_), el] if (switch (x.eexpr) { case TIdent("__js__"): false; case _: true;}):
				spr(ctx, '(');
				gen_value(ctx, e);
				spr(ctx, ")");
				spr(ctx, "(");
				concat(ctx, ",", gen_value.bind(ctx), el);
				spr(ctx, ")");
			case [TField(_, FStatic({cl_path:{a:["js"], b:"Syntax"}}, {cf_name:meth})), args]:
				gen_syntax(ctx, meth, args, e.epos);
			case [TIdent("__new__"), args]:
				print_deprecation_message(ctx.com, "__new__ is deprecated, use js.Syntax.new_ instead", e.epos);
				gen_syntax(ctx, "new_", args, e.epos);
			case [TIdent("__js__"), args]:
				// TODO: add deprecation warning when we figure out what to do with purity here
				gen_syntax(ctx, "code", args, e.epos);
			case [TIdent("__instanceof__"), args]:
				print_deprecation_message(ctx.com, "__instanceof__ is deprecated, use js.Syntax.instanceof instead", e.epos);
				gen_syntax(ctx, "instanceof", args, e.epos);
			case [TIdent("__typeof__"), args]:
				print_deprecation_message(ctx.com, "__typeof__ is deprecated, use js.Syntax.typeof instead", e.epos);
				gen_syntax(ctx, "typeof", args, e.epos);
			case [TIdent("__strict_eq__"), args]:
				print_deprecation_message(ctx.com, "__strict_eq__ is deprecated, use js.Syntax.strictEq instead", e.epos);
				gen_syntax(ctx, "strictEq", args, e.epos);
			case [TIdent("__strict_neq__"), args]:
				print_deprecation_message(ctx.com, "__strict_neq__ is deprecated, use js.Syntax.strictNeq instead", e.epos);
				gen_syntax(ctx, "strictNeq", args, e.epos);
			case [TIdent("__define_feature__"), [_, e]]:
				gen_expr(ctx, e);
			case [TIdent("__feature__"), {eexpr:TConst(TString(f))} :: eif :: eelse]:
				if (has_feature(ctx, f)) {
					gen_value(ctx, eif);
				}
				else {
					switch (eelse) {
						case []:
						case e :: _:
							gen_value(ctx, e);
					}
				}
			case [TIdent("__resources__"), []]:
				spr(ctx, '[');
				concat(ctx, ",", function (tmp) {
					var name = tmp.fst; var data = tmp.snd;
					spr(ctx, "{ ");
					spr(ctx, "name : ");
					gen_constant(ctx, e.epos, TString(name));
					spr(ctx, ", data : ");
					gen_constant(ctx, e.epos, TString(codegen.Codegen.bytes_serialize(data)));
					spr(ctx, "}");
				}, Hashtbl.fold(function (name:String, data:String, acc:ImmutableList<{fst:String, snd:String}>) { return {fst:name, snd:data} :: acc; }, ctx.com.resources, Tl));
				spr(ctx, "]");
			case [TIdent("`trace"), [e,infos]]:
				if (has_feature(ctx, "haxe.Log.trace")) {
					var t = try {
						List.find(function (t:ModuleType) {
							return core.Type.t_path(t).equals(new Path(["haxe"], "Log"));
						}, ctx.com.types);
					}
					catch (_:Any) {
						trace("Shall not be seen"); std.Sys.exit(255); throw false;
					}
					spr(ctx, ctx.type_accessor(t));
					spr(ctx, ".trace(");
					gen_value(ctx, e);
					spr(ctx, ",");
					gen_value(ctx, infos);
					spr(ctx, ")");
				}
				else {
					spr(ctx, "console.log(");
					switch (infos.eexpr) {
						case TObjectDecl({name:"fileName", expr:{eexpr:TConst(TString(file))}} :: {name:"lineNumber", expr:{eexpr:TConst(TInt(line))}} :: _):
							print(ctx, '"${file}:${line}:",');
						case _:
					}
					gen_value(ctx, e);
					spr(ctx, ")");
				}
			case [TField(x, f), []] if (core.Type.field_name(f) == "iterator" && is_dynamic_iterator(ctx, e)):
				add_feature(ctx, "use.$getIterator");
				print(ctx, "$getIterator(");
				gen_value(ctx, x);
				print(ctx, ")");
			case _:
				gen_value(ctx, e);
				spr(ctx, "(");
				concat(ctx, ",", gen_value.bind(ctx), el);
				spr(ctx, ")");
		}
	}

	/*
		this wraps {} in parenthesis which is required to produce valid js code for field and array access to it.
		the case itself is very rare and most probably comes from redundant code fused by the analyzer,
		but we still have to support it.
	*/
	public static function add_objectdecl_parens (e:TExpr) : TExpr {
		function loop(e:TExpr) : TExpr {
			return switch (e.eexpr) {
				case TCast(e1, None), TMeta(_,e1): loop(e1); // TODO: do we really want to lose these?
				case TObjectDecl(_): e.with({eexpr:TParenthesis(e)});
				case _: e;
			}
		}
		return loop(e);
	}

	public static function gen_expr (ctx:Ctx, e:TExpr) : Void {
		var clear_mapping = add_mapping(ctx, e);
		switch (e.eexpr) {
			case TConst(c): gen_constant(ctx, e.epos, c);
			case TLocal(v): spr(ctx, ident(v.v_name));
			case TArray(e1, e2):
				gen_value(ctx, add_objectdecl_parens(e1));
				spr(ctx, "[");
				gen_value(ctx, e2);
				spr(ctx, "]");
			case TBinop(op, {eexpr:TField(x, f)}, e2) if (core.Type.field_name(f) == "iterator"):
				gen_value(ctx, x);
				spr(ctx, field("iterator"));
				print(ctx, ' ${core.Ast.s_binop(op)}');
				gen_value(ctx, e2);
			case TBinop(op, e1, e2):
				gen_value(ctx, e1);
				print(ctx, ' ${core.Ast.s_binop(op)}');
				gen_value(ctx, e2);
			case TField(x, f) if (core.Type.field_name(f) == "iterator" && is_dynamic_iterator(ctx, e)):
				add_feature(ctx, "use.$iterator");
				print(ctx, "$iterator(");
				gen_value(ctx, x);
				print(ctx, ")");
			// Don't generate `$iterator(value)` for exprs like `value.iterator--`
			case TUnop(op, flag, fe={eexpr:TField(x, f)}) if (core.Type.field_name(f) == "iterator" && is_dynamic_iterator(ctx, fe)):
				switch (flag) {
					case Prefix:
						spr(ctx, core.Ast.s_unop(op));
						gen_value(ctx, x);
						spr(ctx, ".iterator");
					case Postfix:
						gen_value(ctx, x);
						spr(ctx, ".iterator");
						spr(ctx, core.Ast.s_unop(op));
				}
			case TField(x, FClosure(Some({c:{cl_path:{a:[], b:"Array"}}}), {cf_name:"push"})):
				// see https://github.com/HaxeFoundation/haxe/issues/1997
				add_feature(ctx, "use.$arrayPush");
				add_feature(ctx, "use.$bind");
				print(ctx, "$bind(");
				gen_value(ctx, x);
				print(ctx, ",$arrayPush");
			case TField(x, FClosure(_, f)):
				add_feature(ctx, "use.$bind");
				switch (x.eexpr) {
					case TConst(_), TLocal(_):
						print(ctx, "$bind(");
						gen_value(ctx, x);
						print(ctx, ",");
						gen_value(ctx, x);
						print(ctx, '${(core.Meta.has(SelfCall, f.cf_meta)) ? "" : field(f.cf_name)})');
					case _:
						print(ctx, "($_=");
						gen_value(ctx, x);
						print(ctx, ',$$bind($$_,$$_${(core.Meta.has(SelfCall, f.cf_meta)) ? "" : field(f.cf_name)}))');
				}
			case TEnumIndex(x):
				gen_value(ctx, x);
				if (context.Common.defined(ctx.com, JsEnumsAsObjects)) {
					print(ctx, "._hx_index");
				}
				else {
					print(ctx, "[1]");
				}
			case TEnumParameter(x, f, i):
				gen_value(ctx, x);
				if (context.Common.defined(ctx.com, JsEnumsAsObjects)) {
					var fname = switch (f.ef_type) {
						case TFun({args:args}):
							List.nth(args, i).name;
						case _:
							trace("Shall not be seen"); std.Sys.exit(255); throw false;
					}
					print(ctx, '.${fname}');
				}
				else {
					print(ctx, '[${i+2}]');
				}
			case TField(_, FStatic({cl_path:{a:Tl, b:""}}, f)):
				spr(ctx, f.cf_name);
			case TField(x, (FInstance(_,_,f)|FStatic(_,f)|FAnon(f))) if (core.Meta.has(SelfCall, f.cf_meta)):
				gen_value(ctx, x);
			case TField(x, f):
				function skip (e:TExpr) : TExpr {
					return switch (e.eexpr) {
						case TCast(e1, None), TMeta(_, e1):
							skip(e1);
						case TConst(TInt(_)|TFloat(_)), TObjectDecl(_):
							e.with({eexpr:TParenthesis(e)});
						case _:
							e;
					}
				}
				var x = skip(x);
				gen_value(ctx, x);
				var name = core.Type.field_name(f);
				spr(ctx, switch (f) { case FStatic(c,_): static_field(c, name); case FEnum(_), FInstance(_), FAnon(_), FDynamic(_), FClosure(_): field(name); });
			case TTypeExpr(t):
				spr(ctx, ctx.type_accessor(t));
			case TParenthesis(e):
				spr(ctx, "(");
				gen_value(ctx, e);
				spr(ctx, ")");
			case TMeta({name:LoopLabel, params:[{expr:EConst(CInt(n))}]}, e):
				switch (e.eexpr) {
					case TWhile(_,_,_), TFor(_,_,_):
						print(ctx, '_hx_loop${n}: ');
						gen_expr(ctx, e);
					case TBreak:
						print(ctx, 'break _hx_loop${n}');
					case _:
						trace("Shall not be seen"); std.Sys.exit(255);
				}
			case TMeta(_, e):
				gen_expr(ctx, e);
			case TReturn(eo):
				if (ctx.in_value != None) { unsupported(e.epos); }
				switch (eo) {
					case None:
						spr(ctx, "return");
					case Some(e):
						spr(ctx, "return ");
						gen_value(ctx, e);
				}
			case TBreak:
				if (!ctx.in_loop) { unsupported(e.epos); }
				spr(ctx, "break");
			case TContinue:
				if (!ctx.in_loop) { unsupported(e.epos); }
				spr(ctx, "continue");
			case TBlock(el):
				print(ctx, "{");
				var bend = open_block(ctx);
				List.iter(gen_block_element.bind(false,ctx), el);
				bend();
				newline(ctx);
				print(ctx, "}");
			case TFunction(f):
				var old = {fst:ctx.in_value, snd:ctx.in_loop};
				ctx.in_value = None;
				ctx.in_loop = false;
				var args:ImmutableList<String> = List.map(function (tmp) {
					var v = tmp.v;
					check_var_declaration(v);
					return ident(v.v_name);
				}, f.tf_args);
				print(ctx, 'function(${List.join(",", args)}) ');
				gen_expr(ctx, fun_block(ctx, f, e.epos));
				ctx.in_value = old.fst;
				ctx.in_loop = old.snd;
				ctx.separator = true;
			case TCall(e, el):
				gen_call(ctx, e, el, false);
			case TArrayDecl(el):
				spr(ctx, "[");
				concat(ctx, ",", gen_value.bind(ctx), el);
				spr(ctx, "]");
			case TThrow(e):
				spr(ctx, "throw");
				gen_value(ctx, e);
			case TVar(v, eo):
				spr(ctx, "var");
				check_var_declaration(v);
				spr(ctx, ident(v.v_name));
				switch (eo) {
					case None:
					case Some(e):
						spr(ctx, " = ");
						gen_value(ctx, e);
				}
			case TNew({cl_path:{a:[], b:"Array"}}, _, []):
				print(ctx, "[]");
			case TNew(c, _, el):
				switch (c.cl_constructor) {
					case Some(cf) if (core.Meta.has(SelfCall, cf.cf_meta)):
					case _:
						print(ctx, "new ");
				}
				print(ctx, '${ctx.type_accessor(TClassDecl(c))}(');
				concat(ctx, ",", gen_value.bind(ctx), el);
				spr(ctx, ")");
			case TIf(cond, e, eelse):
				spr(ctx, "if");
				gen_value(ctx, cond);
				spr(ctx, " ");
				gen_expr(ctx, core.Type.mk_block(e));
				switch (eelse) {
					case None:
					case Some(e2):
						switch (e.eexpr) {
							case TObjectDecl(_): ctx.separator = false;
							case _:
						}
						semicolon(ctx);
						spr(ctx, " else ");
						gen_expr(ctx, switch (e2.eexpr) { case TIf(_,_,_): e2; case _: core.Type.mk_block(e2);});
				}
			case TUnop(op, Prefix, e):
				spr(ctx, core.Ast.s_unop(op));
				gen_value(ctx, e);
			case TUnop(op, Postfix, e):
				gen_value(ctx, e);
				spr(ctx, core.Ast.s_unop(op));
			case TWhile(cond, e, NormalWhile):
				var old_in_loop = ctx.in_loop;
				ctx.in_loop = true;
				spr(ctx, "while");
				gen_value(ctx, cond);
				spr(ctx, " ");
				gen_expr(ctx, e);
				ctx.in_loop = old_in_loop;
			case TWhile(cond, e, DoWhile):
				var old_in_loop = ctx.in_loop;
				ctx.in_loop = true;
				spr(ctx, "do ");
				gen_expr(ctx, e);
				semicolon(ctx);
				spr(ctx, "while");
				gen_value(ctx, cond);
				ctx.in_loop = old_in_loop;
			case TObjectDecl(fields):
				spr(ctx, "{");
				concat(ctx, ", ", function (field:TObjectField) {
					var f = field.name; var qs = field.quotes; var e = field.expr;
					switch (qs) {
						case DoubleQuotes:
							print(ctx, '"${core.Ast.s_escape(f)}" : ');
						case NoQuotes:
							print(ctx, '${anon_field(f)} ; ');
					}
				}, fields);
				spr(ctx, "}");
				ctx.separator = true;
			case TFor(v, it, e):
				check_var_declaration(v);
				var old_in_loop = ctx.in_loop;
				ctx.in_loop = true;
				var it = ident(switch (it.eexpr) {
					case TLocal(v): v.v_name;
					case _:
						var id = ctx.id_counter;
						ctx.id_counter++;
						var name = "$it" + id;
						print(ctx, 'var ${name} = ');
						gen_value(ctx, it);
						newline(ctx);
						name;
				});
				print(ctx, 'while ( ${it}.hasNext() ) {');
				var bend = open_block(ctx);
				newline(ctx);
				print(ctx, 'var ${ident(v.v_name)} = ${it}.next()');
				gen_block_element(ctx, e);
				bend();
				newline(ctx);
				spr(ctx, "}");
				ctx.in_loop = old_in_loop;
			case TTry(etry, [{v:v, e:ecatch}]):
				spr(ctx, "try ");
				gen_expr(ctx, etry);
				check_var_declaration(v);
				print(ctx, ' catch ( ${v.v_name} ) ');
				gen_expr(ctx, ecatch);
			case TTry(_):
				context.Common.abort("Unhandled try/catch, please report", e.epos);
			case TSwitch(e, cases, def):
				spr(ctx, "switch");
				gen_value(ctx, e);
				spr(ctx, " {");
				newline(ctx);
				List.iter(function (c) {
					var el = c.values; var e2 = c.e;
					List.iter (function (e) {
						switch (e.eexpr) {
							case TConst(c) if (c == TNull):
								spr(ctx, "case null: case undefined:");
							case _:
								spr(ctx, "case ");
								gen_value(ctx, e);
								spr(ctx, ":");
						}
					}, el);
					var bend = open_block(ctx);
					gen_block_element(ctx, e2);
					if (needs_switch_break(e2)) {
						newline(ctx);
						print(ctx, "break");
					}
					bend();
					newline(ctx);
				}, cases);
				switch(def) {
					case None:
					case Some(e):
						spr(ctx, "default:");
						var bend = open_block(ctx);
						gen_block_element(ctx, e);
						bend();
						newline(ctx);
				}
				spr(ctx, "}");
			case TCast(e, None):
				gen_expr(ctx, e);
			case TCast(e1, Some(t)):
				print(ctx, '${ctx.type_accessor(TClassDecl(core.Type.null_class.with({cl_path:{a:["js"], b:"Boot"}})))}.__cast(');
				gen_expr(ctx, e1);
				spr(ctx, " , ");
				spr(ctx, ctx.type_accessor(t));
				spr(ctx, ")");
			case TIdent(s):
				spr(ctx, s);
		}
		clear_mapping();
	}

	public static function gen_block_element (?after:Bool=false,ctx:Ctx, e:TExpr) {
		switch (e.eexpr) {
			case TBlock(el):
				List.iter(gen_block_element.bind(after, ctx), el);
			case TCall({eexpr:TIdent("__feature__")}, {eexpr:TConst(TString(f))} :: eif :: eelse):
				if (has_feature(ctx, f)) {
					gen_block_element(after, ctx, eif);
				}
				else {
					switch (eelse) {
						case []:
						case [e]: gen_block_element(after, ctx, e);
						case _: trace("Shall not be seen"); std.Sys.exit(255);
					}
				}
			case TFunction(_):
				gen_block_element(after, ctx, core.Type.mk(TParenthesis(e), e.etype, e.epos));
			case TObjectDecl(fl):
				List.iter(function (field:TObjectField) { var e = field.expr; gen_block_element(after, ctx, e); }, fl);
			case _ :
				if (!after) { newline(ctx); }
				gen_expr(ctx, e);
				if (after) { newline(ctx); }
		}
	}

	public static function gen_value (ctx:Ctx, e:TExpr) : Void {
		var clear_mapping = add_mapping(ctx, e);
		function assign (e:TExpr) : TExpr {
			return core.Type.mk(TBinop(
				OpAssign,
				core.Type.mk(TLocal(switch (ctx.in_value) {case None: trace("Shall not be seen"); std.Sys.exit(255); throw false; case Some(v): v;}), core.Type.t_dynamic, e.epos),
				e), e.etype, e.epos);
		}
		function value () : Void->Void {
			var old = {fst:ctx.in_value, snd:ctx.in_loop};
			var r = core.Type.alloc_var("$r", core.Type.t_dynamic, e.epos);
			ctx.in_value = Some(r);
			ctx.in_loop = false;
			spr(ctx, "(function($this) ");
			spr(ctx, "{");
			var b = open_block(ctx);
			newline(ctx);
			spr(ctx, "var $r");
			newline(ctx);
			return function () {
				newline(ctx);
				spr(ctx, "return $r");
				b();
				newline(ctx);
				spr(ctx, "}");
				ctx.in_value = old.fst;
				ctx.in_loop = old.snd;
				print(ctx, '(${this_(ctx)}))');
			}
		}
		switch (e.eexpr) {
			case TConst(_), TLocal(_), TArray(_,_), TBinop(_,_,_), TField(_,_), TEnumParameter(_,_), TEnumIndex(_), TTypeExpr(_), TParenthesis(_), TObjectDecl(_), TArrayDecl(_), TNew(_,_,_), TUnop(_,_,_), TFunction(_), TIdent(_):
				gen_expr(ctx, e);
			case TMeta(_, e1):
				gen_value(ctx, e1);
			case TCall(e, el):
				gen_call(ctx, e, el, true);
			case TReturn(_), TBreak, TContinue:
				unsupported(e.epos);
			case TCast(e1, None):
				gen_value(ctx, e1);
			case TCast(e1, Some(t)):
				print(ctx, '${ctx.type_accessor(TClassDecl(core.Type.null_class.with({cl_path:{a:["js"], b:"Boot"}})))}.__cast(');
				gen_value(ctx, e1);
				spr(ctx, " , ");
				spr(ctx, (ctx.type_accessor(t)));
				spr(ctx, ")");
			case TVar(_), TFor(_,_,_), TWhile(_,_,_), TThrow(_):
				// value is discarded anyway
				var v = value();
				gen_expr(ctx, e);
				v();
			case TBlock([e]):
				gen_value(ctx, e);
			case TBlock(el):
				var v = value();
				function loop (l:ImmutableList<TExpr>) {
					switch (l) {
						case []: spr(ctx, "'return null");
						case [e]: gen_expr(ctx, assign(e));
						case e :: l:
							gen_expr(ctx, e);
							newline(ctx);
							loop(l);
					}
				}
				loop(el);
				v();
			case TIf(cond, e, eo):
				// remove parenthesis unless it's an operation with higher precedence than ?:
				var cond = switch (cond.eexpr) {
					case TParenthesis({eexpr:(TBinop((OpAssign|OpAssignOp(_)),_,_) | TIf(_,_,_))}): cond;
					case TParenthesis(e): e;
					case _: cond;
				};
				gen_value(ctx, cond);
				spr(ctx, " ? ");
				gen_value(ctx, e);
				spr(ctx, " : ");
				switch (eo) {
					case None: spr(ctx, "null");
					case Some(e): gen_value(ctx, e);
				}
			case TSwitch(cond, cases, def):
				var v = value();
				gen_expr(ctx, core.Type.mk(TSwitch(cond,
					List.map(function(c) { var e1 = c.values; var e2 = c.e; return {values:e1, e:assign(e2)}; }, cases),
					switch(def) { case None: None; case Some(e): Some(assign(e)); }
				), e.etype, e.epos));
				v();
			case TTry(b, catchs):
				var v = value();
				function block(e:TExpr) {
					return core.Type.mk(TBlock([e]), e.etype, e.epos);
				}
				gen_expr(ctx, core.Type.mk(TTry(block(assign(b)),
					List.map(function(c) { var v = c.v; var e = c.e; return {v:v, e:block(assign(e))};}, catchs)
				), e.etype, e.epos));
				v();
		}
		clear_mapping();
	}

	public static function gen_syntax (ctx:Ctx, meth:String, args:ImmutableList<core.Type.TExpr>, pos:Pos) : Void {
		trace("TODO: gen_syntax");
		throw false;
	}

	public static function generate_package_create (ctx:Ctx, path:Path) : Void {
		trace("TODO: generate_package_create");
		throw false;
	}

	/* convert a.b.c to ["a"]["b"]["c"] */
	public static function path_to_brackets (path:String) : String {
		var parts = path.split(".");
		return '["${parts.join('"]["')}"]';
	}

	public static function gen_class_static_field (ctx:Ctx, c:TClass, f:TClassField) {
		switch (f.cf_expr) {
			case None, Some({eexpr:TConst(TNull)}) if (!has_feature(ctx, "Type.getClassFields")):
			case None if (!core.Type.is_physical_field(f)):
			case None:
				print(ctx, '${s_path(ctx)(c.cl_path)}${static_field(c, f.cf_name)} = null');
				newline(ctx);
			case Some(e):
				switch (e.eexpr) {
					case TFunction(_):
						var path = s_path(ctx)(c.cl_path) + static_field(c, f.cf_name);
						var dot_path = dot_path(c.cl_path) + static_field(c, f.cf_name);
						ctx.id_counter = 0;
						print(ctx, '${path} = ');
						switch (get_exposed(ctx, dot_path, f.cf_meta)) {
							case [s]: print(ctx, '$$hx_exports${path_to_brackets(s)} = ');
							case _:
						}
						gen_value(ctx, e);
						newline(ctx);
					case _:
						ctx.statics = {c:c, s:f.cf_name, e:e} :: ctx.statics;
				}
		}
	}

	public static function can_gen_class_field (ctx:Ctx, f:TClassField) : Bool {
		return switch (f) {
			case {cf_expr:(None|Some({eexpr:TConst(TNull)}))} if (!has_feature(ctx, "Type.getInstanceFields")):
				false;
			case f:
				core.Type.is_physical_field(f);
		}
	}

	public static function gen_class_field (ctx:Ctx, c:TClass, f:TClassField) : Void {
		trace("TODO: gen_class_field");
		throw false;
	}

	public static function generate_class___name__ (ctx:Ctx, c:TClass) : Void {
		if (has_feature(ctx, "js.Boot.isClass")) {
			var p = s_path(ctx)(c.cl_path);
			print(ctx, '${p}.__name__ = ');
			if (has_feature(ctx, "Type.getClassName")) {
				print(ctx, '[${List.join(",", List.map(function (s:String) {return '"${core.Ast.s_escape(s)}""';}, List.append(c.cl_path.a, [c.cl_path.b])))}]');
			}
			else {
				print(ctx, "true");
			}
			newline(ctx);
		}
	}

	public static function generate_class (ctx:Ctx, c:TClass) : Void {
		ctx.current = c;
		ctx.id_counter = 0;
		switch (c.cl_path) {
			case {a:[], b:"Function"}: context.Common.abort("This class redefine a native one", c.cl_pos);
			case _:
		}
		var p = s_path(ctx)(c.cl_path);
		var hxClasses = has_feature(ctx, "Type.resolveClass");
		if (ctx.js_flatten) {
			print(ctx, "var ");
		}
		else {
			generate_package_create(ctx, c.cl_path);
		}
		if (ctx.js_modern || !hxClasses) {
			print(ctx, '${p} = ');
		}
		else {
			print(ctx, '${p} = $$hxClasses["${dot_path(c.cl_path)}"] = ');
		}
		switch (get_exposed(ctx, dot_path(c.cl_path), c.cl_meta)) {
			case [s]:
				print(ctx, '$$hx_exports${path_to_brackets(s)} = ');
			case _:
		}
		switch (c.cl_kind) {
			case KAbstractImpl(_):
				// abstract implementations only contain static members and don't need to have constructor functions
				print(ctx, "{}");
				ctx.separator = true;
			case _:
				switch (c.cl_constructor) {
					case Some({cf_expr:Some(e)}):
						gen_expr(ctx, e);
					case _:
						print(ctx, "function() { }");
						ctx.separator = true;
				}
		}
		newline(ctx);
		if (ctx.js_modern && hxClasses) {
			print(ctx, '$$hxClasses["${dot_path(c.cl_path)}"] = ${p}');
			newline(ctx);
		}
		generate_class___name__(ctx, c);
		switch (c.cl_implements) {
			case []:
			case l:
				print(ctx, '${p}.__interfaces__ = [${List.join(",", List.map(function (tmp) { var i = tmp.c; return ctx.type_accessor(TClassDecl(i)); }, l))}]');
				newline(ctx);
		}

		function gen_props (props:ImmutableList<{fst:String, snd:String}>) {
			return List.join(",", List.map(function (tmp) { var p = tmp.fst; var v = tmp.snd; return p + ':"'+v+'"';}, props));
		}
		var has_property_reflection = has_feature(ctx, "Reflect.getProperty") || has_feature(ctx, "Reflect.setProperty");
		if (has_property_reflection) {
			switch (codegen.Codegen.get_properties(c.cl_ordered_statics)) {
				case []:
				case props:
					print(ctx, '${p}.__properties__ = ${gen_props(props)}');
					ctx.separator = true;
					newline(ctx);
			}
		}

		List.iter(gen_class_static_field.bind(ctx, c), c.cl_ordered_statics);

		var has_class = has_feature(ctx, "js.Boot.getClass") && (c.cl_super != None || c.cl_ordered_fields != Tl || c.cl_constructor != None);
		var has_prototype = c.cl_super != None || has_class || List.exists(can_gen_class_field.bind(ctx), c.cl_ordered_fields);
		if (has_prototype) {
			switch (c.cl_super) {
				case None: print(ctx, '${p}.prototype = {');
				case Some({c:csup}):
					var psup = ctx.type_accessor(TClassDecl(csup));
					print(ctx, '${p}.__super__ = ${psup}');
					newline(ctx);
					print(ctx, '${p}.prototype = $$extends(${psup}.prototype, {');
			}

			var bend = open_block(ctx);
			List.iter(function (f) {
				if (can_gen_class_field(ctx, f)) {
					gen_class_field(ctx, c, f);
				}
			}, c.cl_ordered_fields);
			if (has_class) {
				newprop(ctx);
				print(ctx, '__class__: ${p}');
			}

			if (has_property_reflection) {
				var props = codegen.Codegen.get_properties(c.cl_ordered_fields);
				switch (c.cl_super) {
					case _ if (props == Tl):
					case Some({c:csup}) if (codegen.Codegen.has_properties(csup)):
						newprop(ctx);
						var psup = ctx.type_accessor(TClassDecl(csup));
						print(ctx, '__properties__: $$extends(${psup}.prototype.__properties__, {${gen_props(props)}}');
					case _:
						newprop(ctx);
						print(ctx, '__properties__: {${gen_props(props)}}');
				}

				bend();
				print(ctx, "\n}");
				switch (c.cl_super) {
					case None: ctx.separator = true;
					case _: print(ctx, ")");
				}
				newline(ctx);
			}
			flush(ctx);
		}
	}
	public static function generate_enum (ctx:Ctx, c:TEnum) : Void {
		trace("TODO: generate_enum");
		throw false;
	}

	public static function generate_static (ctx:Ctx, tmp:{c:TClass, s:String, e:TExpr}) : Void {
		var c = tmp.c; var f = tmp.s; var e = tmp.e;
		print(ctx, '${s_path(ctx)(c.cl_path)}${static_field(c, f)} = ');
		gen_value(ctx, e);
		newline(ctx);
	}

	public static function generate_require (ctx:Ctx, path:Path, meta:Metadata) : Void {
		trace("TODO: generate_require");
		throw false;
	}

	public static function generate_type (ctx:Ctx, mt:ModuleType) : Void {
		switch (mt) {
			case TClassDecl(c):
				switch (c.cl_init) {
					case None:
					case Some(e):
						ctx.inits = e :: ctx.inits;
				}
				/* Special case, want to add Math.__name__ only when required, handle here since Math is extern */
				var p = s_path(ctx)(c.cl_path);
				if (p == "Math") { generate_class___name__(ctx, c); };
				/* Another special case for Std because we do not want to generate it if it's empty. */
				if (p == "Std" && c.cl_ordered_statics == Tl) {}
				else if (!c.cl_extern) {
					generate_class(ctx, c);
				}
				else if (core.Meta.has(JsRequire, c.cl_meta) && context.Common.is_directly_used(ctx.com, c.cl_meta)) {
					generate_require(ctx, c.cl_path, c.cl_meta);
				}
				else if (!ctx.js_flatten && core.Meta.has(InitPackage, c.cl_meta)) {
					switch (c.cl_path) {
						case {a:[]}:
						case _: generate_package_create(ctx, c.cl_path);
					}
				}
			case TEnumDecl(e) if (e.e_extern):
				if (core.Meta.has(JsRequire, e.e_meta) && context.Common.is_directly_used(ctx.com, e.e_meta)) {
					generate_require(ctx, e.e_path, e.e_meta);
				}
			case TEnumDecl(e):
				generate_enum(ctx, e);
			case TTypeDecl(_), TAbstractDecl(_):
		}
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
				var exposedObject:ObjectStore = {os_name: "", os_fields:Tl};
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
				List.iter (function (file:{fst:String, snd:String}) {
					switch (file) {
						case {fst:path, snd:"top"}:
							var file_content = sys.io.File.getBytes(path).toString();
							print(ctx, '${file_content}\n');
						case _:
					}
				}, include_files);
				var var_console = {
					fst:"console",
					snd:"typeof console != \"undefined\" ? console : {log:function(){}}"
				};

				var var_exports = {
					fst:"$hx_exports",
					snd:"typeof exports != \"undefined\" ? exports : typeof window != \"undefined\" ? window : typeof self != \"undefined\" ? self : this"
				};

				var var_global = {
					fst:"$global",
					snd:"typeof window != \"undefined\" ? window : typeof global != \"undefined\" ? global : typeof self != \"undefined\" ? self : this"
				};

				var closureArgs:ImmutableList<{fst:String, snd:String}> = [];
				closureArgs = (has_feature(ctx, "js.Lib.global")) ? var_global :: closureArgs : closureArgs;
				closureArgs = (anyExposed && !context.Common.defined(com, ShallowExpose)) ? var_exports :: closureArgs : closureArgs;
				// Provide console for environments that may not have it.
				closureArgs = (ctx.es_version < 5) ? var_console :: closureArgs : closureArgs;

				if (nodejs) {
					// Add node globals to pseudo-keywords, so they are not shadowed by local vars
					List.iter(function (s) { Hashtbl.replace(kwds2, s, true); }, ["global", "process", "__filename", "__dirname", "module"]);
				}

				if (anyExposed && (context.Common.defined(com, ShallowExpose) || !ctx.js_modern)) {
					print(ctx, 'var ${var_exports.fst} = ${var_exports.snd}');
					ctx.separator = true;
					newline(ctx);
				}

				if (ctx.js_modern) {
					// Wrap output in a closure
					print(ctx, '(function (${List.join(", ", List.map(function (c) { return c.fst; }, closureArgs))}) { "use strict"');
					newline(ctx);
				}

				function print_obj (f:ObjectStore, root:String) {
					var path = root + path_to_brackets(f.os_name);
					print(ctx, '${path} = ${path} || {}');
					ctx.separator = true;
					newline(ctx);
					concat(ctx, ";", function (g:ObjectStore) { print_obj(g, path); }, f.os_fields);
				}
				List.iter(function (f:ObjectStore) { print_obj(f, "$hx_exports"); }, exposedObject.os_fields);

				List.iter(function (file:{fst:String, snd:String}) {
					switch (file) {
						case {fst:path, snd:"closure"}:
							var file_content = sys.io.File.getBytes(path).toString();
							print(ctx, '${file_content}\n');
						case _:
					}
				}, include_files);

				// If ctx.js_modern, console is defined in closureArgs.
				if (!ctx.js_modern && ctx.es_version < 5) {
					add_feature(ctx, "js.Lib.global"); // console polyfill will check console from $global
				}

				if (!ctx.js_modern && has_feature(ctx, "js.Lib.global")) {
					print(ctx, 'var ${var_global.fst} = ${var_global.snd}:\n');
				}

				if (!ctx.js_modern && ctx.es_version < 5) {
					spr(ctx, "var console = $global.console || {log:function(){}};\n");
				}

				// TODO: fix $estr
				var vars:ImmutableList<String> = [];
				vars = (has_feature(ctx, "Type.resolveClass") || has_feature(ctx, "Type.resolveEnum")) ? ("$hxClasses = " + (ctx.js_modern ? "{}" : "$hxClasses || {}")) :: vars : vars;
				vars = (has_feature(ctx, "has_enum"))
					? ("$estr = function() { return " + (ctx.type_accessor(TClassDecl(core.Type.null_class.with({cl_path:{a:["js"], b:"Boot"}})))) + ".__string_rec(this,''); }") :: vars
					: vars;
				vars = (context.Common.defined(com, JsEnumsAsObjects)) ? "$hxEnums = {}" :: vars : vars;
				switch (List.rev(vars)) {
					case []:
					case vl:
						print(ctx, 'var ${List.join(",", vl)}');
						ctx.separator = true;
						newline(ctx);
				}

				if (List.exists(function (t:ModuleType) { return t.match(TClassDecl({cl_extern:false, cl_super:Some(_)})); }, com.types)) {
					print(ctx, "function $extend(from, fields) {
	function Inherit() {} Inherit.prototype = from; var proto = new Inherit();
	for (var name in fields) proto[name] = fields[name];
	if( fields.toString !== Object.prototype.toString ) proto.toString = fields.toString;
	return proto;
}
");
				}
				List.iter(generate_type.bind(ctx), com.types);
				function chk_features (e:TExpr) {
					if (is_dynamic_iterator(ctx, e)) {
						add_feature(ctx, "use.$iterator");
					}
					switch (e.eexpr) {
						case TField(_, FClosure(_)):
							add_feature(ctx, "use.$bind");
						case _:
							core.Type.iter(chk_features, e);
					}
				}

				List.iter(chk_features, ctx.inits);
				List.iter(function (tmp) { var e = tmp.e; chk_features(e); }, ctx.statics);
				if (has_feature(ctx, "use.$iterator")) {
					add_feature(ctx, "use.$bind");
					print(ctx, "function $iterator(o) { if( o instanceof Array ) return function() { return HxOverrides.iter(o); }; return typeof(o.iterator) == 'function' ? $bind(o,o.iterator) : o.iterator; }");
					newline(ctx);
				}
				if (has_feature(ctx, "use.$getIterator")) {
					print(ctx, "function $getIterator(o) { if( o instanceof Array ) return HxOverrides.iter(o); else return o.iterator(); }");
					newline(ctx);
				}
				if (has_feature(ctx, "use.$bind")) {
					print(ctx, "var $_, $fid = 0");
					newline(ctx);
					print(ctx, "function $bind(o,m) { if( m == null ) return null; if( m.__id__ == null ) m.__id__ = $fid++; var f; if( o.hx__closures__ == null ) o.hx__closures__ = {}; else f = o.hx__closures__[m.__id__]; if( f == null ) { f = function(){ return f.method.apply(f.scope, arguments); }; f.scope = o; f.method = m; o.hx__closures__[m.__id__] = f; } return f; }");
					newline(ctx);
				}
				if (has_feature(ctx, "use.$arrayPush")) {
					print(ctx, "function $arrayPush(x) { this.push(x); }");
					newline(ctx);
				}
				List.iter(gen_block_element.bind(true, ctx), List.rev(ctx.inits));
				List.iter(generate_static.bind(ctx), List.rev(ctx.statics));
				switch (com.main) {
					case None:
					case Some(e):
						gen_expr(ctx, e);
						newline(ctx);
				}
				if (ctx.js_modern) {
					print(ctx, '})(${List.join(", ", List.map(function (tmp) { return tmp.snd; }, closureArgs))})');
					newline(ctx);
				}

				if (anyExposed && context.Common.defined(com, ShallowExpose)) {
					List.iter(function (f) {
						print(ctx, 'var ${f.os_name} = $$hx_exports${path_to_brackets(f.os_name)}');
						ctx.separator = true;
						newline(ctx);
					}, exposedObject.os_fields);
					List.iter(function (f) {
						print(ctx, 'var ${f} = $$hx_exports${path_to_brackets(f)}');
						ctx.separator = true;
						newline(ctx);
					}, toplevelExposed.get());
				}
				switch (ctx.smap) {
					case Some(smap): write_mappings(ctx, smap);
					case None:
						try {
							sys.FileSystem.deleteFile(com.file + ".map");
						}
						catch (_:Any) {}
				}
				flush(ctx);
				ctx.chan.flush(); ctx.chan.close();
		}
	}
}