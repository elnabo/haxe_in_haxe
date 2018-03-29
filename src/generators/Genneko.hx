package generators;

import core.Path;
import core.Type.ModuleType;
import core.Type.TClass;
import core.Type.TClassField;
import core.Type.TConstant;
import core.Type.TEnum;
import core.Type.TExpr;
import core.Type.TVar;

import haxe.ds.ImmutableList;
import haxe.ds.Option;

import neko.Binast;
import neko.Nast;
import neko.Nbytecode;
import neko.Ncompile;

import ocaml.Hashtbl;
import ocaml.List;
import ocaml.PMap;
import ocaml.Ref;

typedef Context = {
	version: Int,
	com: context.Common.Context,
	packages: Hashtbl<ImmutableList<String>, Bool>,
	globals: Hashtbl<Path, String>,
	curglobal:Int,
	macros:Bool,
	curclass: String,
	curmethod: String,
	inits: ImmutableList<{c:TClass, e:TExpr}>,
	label_count:Int
}

class Genneko {

	public static final files:Hashtbl<String,String> = Hashtbl.create(0);

	public static function pos (ctx:Context, p:core.Globals.Pos) : Pos {
		return
		if (ctx.macros) {
			{psource: p.pfile, pline: p.pmin | ((p.pmax - p.pmin) << 20)};
		}
		else {
			var file:String = if (ctx.com.debug) {
				ctx.curclass + "::" + ctx.curmethod;
			}
			else {
				try {
					Hashtbl.find(files, p.pfile);
				}
				catch (_:ocaml.Not_found) {
					var path:String = if (context.Common.defined(ctx.com, AbsolutePath)) {
						sys.FileSystem.fullPath(p.pfile);
					}
					else {
						try {
							// lookup relative path
							var len = p.pfile.length;
							var base = List.find(function (path:String) {
								var l = path.length;
								return len > l && p.pfile.substr(0, l) == path;
							}, ctx.com.class_path);
							var l = base.length;
							p.pfile.substr(l, len -l);
						}
						catch (_:ocaml.Not_found) {
							p.pfile;
						}
					}
					Hashtbl.add(files, p.pfile, path);
					path;
				}
			}
			{
				psource: file,
				pline: syntax.Lexer.get_error_line(p)
			};
		}
	}

	public static function gen_global_name (ctx:Context, path:core.Path) : String {
		return switch (path) {
			case {a:[], b:name}: name;
			case _:
				try {
					Hashtbl.find(ctx.globals, path);
				}
				catch (_:ocaml.Not_found) {
					var name = "@G" + ctx.curglobal;
					ctx.curglobal = ctx.curglobal + 1;
					Hashtbl.add(ctx.globals, path, name);
					name;
				}
		}
	}

	public static function null_ (p:Pos) : Expr {
		return {decl:EConst(Null), pos:p};
	}

	public static function this_ (p:Pos) : Expr {
		return {decl:EConst(This), pos:p};
	}

	public static function int (p:Pos, n:Int) : Expr {
		return {decl:EConst(Int(n)), pos:p};
	}

	public static function str (p:Pos, s:String) : Expr {
		return {decl:EConst(String(s)), pos:p};
	}

	public static function ident (p:Pos, s:String) : Expr {
		var l = s.length;
		return
		if (l > 10 && s.substr(0,10) == "__dollar__") {
			{decl:EConst(Builtin(s.substr(10, l-10))), pos:p};
		}
		else {
			{decl:EConst(Ident(s)), pos:p};
		}
	}

	public static function field (p:Pos, e:Expr, f:String) : Expr {
		return {decl:EField(e, f), pos:p};
	}

	public static function builtin (p:Pos, n:String) : Expr {
		return {decl:EConst(Builtin(n)), pos:p};
	}

	public static function call (p:Pos, e:Expr, el:ImmutableList<Expr>) : Expr {
		return {decl:ECall(e, el), pos:p};
	}

	public static function array (p:Pos, el:ImmutableList<Expr>) : Expr {
		return call(p, builtin(p, "array"), el);
	}

	public static function pmap_list<A, B, C> (f:A->B, p:PMap<C, A>) : ImmutableList<B> {
		return PMap.fold(function (v:A, acc:ImmutableList<B>) { return f(v) :: acc; }, p, Tl);
	}

	public static function needs_return (e:Expr) : Bool {
		return switch (e) {
			case {decl:EBlock(l)}:
				function loop (l:ImmutableList<Expr>) : Bool {
					return switch (l) {
						case []: true;
						case [x]: needs_return(x);
						case _ :: l: loop(l);
					}
				}
				loop(l);
			case {decl:EReturn(_)}:
				false;
			case _:
				true;
		}
	}

	public static function with_return (e:Expr) : Expr {
		return
		if (needs_return(e)) {
			var p = e.pos;
			var ret = {decl:EReturn(Some(null_(p))), pos:p};
			switch (e) {
				case {decl:EBlock(l)}:
					{decl:EBlock(List.append(l, [ret])), pos:p};
				case _:
					{decl:EBlock([e, ret]), pos:p};
			}
		}
		else {
			e;
		}
	}

	public static function gen_type_path (p:Pos, tmp:Path) : Expr {
		var path = tmp.a; var t = tmp.b;
		return switch (path) {
			case []: ident(p, t);
			case path :: l:
				var epath = List.fold_left(function (e, path) { return field(p, e, path); }, ident(p, path), l);
				field(p, epath, t);
		}
	}

	public static function gen_big_string (ctx:Context, p:Pos, s:String) : Expr {
		var max = (1 << 16) - 1;
		return
		if (s.length > max) {
			{decl:EBinop("+", str(p, s.substr(0, max)), gen_big_string(ctx, p, s.substr(max, s.length - max))), pos:p};
		}
		else {
			str(p, s);
		}
	}

	public static function gen_constant (ctx:Context, pe:core.Globals.Pos, c:TConstant)  : Expr {
		var p = pos(ctx, pe);
		return switch (c) {
			case TInt(i):
				try {
					var h = i >> 24;
					if (((h & 128) == 0) != ((h & 64) == 0)) { throw ocaml.Exit.instance; }
					int(p, i);
				}
				catch (_:Any) {
					if (ctx.version < 2) {
						context.Common.abort("This integer is too big to be compiled to a Neko 31-bit integer. Please use a Float instead", pe);
					}
					{decl:EConst(Int32(i)), pos:p};
				}
			case TFloat(f): {decl:EConst(Float(f)), pos:p};
			case TString(s): call(p, field(p, ident(p, "String"), "new"), [gen_big_string(ctx, p, s)]);
			case TBool(b): {decl:EConst((b) ? True : False), pos:p};
			case TNull: null_(p);
			case TThis: this_(p);
			case TSuper: trace("Shall not be seen"); std.Sys.exit(255); throw false;
		}
	}

	public static function gen_binop (ctx:Context, p:Pos, op:core.Ast.Binop, e1:TExpr, e2:TExpr) : Expr {
		return {decl:EBinop(core.Ast.s_binop(op), gen_expr(ctx, e1), gen_expr(ctx, e2)), pos:p};
	}

	public static function gen_unop (ctx:Context, p:Pos, op:core.Ast.Unop, flag:core.Ast.UnopFlag, e:TExpr) : Expr {
		return switch (op) {
			case OpIncrement: {decl:EBinop((flag == Prefix) ? "+=" : "++=", gen_expr(ctx, e), int(p, 1)), pos:p};
			case OpDecrement: {decl:EBinop((flag == Prefix) ? "-=" : "--=", gen_expr(ctx, e), int(p, 1)), pos:p};
			case OpNot: call(p, builtin(p, "not"), [gen_expr(ctx, e)]);
			case OpNeg: {decl:EBinop("-", int(p, 0), gen_expr(ctx, e)), pos:p};
			case OpNegBits: {decl:EBinop("-", int(p, -1), gen_expr(ctx, e)), pos:p};
		}
	}

	public static function gen_call (ctx:Context, p:Pos, e:TExpr, el:ImmutableList<TExpr>) : Expr {
		return switch [e.eexpr, el] {
			case [TConst(TSuper), _]:
				var c = switch (core.Type.follow(e.etype)) { case TInst(c,_): c; case _: trace("Shall not be seen"); std.Sys.exit(255); throw false; }
				call(p, builtin(p, "call"), [
					field(p, gen_type_path(p, c.cl_path), "__construct__"),
					this_(p),
					array(p, List.map(gen_expr.bind(ctx), el))
				]);
			case [TIdent("__resources__"), []]:
				call(p, builtin(p, "array"), Hashtbl.fold(function (name:String, data:String, acc:ImmutableList<Expr>) {
					return {decl:EObject([{s:"name", e:gen_constant(ctx, e.epos, TString(name))}, {s:"data", e:gen_big_string(ctx, p, data)}]), pos:p} :: acc;
				}, ctx.com.resources, []));
			case [TField({eexpr:TConst(TSuper), etype:t}, f), _]:
				var c = switch (core.Type.follow(t)) { case TInst(c,_): c; case _: trace("Shall not be seen"); std.Sys.exit(255); throw false; }
				call(p, builtin(p, "call"), [
					field(p, gen_type_path(p, new Path(c.cl_path.a, "@"+c.cl_path.b)), core.Type.field_name(f)),
					this_(p),
					array(p, List.map(gen_expr.bind(ctx), el))
				]);
			case [_,_]:
				var e = switch (gen_expr(ctx, e)) {
					case e={decl:EFunction(_,_)}:
						{decl:EBlock([e]), pos:p};
					case e:
						e;
				}
				call(p, e, List.map(gen_expr.bind(ctx), el));
		}
	}

	public static function gen_expr (ctx:Context, e:TExpr) : Expr {
		var p = pos(ctx, e.epos);
		return
		switch (e.eexpr) {
			case TConst(c):
				gen_constant(ctx, e.epos, c);
			case TIdent(s) if (s.charAt(0) == "$"):
				{decl:EConst(Builtin(s.substr(1, s.length - 1))), pos:p};
			case TLocal(v):
				if (v.v_capture) {
					{decl:EArray(ident(p, v.v_name), int(p, 0)), pos:p};
				}
				else {
					ident(p, v.v_name);
				}
			case TArray (e1, e2):
				{decl:EArray(gen_expr(ctx, e1), gen_expr(ctx, e2)), pos:p};
			case TBinop(OpAssign, {eexpr:TField(e1, f)}, e2):
				{decl:EBinop("=", field(p, gen_expr(ctx, e1), core.Type.field_name(f)), gen_expr(ctx, e2)), pos:p};
			case TBinop(op, e1, e2):
				gen_binop(ctx, p, op, e1, e2);
			case TField(e2, FClosure(_,f)):
				switch (core.Type.follow(e.etype)) {
					case TFun({args:args}):
						var n = List.length(args);
						if (n > 5) {
							context.Common.abort("Cannot create closure with more than 5 arguments", e.epos);
						}
						var tmp = ident(p, "@tmp");
						{
							decl: EBlock([
								{decl:EVars([{name:"@tmp", def:Some(gen_expr(ctx, e2))}, {name:"@fun", def:Some(field(p, tmp, f.cf_name))}]), pos:p},
								(ctx.macros) ? call(p, builtin(p, "closure"), [ident(p, "@fun"), tmp]) : call(p, ident(p, "@closure"+n), [tmp, ident(p, "@fun")])
							]),
							pos: p
						};
					case _:
						trace("Shall not be seen"); std.Sys.exit(255); throw false;
				}
			case TEnumParameter(e, _, i):
				{decl:EArray(field(p, gen_expr(ctx, e), "args"), int(p, i)), pos:p};
			case TEnumIndex(e):
				field(p, gen_expr(ctx, e), "index");
			case TField(e, f):
				field(p, gen_expr(ctx, e), core.Type.field_name(f));
			case TTypeExpr(t):
				gen_type_path(p, core.Type.t_path(t));
			case TParenthesis(e):
				{decl:EParenthesis(gen_expr(ctx, e)), pos:p};
			case TMeta(_, e):
				gen_expr(ctx, e);
			case TObjectDecl(fl):
				var hasToString = new Ref(false);
				var fl = List.map(function (o:core.Type.TObjectField) : {s:String, e:Expr} { var f = o.name; var e = o.expr; if (f == "toString") { hasToString.set(core.Type.follow(e.etype).match(TFun({args:Tl, ret:_})));} return {s:f, e: gen_expr(ctx, e)}; }, fl);
				{decl:EObject( (hasToString.get()) ? ({s:"__string", e:ident(p, "@default__string")} :: fl) : fl), pos:p}
			case TArrayDecl(el):
				call(p, field(p, ident(p, "Array"), "new1"), [array(p, List.map(gen_expr.bind(ctx), el)), int(p, List.length(el))]);
			case TCall(e, el):
				gen_call(ctx, p, e, el);
			case TNew(c,_,params):
				call(p, field(p, gen_type_path(p, c.cl_path), "new"), List.map(gen_expr.bind(ctx), params));
			case TUnop(op, flag, e):
				gen_unop(ctx, p, op, flag, e);
			case TVar(v, eo):
				{
					decl: EVars(
						{
							var e = switch (eo) {
								case None:
									(v.v_capture) ? Some(call(p, builtin(p, "array"), [null_(p)])) : None;
								case Some(e):
									var e = gen_expr(ctx, e);
									(v.v_capture) ? Some(call(p, builtin(p, "array"), [e])) : Some(e);
							}
							[{name:v.v_name, def:e}];
						}
					),
					pos: p
				};
			case TFunction(f):
				var inits = List.fold_left (function (acc, tmp) {
					var a = tmp.v; var c = tmp.c;
					var acc = (a.v_capture) ? ({decl:EBinop("=", ident(p, a.v_name), call(p, builtin(p, "array"), [ident(p, a.v_name)])), pos:p} :: acc) : acc;
					return switch (c) {
						case None, Some(TNull): acc;
						case Some(c): gen_expr(ctx, core.Texpr.set_default(ctx.com.basic, a, c, e.epos)) :: acc;
					}
				}, Tl, f.tf_args);
				var e = gen_expr(ctx, f.tf_expr);
				var e = switch (inits) {case []: e; case _: {decl:EBlock(List.rev(e::inits)), pos:p}};
				{decl:EFunction(List.map(core.Type.arg_name, f.tf_args), with_return(e)), pos:p};
			case TBlock(el):
				{decl:EBlock(List.map(gen_expr.bind(ctx), el)), pos:p};
			case TFor(v, it, e):
				var it = gen_expr(ctx, it);
				var e = gen_expr(ctx, e);
				var next = call(p, field(p, ident(p, "@tmp"), "next"), []);
				var next = (v.v_capture) ? call(p, builtin(p, "array"), [next]) : next;
				{
					decl:EBlock([
						{decl:EVars([{name:"@tmp", def:Some(it)}]), pos:p},
						{decl:EWhile(
							call(p, field(p, ident(p, "@tmp"), "hasNext"), []),
							{decl:EBlock([
								{decl:EVars([{name:v.v_name, def:Some(next)}]), pos:p},
								e
							]), pos:p},
							NormalWhile
						), pos:p}
					]),
					pos:p
				};
			case TIf(cond, e1, e2):
				/* if(e)-1 is parsed as if ( e - 1 ) */
				function parent (e:TExpr) { return core.Type.mk(TParenthesis(e), e.etype, e.epos); }
				var e1 = switch (e1.eexpr) {
					case TConst(TInt(n)) if (n < 0): parent(e1);
					case TConst(TFloat(f)) if (f.charAt(0) == "-"): parent(e1);
					case _: e1;
				}
				{decl:EIf(gen_expr(ctx, cond), gen_expr(ctx, e1), switch (e2) { case None: None; case Some(e): Some(gen_expr(ctx, e)); }), pos:p};
			case TWhile(econd, e, flag):
				{decl:EWhile(gen_expr(ctx, econd), gen_expr(ctx, e), switch (flag) { case NormalWhile: NormalWhile; case DoWhile: DoWhile;}), pos:p};
			case TTry(e, catchs):
				function loop (catchs:ImmutableList<{v:TVar, e:TExpr}>) : Expr {
					return switch (catchs) {
						case []: call(p, builtin(p, "rethrow"), [ident(p, "@tmp")]);
						case {v:v, e:e} :: l:
							var e2 = loop(l);
							var path = switch (core.Type.follow(v.v_type)) {
								case TInst(c, _): Some(c.cl_path);
								case TEnum(e, _): Some(e.e_path);
								case TAbstract(a, _): Some(a.a_path);
								case TDynamic(_): None;
								case _: trace("Shall not be seen"); std.Sys.exit(255); throw false;
							}
							var cond = switch (path) {
								case None: {decl:EConst(True), pos:p};
								case Some(path): call(p, field(p, gen_type_path(p, new Path(["neko"], "Boot")), "__instanceof"), [ident(p, "@tmp"), gen_type_path(p, path)]);
							}
							var id = ident(p, "@tmp");
							id = (v.v_capture) ? call(p, builtin(p, "array"), [id]) : id;
							var e = gen_expr(ctx, e);
							{decl:EIf(cond, {decl:EBlock([
								{decl:EVars([{name:v.v_name, def:Some(id)}]), pos:p},
								e
							]), pos:p}, Some(e2)), pos:p};
					}
				}
				var catchs = loop(catchs);
				catchs = {decl:EBlock([
					{decl:EIf(
						{decl:EBinop("==", call(p, builtin(p, "typeof"), [ident(p, "@tmp")]), builtin(p, "tstring")), pos:p},
						{decl:EBinop("=", ident(p, "@tmp"), call(p, field(p, ident(p, "String"), "new"), [ident(p, "@tmp")])), pos:p},
						None
					), pos:p},
					catchs
				]), pos:p};
				{decl:ETry(gen_expr(ctx, e), "@tmp", catchs), pos:p};
			case TReturn(eo):
				{decl:EReturn(switch (eo) { case None: Some(null_(p)); case Some(e): Some(gen_expr(ctx, e)); }), pos:p};
			case TBreak:
				{decl:EBreak(None), pos:p};
			case TContinue:
				{decl:EContinue, pos:p};
			case TThrow(e):
				call(p, builtin(p, "throw"), [gen_expr(ctx, e)]);
			case TCast(e, None):
				gen_expr(ctx, e);
			case TCast(e1, Some(t)):
				gen_expr(ctx, codegen.Codegen.default_cast("@tmp", ctx.com, e1, t, e.etype, e.epos));
			case TIdent(s):
				ident(p, s);
			case TSwitch(e, cases, eo):
				var e = gen_expr(ctx, e);
				var eo = switch (eo) { case None: None; case Some(e): Some(gen_expr(ctx, e)); }
				try {
					{decl:ESwitch(
						e,
						List.map(function (tmp) {
							var el = tmp.values; var e2 = tmp.e;
							return switch (List.map(gen_expr.bind(ctx), el)) {
								case []: trace("Shall not be seen"); std.Sys.exit(255); throw false;
								case [e]: {e1:e, e2:gen_expr(ctx, e2)};
								case _: throw ocaml.Exit.instance;
							}
						}, cases),
						eo
					), pos:p}
				}
				catch (_:ocaml.Exit) {
					{decl:EBlock([
						{decl:EVars([{name:"@tmp", def:Some(e)}]), pos:p},
						List.fold_left(function (acc:Expr, tmp:{values:ImmutableList<TExpr>, e:TExpr}) {
							var el = tmp.values; var e = tmp.e;
							var cond = switch (el) {
								case []: trace("Shall not be seen"); std.Sys.exit(255); throw false;
								case e :: l:
									function eq(e:TExpr) { return {decl:EBinop("==", ident(p, "@tmp"), gen_expr(ctx, e)), pos:p}; }
									List.fold_left(function (acc, e) { return {decl:EBinop("||", acc, eq(e)), pos:p} ; }, eq(e), l);
							}
							return {decl:EIf(cond, gen_expr(ctx, e), Some(acc)), pos:p};
						}, switch (eo) {case None: null_(p); case Some(e): e;}, List.rev(cases))
					]), pos:p};
				}
		}
	}

	public static function gen_method (ctx:Context, p:Pos, c:TClassField, acc:ImmutableList<EObjectElement>) : ImmutableList<EObjectElement> {
		ctx.curmethod = c.cf_name;
		return
		if (!core.Type.is_physical_field(c)) {
			acc;
		}
		else {
			switch (c.cf_expr) {
				case None:
					{s:c.cf_name, e:null_(p)} :: acc;
				case Some(e):
					switch (e.eexpr) {
						case TCall({eexpr:TField(_, FStatic({cl_path:{a:["neko"], b:"Lib"}}, {cf_name:(load=("load"|"loadLazy"))}))}, [{eexpr:TConst(TString(m))}, {eexpr:TConst(TString(f))}, {eexpr:TConst(TInt(n))}]):
							var p = pos(ctx, e.epos);
							var e = call(p, {decl:EField(builtin(p, "loader"), "loadprim"), pos:p}, [{decl:EBinop("+", {decl:EBinop("+", str(p, m), str(p, "@")), pos:p}, str(p, f)), pos:p}, {decl:EConst(Int(n)), pos:p}]);
							e = (load == "load") ? e : {decl:ETry(e, "@e", call(p, ident(p, "@lazy_error"), [ident(p, "@e")])), pos:p};
							{s:c.cf_name, e:e} :: acc;
						case TFunction(_):
							{s:(c.cf_name == "new") ? "new" : "__construct__", e:gen_expr(ctx, e)} :: acc;
						case _:
							{s:c.cf_name, e:null_(p)} :: acc;
					}
			}
		}
	}

	public static function gen_class (ctx:Context, c:TClass) : Expr {
		ctx.curclass = core.Globals.s_type_path(c.cl_path);
		ctx.curmethod = "$init";
		var p = pos(ctx, c.cl_pos);
		var clpath = gen_type_path(p, new Path(c.cl_path.a, "@"+c.cl_path.b));
		var stpath = gen_type_path(p, c.cl_path);
		var fnew:ImmutableList<EObjectElement> = switch (c.cl_constructor) {
			case Some(f):
				switch (f.cf_expr) {
					case Some({eexpr:TFunction(tf)}):
						var params = List.map(function (tmp) : String { var v = tmp.v; return v.v_name; }, tf.tf_args);
						gen_method(ctx, p, f, [{s:"new", e:{
							decl:EFunction(params, {
								decl:EBlock([
									{decl:EVars([{name:"@o", def:Some(call(p, builtin(p, "new"), [null_(p)]))}]), pos:p},
									call(p, builtin(p, "objsetproto"), [ident(p, "@o"), clpath]),
									call(p, builtin(p, "call"), [field(p, this_(p), "__construct__"), ident(p, "@o"), array(p, List.map(ident.bind(p), params))]),
									{decl:EReturn(Some(ident(p, "@o"))), pos:p}
								]),
								pos:p
							}),
							pos:p
						}}]);
					case _: [];
				}
			case None:
				[];
		}
		var fstring:ImmutableList<EObjectElement> = try {
			var f = PMap.find("toString", c.cl_fields);
			switch (core.Type.follow(f.cf_type)) {
				case TFun({args:[]}):
					[{s:"__string", e:ident(p, "@default_string")}];
				case _:
					[];
			}
		}
		catch (_:ocaml.Not_found) {
			Tl;
		}
		var fserialize = {s:"__serialize", e:ident(p, "@serialize")};
		var others = List.append(switch (c.cl_implements) {
			case []: [];
			case l: [{s:"__interfaces__", e:array(p, List.map(function (tmp) { var c = tmp.c; return gen_type_path(p, c.cl_path); }, l))}];
		}, switch (c.cl_super) {
			case None: [];
			case Some({c:c}): [{s:"__super__", e:gen_type_path(p, c.cl_path)}];
		});
		function build (tmp:EObjectElement) : Expr {
			var f = tmp.s; var e = tmp.e;
			return {decl:EBinop('=', field(p, ident(p, "@tmp"), f), e), pos:p};
		}
		var tmp:Expr = {decl:EVars([{name:"@tmp", def:Some(call(p, builtin(p, "new"), [null_(p)]))}]), pos:p};
		var estat:Expr = {decl:EBinop("=", stpath, ident(p, "@tmp")), pos:p};
		function gen_props (props:ImmutableList<{fst:String, snd:String}>) : Expr {
			return {decl:EObject(List.map(function (tmp) : EObjectElement { var n = tmp.fst; var s = tmp.snd; return {s:n, e:str(p, s)}; }, props)), pos:p};
		}
		var sprops = switch (codegen.Codegen.get_properties(c.cl_ordered_statics)) {
			case []: [];
			case l: [{s:"__properties__", e:gen_props(l)}];
		}
		var sfields = List.map(build,
			List.append({s:"prototype", e:clpath} :: sprops, PMap.fold(gen_method.bind(ctx, p), c.cl_statics, List.append(fnew, others)))
		);
		var eclass:Expr = {decl:EBinop("=", clpath, ident(p, "@tmp")), pos:p};
		var mfields = List.map(build, PMap.fold(gen_method.bind(ctx, p), c.cl_fields, fserialize :: fstring));
		var props = codegen.Codegen.get_properties(c.cl_ordered_fields);
		var emeta:ImmutableList<Expr> = {decl:EBinop("=", field(p, clpath, "__class__"), stpath), pos:p} ::
			switch (props) {
				case []: [];
				case _:
					var props = gen_props(props);
					props = switch (c.cl_super) {
						case Some({c:csup}) if (codegen.Codegen.has_properties(csup)):
							{
								decl: EBlock([
									{decl:EVars([{name:"@tmp", def:Some(props)}]), pos:p},
									call(p, builtin(p, "objsetproto"), [ident(p, "@tmp"), field(p, field(p, gen_type_path(p, csup.cl_path), "prototype"), "__properties__")]),
									ident(p, "@tmp")
								]),
								pos:p
							};
						case _: props;
					}
					[{decl:EBinop("=", field(p, clpath, "__properties__"), props), pos:p}];
			}
		emeta = List.append(emeta, switch (c.cl_path) {
			case {a:[], b:name}: [{decl:EBinop('=', field(p, ident(p, "@classes"), name), ident(p, name)), pos:p}];
			case _: [];
		});
		emeta = (ctx.macros) ? {decl:EBinop("=", field(p, stpath, "__ct__"), call(p, builtin(p, "typewrap"), [cast TClassDecl(c)])), pos:p} :: emeta : emeta;
		var eextends:ImmutableList<Expr> = switch (c.cl_super) {
			case None: [];
			case Some({c:c}):
				var esuper = gen_type_path(p, new Path(c.cl_path.a, "@"+c.cl_path.b));
				[call(p, builtin(p, "objsetproto"), [clpath, esuper])];
		}
		return {decl:EBlock(List.append(tmp :: eclass :: mfields, List.append(tmp :: estat :: sfields, List.append(eextends, emeta)))), pos:p};
	}

	public static function gen_enum_constr (ctx:Context, path:Expr, c)  : Expr {
		ctx.curmethod = c.ef_name;
		var p = pos(ctx, c.ef_pos);
		return {
			decl:EBinop("=", field(p, path, c.ef_name), switch(core.Type.follow(c.ef_type)) {
				case TFun({args:params}):
					var params = List.map(function (tmp) : String { var n = tmp.name; return n; }, params);
					{
						decl:EFunction(params, {
							decl:EBlock([
								{decl:EVars([
									{name:"@tmp", def:Some(
										{decl:EObject([
											{s:"tag", e:str(p, c.ef_name)},
											{s:"index", e:int(p, c.ef_index)},
											{s:"args", e:array(p, List.map(ident.bind(p), params))}
										]), pos:p}
									)}
								]), pos:p},
								call(p, builtin(p, "objsetproto"), [ident(p, "@tmp"), field(p, path, "prototype")]),
								ident(p, "@tmp")
							]),
							pos:p
						}),
						pos:p
					};
				case _:
					{
						decl:EBlock([
							{decl:EVars([{name:"@tmp", def:Some({decl:EObject([{s:"tag", e:str(p, c.ef_name)}, {s:"index", e:int(p, c.ef_index)}, {s:"__serialize", e:ident(p, "@tag_serialize")}]), pos:p})}]), pos:p},
							call(p, builtin(p, "objsetproto"), [ident(p, "@tmp"), field(p, path, "prototype")]),
							ident(p, "@tmp")
						]),
						pos:p
					}
			}),
			pos:p
		}
	}

	public static function gen_enum (ctx:Context, e:TEnum) : Expr {
		ctx.curclass = core.Globals.s_type_path(e.e_path);
		ctx.curmethod = "$init";
		var p = pos(ctx, e.e_pos);
		var path = gen_type_path(p, e.e_path);
		var uname:Expr = {decl:EConst(Ident(gen_global_name(ctx, e.e_path))), pos:p};
		return {
			decl: EBlock(
				{decl:EBinop("=", uname, call(p, builtin(p, "new"), [null_(p)])), pos:p} ::
				{decl:EBinop("=", path, uname), pos:p} ::
				{decl:EBinop("=", field(p, uname, "prototype"), {
					decl:EObject([
						{s:"__enum__", e:uname},
						{s:"__serialize", e:ident(p, "@serialize")},
						{s:"__string", e:ident(p, "@enum_to_string")}
					]),	pos:p
				}), pos:p} ::
				List.append(
					pmap_list(gen_enum_constr.bind(ctx, uname), e.e_constrs),
					switch (e.e_path) {
						case {a:[], b:name}: [{decl:EBinop("=", field(p, ident(p, "@classes"), name), ident(p, name)), pos:p}];
						case _: [];
					}
				)
			),
			pos:p
		};
	}

	public static function gen_type (ctx:Context, t:ModuleType, acc) {
		return switch (t) {
			case TClassDecl(c):
				switch (c.cl_init) {
					case None:
					case Some(e): ctx.inits = {c:c, e:e} :: ctx.inits;
				}
				if (c.cl_extern) {
					acc;
				}
				else {
					gen_class(ctx, c) :: acc;
				}
			case TEnumDecl(e):
				if (e.e_extern) {
					acc;
				}
				else {
					gen_enum(ctx, e) :: acc;
				}
			case TTypeDecl(_), TAbstractDecl(_):
				acc;
		}
	}

	public static function gen_static_vars (ctx:Context, t:ModuleType) : ImmutableList<Expr> {
		return switch (t) {
			case TEnumDecl(_), TTypeDecl(_), TAbstractDecl(_): [];
			case TClassDecl(c):
				if (c.cl_extern) {
					[];
				}
				else {
					List.fold_right(function (f:TClassField, acc:ImmutableList<Expr>) {
						return switch (f.cf_expr) {
							case None: acc;
							case Some(e):
								switch (e.eexpr) {
									case TFunction(_): acc;
									case _:
										ctx.curclass = core.Globals.s_type_path(c.cl_path);
										ctx.curmethod = "$statics";
										var p = pos(ctx, e.epos);
										{
											decl:EBinop("=", field(p, gen_type_path(p, c.cl_path), f.cf_name), gen_expr(ctx, e)),
											pos:p
										} :: acc;
								}
						}
					}, c.cl_ordered_statics, []);
				}
		}
	}

	public static function gen_package (ctx:Context, t:ModuleType) : ImmutableList<Expr> {
		function loop (acc:ImmutableList<String>, p:ImmutableList<String>) : ImmutableList<Expr> {
			return switch (p) {
				case []: [];
				case x :: l:
					var path = List.append(acc, [x]);
					if (!Hashtbl.mem(ctx.packages, path)) {
						var p = pos(ctx, core.Type.t_infos(t).mt_pos);
						var e = {decl:EBinop("=", gen_type_path(p, new Path(acc, x)), call(p, builtin(p, "new"), [null_(p)])), pos:p};
						Hashtbl.add(ctx.packages, path, true);
						switch (acc) {
							case []:
								var reg = {decl:EBinop("=", field(p, ident(p, "@classes"), x), ident(p, x)), pos:p};
								e :: reg :: loop(path, l);
							case _:
								e :: loop(path, l);
						}
					}
					else {
						loop(path, l);
					}
			}
		}
		return loop([], core.Type.t_path(t).a);
	}

	public static function gen_boot (ctx:Context) : Expr {
		var null_pos = Nast.null_pos;
		return {
			decl:EBlock([
				{decl:EBinop("=", field(null_pos, gen_type_path(null_pos, new Path(["neko"], "Boot")), "__classes"), ident(null_pos, "@classes")), pos:null_pos},
				call(null_pos, field(null_pos, gen_type_path(null_pos, new Path(["neko"], "Boot")), "__init"), [])
			]),
			pos:null_pos
		};
	}

	public static function gen_name (ctx:Context, acc:ImmutableList<Expr>, t:ModuleType) : ImmutableList<Expr> {
		return switch (t) {
			case TEnumDecl(e) if (e.e_extern):
				acc;
			case TEnumDecl(e):
				var p = pos(ctx, e.e_pos);
				var name = List.append(e.e_path.a, [e.e_path.b]);
				var arr = call(p, field(p, ident(p, "Array"), "new1"), [array(p, List.map(function (n:String) { return gen_constant(ctx, e.e_pos, TString(n)); }, name)), int(p, List.length(name))]);
				var path = gen_type_path(p, e.e_path);
				var setname = {decl:EBinop("=", field(p, path, "__ename__"), arr), pos:p};
				var arr = call(p, field(p, ident(p, "Array"), "new1"), [array(p, List.map(function (n:String) { return gen_constant(ctx, e.e_pos, TString(n)); }, e.e_names)), int(p, List.length(e.e_names))]);
				var setconstrs = {decl:EBinop("=", field(p, path, "__constructs__"), arr), pos:p};
				var meta:ImmutableList<Expr> = switch (core.Texpr.build_metadata(ctx.com.basic, TEnumDecl(e))) {
					case None: [];
					case Some(e): [{decl:EBinop("=", field(p, path, "__meta__"), gen_expr(ctx, e)), pos:p}];
				}
				meta = if (ctx.macros) {
					trace("TODO: figure how Obj.magic(t) cast core.Type.ModuleType to neko.Nast.Expr");
					{decl:EBinop("=", field(p, path, "__et__"), call(p, builtin(p, "typewrap"), [cast t])), pos:p} :: meta;
				}
				else {
					meta;
				}
				List.append(setname :: setconstrs :: meta, acc);
			case TClassDecl (c):
				if (c.cl_extern || c.cl_kind.match(KTypeParameter(_))) {
					acc;
				}
				else {
					var p = pos(ctx, c.cl_pos);
					var name = List.append(c.cl_path.a, [c.cl_path.b]);
					var arr = call(p, field(p, ident(p, "Array"), "new1"), [array(p, List.map(function (n:String) { return gen_constant(ctx, c.cl_pos, TString(n)); }, name)), int(p, List.length(name))]);
					{decl:EBinop("=", field(p, gen_type_path(p, c.cl_path), "__name__"), arr), pos:p} :: switch (c.cl_implements) {
						case []: acc;
						case l:
							var interf = field(p, gen_type_path(p, c.cl_path), "__interfaces__");
							{decl:EBinop("=", interf, call(p, field(p, ident(p, "Array"), "new1"), [interf, int(p, List.length(l))])), pos:p} :: acc;
					}
				}
			case TTypeDecl(_), TAbstractDecl(_):
				acc;
		}
	}

	public static function generate_libs_inits (libs:ImmutableList<String>) : ImmutableList<Expr> {
		return switch (libs) {
			case []: [];
			case libs:
				var p = Nast.null_pos;
				var es = ident(p, "@s");
				function loadp (n:String, nargs:Int) : Expr {
					return call(p, field(p, builtin(p, "loader"), "loadprim"), [str(p, "std@"+n), int(p, nargs)]);
				}
				function op (o:String, e1:Expr, e2:Expr) : Expr {
					return {decl:EBinop(o, e1, e2), pos:p};
				}
				var boot = [
					{decl:EVars([
						{name:"@s", def:Some(call(p, loadp("sys_string", 0), Tl))},
						{name:"@env", def:Some(loadp("get_env", 1))},
						{name:"@b", def:Some({decl:EIf(
							op("==", es, str(p, "Windows")),
							op("+", call(p, ident(p, "@env"), [str(p, "HAXEPATH")]), (str(p, "\\lib\\"))),
							Some({decl:ETry(
									op("+", call(p, loadp("file_contents", 1), [op("+", call(p, ident(p, "@env"), [str(p, "Home")]), str(p, "/.haxelib"))]), str(p, "/")),
									"e",
									{decl:EIf(
										op("==", es, str(p, "Linux")),
										{decl:EIf(
											call(p, loadp("sys_exists", 1), [str(p, "/usr/lib/haxe/lib")]),
											str(p, "/usr/lib/haxe/lib"),
											Some(str(p, "/usr/share/haxe/lib/"))
										), pos:p},
										Some(str(p, "/usr/local/lib/haxe/lib/"))
									),pos:p}
								), pos:p})
							), pos:p})}
					]), pos:p}
				];
				var lpath = field(p, builtin(p, "loader"), "path");
				return List.append(boot, List.map(function (dir:String) : Expr {
					var full_path = dir.charAt(0) == "/" || dir.charAt(1) == ":";
					var dstr = str(p, dir);
					return op("=", lpath, call(p, builtin(p, "array"), [op("+", (full_path) ? dstr : op("+", ident(p, "@b"), dstr), ident(p, "@s")), lpath]));
				}, libs));
		}
	}

	public static function new_context (com:context.Common.Context, ver:Int, macros:Bool) : Context {
		return {
			version: ver,
			com: com,
			globals: Hashtbl.create(0),
			curglobal: 0,
			packages: Hashtbl.create(0),
			macros: macros,
			curclass: "$boot",
			curmethod: "$init",
			inits: [],
			label_count: 0
		};
	}

	public static function header () : ImmutableList<Expr> {
		var p:Pos = {psource: "<header>", pline:1};
		function fields (l:ImmutableList<String>) : Expr {
			function loop (l:ImmutableList<String>) : Expr {
				return switch (l) {
					case []: trace("Shall not be seen"); std.Sys.exit(255); throw false;
					case [x]: ident(p, x);
					case x :: l: field(p, loop(l), x);
				}
			}
			return loop(List.rev(l));
		}
		function func (pl:ImmutableList<String>, e:Expr) : Expr {
			return {decl:EFunction(pl, {decl:EReturn(Some(e)), pos:p}), pos:p};
		}

		var inits = [
			{s:"@classes", e:call(p, builtin(p, "new"), [null_(p)])},
			{s:"@enum_to_string", e:func([], call(p, fields(["neko", "Boot", "__enum_str"]), [this_(p)]))},
			{s:"@serialize", e:func([], call(p, fields(["neko", "Boot", "__serialize"]), [this_(p)]))},
			{s:"@tag_serialize", e:func([], call(p, fields(["neko", "Boot", "__tagserialize"]), [this_(p)]))},
			{s:"@lazy_error", e:func(["e"], call(p, builtin(p, "varargs"), [func(["_"], call(p, builtin(p, "throw"), [ident(p, "e")]))]))},
			{s:"@default__string", e: func([], {
				decl:EBlock([
					{decl:EVars([{name:"@s", def:Some(call(p, field(p, this_(p), "toString"), []))}]), pos:p},
					{decl:EIf(
						{decl:EBinop("!=", call(p, builtin(p, "typeof"), [ident(p, "@s")]), builtin(p, "tobject")), pos:p},
						{decl:EReturn(Some(null_(p))), pos:p},
						None
					), pos:p},
					{decl:EReturn(Some(field(p, ident(p, "@s"), "__s"))), pos:p}
				]),
				pos:p
			})}
		];
		var inits = List.append(inits, List.map(function (nargs:Int) {
			var args:ImmutableList<String> = [ for (i in 0...nargs) std.String.fromCharCode("a".code + i) ];
			var efun:Expr = {
				decl: EFunction(args, {
					decl:EBlock([
						{decl:EBinop("=", {decl:EConst(This), pos:p}, ident(p, "@this")), pos:p},
						call(p, ident(p, "@fun"), List.map(ident.bind(p), args))
					]),
					pos:p
				}),
				pos:p
			};
			var eif:ExprDecl = EIf({decl:EBinop("==", ident(p, "@fun"), null_(p)), pos:p},null_(p), Some(efun));
			var e = func(["@this", "@fun"], {decl:eif, pos:p});
			return {s:"@closure"+nargs, e:e};
		}, [0,1,2,3,4,5]));
		return List.map(function (tmp) { var v = tmp.s; var e = tmp.e; return {decl:EBinop("=", ident(p, v), e), pos:p}; }, inits);
	}

	public static function build (ctx:Context, types:ImmutableList<ModuleType>) : ImmutableList<Expr> {
		var packs = List.concat(List.map(gen_package.bind(ctx), types));
		var names = List.fold_left(gen_name.bind(ctx), [], types);
		var methods = List.rev(List.fold_left(function (acc, t) { return gen_type(ctx, t, acc); }, [], types));
		var boot = gen_boot(ctx);
		var inits = List.map(function (tmp) {
			var c = tmp.c; var e = tmp.e;
			ctx.curclass = core.Globals.s_type_path(c.cl_path);
			ctx.curmethod = "__init__";
			return gen_expr(ctx, e);
		},  List.rev(ctx.inits));
		ctx.inits = [];
		var vars = List.concat(List.map(gen_static_vars.bind(ctx), types));
		return List.append(packs, List.append(methods, List.append(boot :: names, List.append(inits, vars))));
	}

	public static function generate (com:context.Common.Context) : Void {
		Hashtbl.clear(files);
		var ctx = new_context(com, (context.Common.defined(com, NekoV1) ? 1 : 2), false);
		var libs:Expr = {decl:EBlock(generate_libs_inits(com.neko_libs)), pos:{psource:"<header>", pline:1}};
		var el = build(ctx, com.types);
		var emain:ImmutableList<Expr> = switch (com.main) { case None: []; case Some(e): [gen_expr(ctx, e)]; }
		var e:Expr = {decl:EBlock(List.append(header(), List.append(libs :: el, emain))), pos:Nast.null_pos};
		var source = context.Common.defined(com, NekoSource);
		var use_nekoc = context.Common.defined(com, UseNekoc);
		if (!use_nekoc) {
			try {
				Path.mkdir_from_path(com.file);
				var ch = sys.io.File.write(com.file);
				Nbytecode.write(ch, Ncompile.compile(ctx.version, e));
				ch.flush(); ch.close();
			}
			catch (err:neko.Ncompile.Error) {
				var msg = err.msg; var pos = err.pos;
				var pfile = context.Common.find_file(com, pos.psource);
				function loop (p:Int) : core.Globals.Pos {
					var pp = new core.Globals.Pos(pfile, p, p);
					return
					if (syntax.Lexer.get_error_line(pp) >= pos.pline) {
						pp;
					}
					else {
						loop(p+1);
					}
				}
				context.Common.abort(msg, loop(0));
			}
		}
		function command (cmd:String) : Int {
			return
			try {
				com.run_command(cmd);
			}
			catch (_:Any) {
				-1;
			}
		}
		var neko_file = haxe.io.Path.withoutExtension(com.file) + ".neko";
		if (source || use_nekoc) {
			var ch = sys.io.File.write(neko_file);
			Binast.write(ch, e);
			ch.flush(); ch.close();
		}
		if (use_nekoc && command("nekoc" + ((ctx.version > 1) ? " -version "+ctx.version : "") + " \"" + neko_file + "\"") != 0) {
			throw "Neko compilation failure";
		}
		if (source) {
			if (command("nekoc -p \""+neko_file+"\"") != 0) {
				throw "Failed to print neko code";
			}
			compiler.Main.delete_file(neko_file);
			sys.FileSystem.rename(haxe.io.Path.withoutExtension(com.file)+"2.neko", neko_file);
		}
	}
}