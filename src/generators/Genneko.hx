package generators;

import core.Path;
import core.Type.ModuleType;
import core.Type.TClass;
import core.Type.TConstant;
import core.Type.TExpr;

import haxe.ds.ImmutableList;

import neko.Nast;

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

	public static function pmap_list<A, B> (f:A->B, p:PMap<Any, A>) : ImmutableList<B> {
		return PMap.fold(function (v:A, acc:ImmutableList<B>) { return f(v) :: acc; }, p, Tl);
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
		trace("TODO: gen_call");
		throw false;
	}

	public static function gen_expr (ctx:Context, e:TExpr) : Expr {
		trace("TODO: gen_expr");
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
			case _:
				trace("TODO: finish gen_expr");
				throw false;
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

	public static function build (ctx:Context, types:ImmutableList<ModuleType>) : ImmutableList<Expr> {
		trace("TODO build");
		var packs = List.concat(List.map(gen_package.bind(ctx), types));
		var names = List.fold_left(gen_name.bind(ctx), [], types);
		throw false;
	}

	public static function generate (com:context.Common.Context) : Void {
		trace("TODO: generate");
		Hashtbl.clear(files);
		var ctx = new_context(com, (context.Common.defined(com, NekoV1) ? 1 : 2), false);
		var libs:Expr = {decl:EBlock(generate_libs_inits(com.neko_libs)), pos:{psource:"<header>", pline:1}};
		var el = build(ctx, com.types);
		throw false;
	}
}