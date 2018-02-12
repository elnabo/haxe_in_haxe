package typing;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
import ocaml.Hashtbl;
import ocaml.List;
import ocaml.PMap;
import ocaml.Ref;

using equals.Equal;
using haxe.EnumTools.EnumValueTools;
using ocaml.Cloner;
using StringTools;

class GenericException {
	public var s:String;
	public var p:core.Globals.Pos;
	public function new(s:String, p:core.Globals.Pos) {
		this.s = s;
		this.p = p;
	}
}

typedef Generic_context = {
	ctx:context.Typecore.Typer,
	subst:ImmutableList<{fst:core.Type.T, snd:core.Type.T}>,
	name: String,
	p: core.Globals.Pos,
	mg: Option<core.Type.ModuleDef>
}

class Build_canceled {
	public var bs:core.Type.BuildState;
	public function new(bs:core.Type.BuildState) {
		this.bs = bs;
	}
}

class Typeload {

	public static var locate_macro_error:Ref<Bool> = new Ref(true);
	public static var build_count:Ref<Int> = new Ref(0);

	public static function transform_abstract_field(com:context.Common.Context, this_t:core.Ast.TypeHint, a_t:core.Ast.TypeHint, a:core.Type.TAbstract, f:core.Ast.ClassField) : core.Ast.ClassField {
		trace("TODO: typing.Typeload.transform_abstract_field");
		return null;
	}

	public static function get_policy (ctx:context.Typecore.Typer, mpath:core.Path) {
		var sl1 = core.Ast.full_dot_path(mpath, mpath);
		function f (acc, mcp) {
			var sl2 = mcp.l;
			var policy = mcp.mcps;
			var recursive = mcp.b;
			if (core.Ast.match_path(recursive, sl1, sl2)) {
				return List.concat(policy, acc);
			}
			else {
				return acc;
			}
		}
		return List.fold_left(f, [], ctx.g.module_check_policies);
	}

	public static function make_module (ctx:context.Typecore.Typer, mpath:core.Path, file:String, loadp:core.Globals.Pos) : core.Type.ModuleDef {
		return {
			m_id:core.Type.alloc_mid(),
			m_path: mpath,
			m_types: [],
			m_extra: core.Type.module_extra(core.Path.unique_full_path(file), core.Define.get_signature(ctx.com.defines), context.Common.file_time(file), ((ctx.in_macro) ? MMacro : MCode), get_policy(ctx, mpath))
		};
	}

	/*
	 * Build module structure : should be atomic - no type loading is possible
	 */
	public static function module_pass_1 (ctx:context.Typecore.Typer, m:core.Type.ModuleDef, tdecls:ImmutableList<core.Ast.TypeDecl>, loadp:core.Globals.Pos) : {fst:ImmutableList<{fst:core.Type.ModuleType, snd:core.Ast.TypeDecl}>, snd:ImmutableList<core.Ast.TypeDecl>} {
		var com = ctx.com;
		var decls:ImmutableList<{fst:core.Type.ModuleType, snd:core.Ast.TypeDecl}> = [];
		function make_path(name:String, priv:Bool) : core.Path {
			function f (a:{fst:core.Type.ModuleType, snd:core.Ast.TypeDecl}) {
				return core.Type.t_path(a.fst).b == name;
			}
			if (List.exists(f, decls)) {
				core.Error.error("Type name " + name + " is already defined in this module", loadp);
			}
			if (priv) {
				return new core.Path(List.concat(m.m_path.a,["_"+m.m_path.b]), name);
			}
			else {
				return new core.Path(m.m_path.a, name);
			}
		}
		var pt:Option<core.Globals.Pos> = None;
		function make_decl (acc:ImmutableList<core.Ast.TypeDecl>, decl:core.Ast.TypeDecl) : ImmutableList<core.Ast.TypeDecl>{
			var p = decl.pos;
			var acc = switch (decl.decl) {
				case EImport(_), EUsing(_):
					switch (pt) {
						case None: acc;
						case Some(_):
							core.Error.error("import and using may not appear after a type declaration", p);
					};
				case EClass(d):
					var name = d.d_name.pack;
					if (name.length > 0 && name.charAt(0) == "$") {
						core.Error.error("Type names starting with a dollar are not allowed",p);
					}
					pt = Some(p);
					var priv = List.mem(core.Ast.ClassFlag.HPrivate, d.d_flags);
					var path = make_path(name, priv);
					var c = core.Type.mk_class(m, path, p, d.d_name.pos);
					// we shouldn't load any other type until we propertly set cl_build
					c.cl_build = function() {
						return core.Error.error(core.Globals.s_type_path(c.cl_path) + " is not ready to be accessed, separate your type declarations in several files", p);
					}
					c.cl_module = m;
					c.cl_private = priv;
					c.cl_doc = d.d_doc;
					c.cl_meta = d.d_meta;
					decls = {fst:core.Type.ModuleType.TClassDecl(c), snd:decl} :: decls;
					acc;
				case EEnum(d):
					var name = d.d_name.pack;
					if (name.length > 0 && name.charAt(0) == "$") {
						core.Error.error("Type names starting with a dollar are not allowed",p);
					}
					pt = Some(p);
					var priv = List.mem(core.Ast.EnumFlag.EPrivate, d.d_flags);
					var path = make_path(name, priv);
					var e:core.Type.TEnum = {
						e_path: path,
						e_module: m,
						e_pos: p,
						e_name_pos: d.d_name.pos,
						e_doc: d.d_doc,
						e_meta: d.d_meta,
						e_params: [],
						e_private: priv,
						e_extern: ocaml.List.mem(core.Ast.EnumFlag.EExtern, d.d_flags),
						e_constrs: new Map<String, core.Type.TEnumField>(),
						e_names: [],
						e_type: core.Type.enum_module_type(m, path, p)
					};
					decls = {fst:core.Type.ModuleType.TEnumDecl(e), snd:decl} :: decls;
					acc;
				case ETypedef(d):
					var name = d.d_name.pack;
					if (name.length > 0 && name.charAt(0) == "$") {
						core.Error.error("Type names starting with a dollar are not allowed",p);
					}
					pt = Some(p);
					var priv = ocaml.List.mem(core.Ast.EnumFlag.EPrivate, d.d_flags);
					var path = make_path(name, priv);
					var t:core.Type.TDef = {
						t_path: path,
						t_module: m,
						t_pos: p,
						t_name_pos: d.d_name.pos,
						t_doc: d.d_doc,
						t_private: priv,
						t_params: [],
						t_type: core.Type.mk_mono(),
						t_meta: d.d_meta
					};
					// failsafe in case the typedef is not initialized (see #3933)
					context.Typecore.delay(ctx, PBuildModule, function () {
						switch (t.t_type) {
							case TMono(r):
								switch (r.get()) {
									case None:
										t.t_type = TMono(new Ref(Some(com.basic.tvoid)));
									default:
								}
							default:
						}
					});
					decls = {fst:core.Type.ModuleType.TTypeDecl(t), snd:decl} :: decls;
					acc;
				case EAbstract(d):
					var name = d.d_name.pack;
					if (name.length > 0 && name.charAt(0) == "$") {
						core.Error.error("Type names starting with a dollar are not allowed",p);
					}
					pt = Some(p);
					var priv = List.mem(core.Ast.AbstractFlag.APrivAbstract, d.d_flags);
					var path = make_path(name, priv);
					var a:core.Type.TAbstract = {
						a_path: path,
						a_private: priv,
						a_module: m,
						a_pos: p,
						a_name_pos: d.d_name.pos,
						a_doc: d.d_doc,
						a_params: [],
						a_meta: d.d_meta,
						a_from: [],
						a_to: [],
						a_from_field: [],
						a_to_field: [],
						a_ops: [],
						a_unops: [],
						a_impl: None,
						a_array: [],
						a_this: core.Type.mk_mono(),
						a_resolve: None
					};
					decls = {fst:core.Type.ModuleType.TAbstractDecl(a), snd:decl} :: decls;
					switch (d.d_data) {
						case [] if (core.Meta.has(CoreType, a.a_meta)):
							a.a_this = core.Type.t_dynamic;
							acc;
						case fields:
							function a_t () : core.Ast.TypeHint {
								var params = List.map(function (t:core.Ast.TypeParam) : core.Ast.TypeParamOrConst {
									return TPType({ct:CTPath({tname:t.tp_name.pack, tparams:[], tsub:None, tpackage:[]}), pos:core.Globals.null_pos});
								}, d.d_params);
								return {ct: CTPath({tpackage:[], tname:d.d_name.pack, tparams:params, tsub:None}), pos:core.Globals.null_pos};
							}
							function loop (l:ImmutableList<core.Ast.AbstractFlag>) : core.Ast.TypeHint {
								return switch (l) {
									case []: a_t();
									case AIsType(t)::_: t;
									case _::l: loop(l);
								};
							}
							var this_t = loop(d.d_flags);
							var fields = ocaml.List.map(function (e) { return transform_abstract_field(com, this_t, a_t(), a, e); }, fields);
							var meta:core.Ast.Metadata = [];
							if (core.Type.has_meta(Dce, a.a_meta)) {
								meta = ({name:Dce, params:Tl, pos:core.Globals.null_pos} : core.Ast.MetadataEntry ) :: meta;
							}
							var acc = make_decl(acc, {decl:EClass({d_name: {pack:d.d_name.pack + "_Impl_", pos:d.d_name.pos}, d_flags: [HPrivate], d_data: fields, d_doc: None, d_params: [], d_meta: meta }), pos:p});
							switch (decls) {
								case {fst:TClassDecl(c)}::_:
									List.iter(function (m:core.Ast.MetadataEntry) {
										switch (m.name) {
											case Build, CoreApi, Allow, Access, Enum, Dce, Native, JsRequire, PythonImport, Expose, Deprecated, PhpGlobal:
												c.cl_meta = m :: c.cl_meta;
											default:
										}
									}, a.a_meta);
									a.a_impl = Some(c);
									c.cl_kind = KAbstractImpl(a);
									c.cl_meta = ({name:Final, params:Tl, pos:core.Globals.null_pos} : core.Ast.MetadataEntry) ::c.cl_meta;
								case _: throw false;
						}
						acc;
					}
			}
			return decl :: acc;
		}
		var tdecls = List.fold_left(make_decl, [], tdecls);
		var decls = List.rev(decls);
		return {fst:decls, snd:List.rev(tdecls)};
	}

	public static var current_stdin:Option<String> = None; // TODO: we're supposed to clear this at some point

	public static function parse_file_from_lexbuf (com:context.Common.Context, file:String, p:core.Globals.Pos, buf:byte.ByteData) : {pack:ImmutableList<String>, decls:ImmutableList<core.Ast.TypeDecl>} {
		var t = core.Timer.timer(["parsing"]);
		syntax.Lexer.init(file, true);
		context.Common.stats.s_files_parsed.set(context.Common.stats.s_files_parsed.get()+1);
		var data = try {
			new haxeparser.HaxeParser(buf, file, com.defines).parse();
		}
		catch (e:Dynamic) {
			t();
			throw (e);
		}
		switch(context.Common.display_default.get()) {
			case DMModuleSymbols(filter) if (filter != None || context.Display.is_display_file(file)):
				var ds = context.display.DocumentSymbols.collect_module_symbols(data);
				com.shared.shared_display_information.document_symbols = {s:file, l:ds} :: com.shared.shared_display_information.document_symbols;
			case _:
		}
		t();
		context.Common.log(com, "Parsed "+file);
		return data;
	}

	public static function parse_file_from_string (com:context.Common.Context, file:String, p:core.Globals.Pos, s:String) : {pack:ImmutableList<String>, decls:ImmutableList<core.Ast.TypeDecl>} {
		var data = byte.ByteData.ofString(s);
		return parse_file_from_lexbuf(com, file, p, data);
	}

	public static function parse_file (com:context.Common.Context, file:String, p:core.Globals.Pos) : {pack:ImmutableList<String>, decls:ImmutableList<core.Ast.TypeDecl>} {
		var use_stdin = context.Common.defined(com, DisplayStdin) && context.Display.is_display_file(file);
		if (use_stdin) {
			var s = switch (current_stdin) {
				case Some(v): v;
				case None:
					var ss = Sys.stdin().readAll().toString();
					Sys.stdin().close();
					current_stdin = Some(ss);
					ss;
			};
			return parse_file_from_string(com, file, p, s);
		}
		else {
			var ch = try {
				sys.io.File.read(file, true);
			}
			catch (e:Any) {
				core.Error.error("Could not open " + file, p);
			};
			try {
				var data = byte.ByteData.ofBytes(ch.readAll());
				var res = parse_file_from_lexbuf(com, file, p, data);
				ch.close();
				return res;
			}
			catch (e:Dynamic) {
				ch.close();
				throw e;
			}
		}
	}
	public static var parse_hook = new Ref(parse_file);
	public static var type_module_hook = new Ref(function(ctx: context.Typecore.Typer, m:core.Path, p:core.Globals.Pos) : Option<core.Type.ModuleDef> {
		return None;
	});
	public static var type_function_params_rec = new Ref(type_function_params);
	public static var return_partial_type = new Ref(false);

	public static function type_function_arg (ctx:context.Typecore.Typer, t:core.Type.T, e:Option<core.Ast.Expr>, opt:Bool, p:core.Globals.Pos) : {fst:core.Type.T, snd:Option<core.Ast.Expr>} {
		if (opt) {
			var e:Option<core.Ast.Expr> = switch (e) {
				case None: Some({expr:EConst(CIdent("null")), pos:p});
				case _: e;
			}
			return {fst:ctx.t.tnull(t), snd:e};
		}
		else
			var t = switch (e) {
				case Some({expr:EConst(CIdent("null")), pos:p}):
					ctx.t.tnull(t);
				case _: t;
			}
			return {fst:t, snd:e};
	}

	public static function type_var_field (ctx:context.Typecore.Typer, t:core.Type.T, e:core.Ast.Expr, stat:Bool, do_display:Bool, p:core.Globals.Pos) {
		ctx.curfun = (stat) ? FunStatic : FunMember;
		var e = (do_display) ? context.display.ExprPreprocessing.process_expr(ctx.com, e) : e;
		var e1 = context.Typecore.type_expr(ctx, e, WithType(t));
		var e2 = context.Typecore.cast_or_unify_ref.get()(ctx, t, e1, p);
		return switch (t) {
			case TType({t_path:{a:[], b:"UInt"}}, []), TAbstract({a_path:{a:[], b:"UInt"}}, []) if (stat):
				var _e = e2.clone();
				_e.etype = t;
				_e;
			case _: e2;
		}
	}

	public static function apply_macro (ctx:context.Typecore.Typer, mode:context.Typecore.MacroMode, path:String, el:ImmutableList<core.Ast.Expr>, p:core.Globals.Pos) : Option<core.Ast.Expr> {
		var pack = path.split(".");
		if (pack.length > 2) {
			core.Error.error("Invalid macro path", p);
		}
		var meth = pack.pop();
		var name = pack.pop();
		return ctx.g.do_macro(ctx, mode, new core.Path(pack, name), meth, el, p);
	}
	/* since load_type_def and load_instance are used in PASS2, they should not access the structure of a type */
	/*
	 *load a type or a subtype definition
	 */
	public static function load_type_def(ctx:context.Typecore.Typer, p:core.Globals.Pos, t:core.Ast.TypePath) : core.Type.ModuleType {
		var no_pack = t.tpackage == Tl;
		var tname = switch (t.tsub) {
			case None: t.tname;
			case Some(n): n;
		}
		if (tname == "") {
			throw context.Display.DisplayException.DisplayToplevel(context.DisplayToplevel.collect(ctx, true));
		}
		return try {
			if (t.tsub != None) {
				throw ocaml.Not_found.instance;
			}
			function path_matches (t2:core.Type.ModuleType) {
				var tp = core.Type.t_path(t2);
				return  (tp == new core.Path(t.tpackage, tname)) || (no_pack && tp.b == tname);
			}
			try {
				List.find(path_matches, ctx.m.curmod.m_types);
			}
			catch (_:ocaml.Not_found) {
				var _tmp = List.find(function (t2:{mt:core.Type.ModuleType, pos:core.Globals.Pos}) { return path_matches(t2.mt); }, ctx.m.module_types);
				var t:core.Type.ModuleType = _tmp.mt; var pi:core.Globals.Pos = _tmp.pos;
				context.display.ImportHandling.mark_import_position(ctx.com, pi);
				t;
			}
		}
		catch (_:ocaml.Not_found) {
			function next() {
				var t:core.Ast.TypePath = t;
				var m:core.Type.ModuleDef;
				try {
					m = ctx.g.do_load_module(ctx, new core.Path(t.tpackage, t.tname), p);
				}
				catch (err:core.Error) {
					switch (err.msg) {
						case Module_not_found(_) if (p == err.pos) :
							switch (t.tpackage) {
								case "std"::l:
									t = {
										tpackage:l,
										tname : t.tname,
										tparams : t.tparams,
										tsub : t.tsub
									};
									m = ctx.g.do_load_module(ctx, new core.Path(t.tpackage, t.tname), p);
								case _: throw err;
							}
						default: throw err;
					}
				}
				var tpath = new core.Path(t.tpackage, tname);
				return try {
					List.find(function (t) {
						return !(core.Type.t_infos(t).mt_private && core.Type.t_path(t).equals(tpath));
					}, m.m_types);
				}
				catch (_:ocaml.Not_found) {
					throw new core.Error(Type_not_found(m.m_path,tname),p);
				}
			}
			// lookup in wildcard imported packages
			try {
				if (!no_pack) {
					throw new ocaml.Exit();
				}
				function loop (l:ImmutableList<{pos:core.Globals.Pos, l:ImmutableList<String>}>) : core.Type.ModuleType {
					return switch (l) {
						case []: throw new ocaml.Exit();
						case {l:wp, pos:pi}::l:
							try {
								var _t = {
									tname: t.tname,
									tpackage: wp,
									tsub: t.tsub,
									tparams: t.tparams
								}
								var t = load_type_def(ctx, p, _t);
								context.display.ImportHandling.mark_import_position(ctx.com, pi);
								t;
							}
							catch (e:core.Error) {
								switch (e.msg) {
									case Module_not_found(_), Type_not_found(_, _) if (p == e.pos):
										loop(l);
									default: throw e;
								}
							}
					}
				}
				loop(ctx.m.wildcard_packages);
			}
			catch (_:ocaml.Exit) {
				// lookup in our own package - and its upper packages
				function loop (l:ImmutableList<String>) {
					return switch (l) {
						case []: throw ocaml.Exit.instance;
						case _::lnext:
							try {
								var _t = {
									tname: t.tname,
									tpackage: List.rev(l),
									tsub: t.tsub,
									tparams: t.tparams
								};
								load_type_def(ctx, p, _t);
							}
							catch (e:core.Error) {
								switch (e.msg) {
									case Module_not_found(_), Type_not_found(_, _) if (p == e.pos):
										loop(lnext);
									case _: throw e;
								}
							}
					}
				}
				try {
					if (!no_pack) { throw ocaml.Exit.instance; }
					switch (ctx.m.curmod.m_path.a) {
						case []: throw ocaml.Exit.instance;
						case x::_:
							/* this can occur due to haxe remoting : a module can be
							already defined in the "js" package and is not allowed
							to access the js classes */
							try {
								switch (PMap.find(x, ctx.com.package_rules)) {
									case Forbidden: throw new ocaml.Exit();
									case _:
								}
							}
							catch (_:ocaml.Not_found) {}
					}
					loop(List.rev(ctx.m.curmod.m_path.a));
				}
				catch (_:ocaml.Exit) {
					next();
				}
			}
		}
	}

	public static function resolve_position_by_path (ctx:context.Typecore.Typer, path:core.Ast.TypePath, p:core.Globals.Pos) : Dynamic {
		var mt = load_type_def(ctx, p, path);
		var p = core.Type.t_infos(mt).mt_pos;
		throw context.Display.DisplayException.DisplayPosition([p]);
	}

	public static function check_param_constraints (ctx:context.Typecore.Typer, types:core.Type.TypeParams, t:core.Type.T, pl:ImmutableList<core.Type.T>, c:core.Type.TClass, p:core.Globals.Pos) {
		switch (core.Type.follow(t)) {
			case TMono(_):
			case _:
				var ctl:ImmutableList<core.Type.T> = switch (c.cl_kind) {
					case KTypeParameter(l): l;
					case _: [];
				}
				List.iter(function (ti) {
					var _ti = core.Type.apply_params(types, pl, ti);
					_ti = switch (core.Type.follow(_ti)) {
						case TInst(c, pl) if (c.cl_kind == KGeneric):
							// if we solve a generic contraint, let's substitute with the actual generic instance before unifying
							var f = ctx.g.do_build_instance(ctx, TClassDecl(c), p).f;
							f(pl);
						case _: ti;
					}
					try {
						context.Typecore.unify_raise(ctx, t, ti, p);
					}
					catch (err:core.Error) {
						switch (err.msg) {
							case Unify(l):
								if (!ctx.untyped_) {
									// if not ctx.untyped then display_error ctx (error_msg (Unify (Constraint_failure (s_type_path c.cl_path) :: l))) p;
									var l = (core.Type.UnifyError.Constraint_failure(core.Globals.s_type_path(c.cl_path))) :: l;
									context.Typecore.display_error(ctx, core.Error.error_msg(Unify(l)), p);
								}
							case _: throw err;
						}
					}
				}, ctl);
		}
	}

	public static function requires_value_meta (com:context.Common.Context, co:Option<core.Type.TClass>) : Bool {
		if (context.Common.defined(com, DocGen)) { return true; }
		return switch (co) {
			case None: false;
			case Some(c): c.cl_extern || core.Meta.has(Rtti, c.cl_meta);
		}
	}

	public static function generate_value_meta (com:context.Common.Context, co:Option<core.Type.TClass>, cf:core.Type.TClassField, args:ImmutableList<core.Ast.FunArg>) : Void {
		if (requires_value_meta(com, co)) {
			var values:ImmutableList<core.Ast.ObjectField> = List.fold_left(function (acc:ImmutableList<core.Ast.ObjectField>, arg:core.Ast.FunArg) : ImmutableList<core.Ast.ObjectField> {
				var name = arg.name.pack; var p = arg.name.pos; var eo = arg.value;
				return switch (eo) {
					case Some(e):
						var res:core.Ast.ObjectField = {name:name, pos:p, quotes:NoQuotes, expr: e};
						res :: acc;
					case _: acc;
				}
			}, [], args);
			switch (values) {
				case []:
				case _: 
					var m:core.Ast.MetadataEntry = {name:Value, params:[{expr:EObjectDecl(values), pos:cf.cf_pos}], pos:core.Globals.null_pos};
					cf.cf_meta = m :: cf.cf_meta;
			}
		}
	}

	public static function pselect (p1:core.Globals.Pos, p2:core.Globals.Pos) {
		return (p1.equals(core.Globals.null_pos)) ? p2 : p1;
	}

	public static function load_instance (allow_display:Bool=false, ctx:context.Typecore.Typer, tp:core.Ast.PlacedTypePath, allow_no_params:Bool, p:core.Globals.Pos) : core.Type.T {
		var t = tp.tp;
		var pn = tp.pos;
		var p = pselect(pn, p);
		var t:core.Type.T = try {
			if (t.tpackage != Tl || t.tsub != None) {
				throw ocaml.Not_found.instance;
			}
			var pt = List.assoc_typeparams(t.tname, ctx.type_params);
			if (t.tparams != Tl) {
				core.Error.error("Class type parameter "+t.tname + " can't have parameters", p);
			}
			pt;
		}
		catch (_:ocaml.Not_found) {
			var mt = load_type_def(ctx, p, t);
			var is_generic = false; var is_generic_build = false;
			switch (mt) {
				case TClassDecl({cl_kind:KGeneric}): is_generic = true;
				case TClassDecl({cl_kind:KGenericBuild(_)}): is_generic_build = true;
				case _:
			}
			var _tmp = ctx.g.do_build_instance(ctx, mt, p);
			var types = _tmp.types; var path = _tmp.path; var f = _tmp.f;
			var is_rest = is_generic_build && (switch (types) {case [{name:"Rest"}]: true; case _:false;});
			if (allow_no_params && t.tparams == Tl && !is_rest) {
				var pl = new Ref<ImmutableList<core.Type.T>>([]);
				pl.set(List.map(function (tp) {
					var name = tp.name; var t = tp.t;
					return switch (core.Type.follow(t)) {
						case TInst(c, _):
							var t = core.Type.mk_mono();
							if (!c.cl_kind.equals(KTypeParameter([])) || is_generic) {
								context.Typecore.delay(ctx, PCheckConstraint, function(){
									check_param_constraints(ctx, types, t, pl.get(), c, p);
								});
							}
							t;
						case _: throw false;
					}
				}, types));
				f(pl.get());
			}
			else if (path.equals(new core.Path([], "Dynamic"))) {
				switch (t.tparams) {
					case []: core.Type.t_dynamic;
					case [TPType(t)]: TDynamic(new Ref(load_complex_type(ctx, true, p, t)));
					case _: core.Error.error("Too many parameteres for Dynamic", p); throw false;
				}
			}
			else {
				if (!is_rest && ctx.com.display.dms_error_policy != EPIgnore && List.length(types) != List.length(t.tparams)) {
					core.Error.error ("Invalid number of type parameters for " + core.Globals.s_type_path(path), p);
				}
				var tparams:ImmutableList<core.Type.T> = List.map(function (t:core.Ast.TypeParamOrConst) : core.Type.T {
					return switch (t) {
						case TPExpr(e):
							var name = switch (e.expr) {
								case EConst(CString(s)): "S"+s;
								case EConst(CInt(i)): "I"+i;
								case EConst(CFloat(f)): "F"+f;
								case _: "Expr";
							}
							var c = core.Type.mk_class(ctx.m.curmod, new core.Path([], name), p, e.pos);
							c.cl_kind = KExpr(e);
							TInst(c, []);
						case TPType(t): load_complex_type(ctx, true, p, t);
					}
				}, t.tparams);
				function loop(tl1:ImmutableList<core.Type.T>, tl2:core.Type.TypeParams, is_rest:Bool) : ImmutableList<core.Type.T>{
					return switch ({fst:tl1, snd:tl2}) {
						case {fst:t::tl1, snd:{name:name, t:t2}::tl2}:
							function check_const(c) {
								var is_expression = switch (t) {
									case TInst({cl_kind:KExpr(_)},_): true;
									case _: false;
								}
								var expects_expression = name == "Const" || core.Meta.has(Const, c.cl_meta);
								var accepts_expression = name == "Rest";
								if (is_expression) {
									if (!expects_expression && !accepts_expression) {
										core.Error.error("Constant value unexpected here", p);
									}
								}
								else if (expects_expression) {
									core.Error.error("Type parameter is expected to be constant value", p);
								}
							}
							var is_rest = is_rest || name == "Rest" && is_generic_build;
							var t:core.Type.T = switch (core.Type.follow(t2)) {
								case TInst(c={cl_kind:KTypeParameter([])}, []) if (!is_generic):
									check_const(c);
									t;
								case TInst(c, []):
									check_const(c);
									var r = context.Typecore.exc_protect(ctx, function (r) {
										r.set(core.Type.lazy_available(t));
										context.Typecore.delay(ctx, PCheckConstraint, function () {
											check_param_constraints(ctx, types, t, tparams, c, p);
										});
										return t;
									}, "constraint");
									TLazy(r);
								case _: throw false;
							}
							t::loop(tl1, tl2, is_rest);
						case {fst:[], snd:[]}: [];
						case {fst:[], snd:[{name:"Rest"}]} if (is_generic_build): [];
						case {fst:[], snd:{t:t}::tl} if (ctx.com.display.dms_error_policy == EPIgnore):
							t::loop([], tl, is_rest);
						case {fst:[]}:
							core.Error.error("Not enough type parameters for "+core.Globals.s_type_path(path), p);
						case {fst:t::tl, snd:[]}:
							if (is_rest) {
								t::loop(tl, [], true);
							}
							else {
								core.Error.error("Too many parameters for "+core.Globals.s_type_path(path), p);
							}
					}
				}
				var params = loop(tparams, types, false);
				f(params);
			}
		}
		if (allow_display) {
			context.display.DisplayEmitter.check_display_type(ctx, t, pn);
		}
		return t;
	}

	/*
	 * build an instance from a complex type
	 */
	public static function load_complex_type (ctx:context.Typecore.Typer, allow_display:Bool, p:core.Globals.Pos, tp:core.Ast.TypeHint) : core.Type.T {
		var t = tp.ct;
		var pn = tp.pos;
		var p = pselect(pn, p);
		return switch (t) {
			case CTParent(_t): load_complex_type(ctx, allow_display,p, _t);
			case CTPath(_t): load_instance(allow_display, ctx, {tp:_t, pos:pn}, false, p);
			case CTOptional(_): core.Error.error("Optional type not allowed here", p);
			case CTNamed(_): core.Error.error("Named type not allowed here", p);
			case CTExtend(tl, l):
				var ta = load_complex_type(ctx, allow_display, p, {ct:CTAnonymous(l), pos:p});
				switch (ta) {
					case TAnon(a):
						function is_redefined(cf1:core.Type.TClassField, a2:core.Type.TAnon) : Bool {
							try {
								var cf2 = PMap.find(cf1.cf_name, a2.a_fields);
								var st = core.Type.s_type.bind(core.Type.print_context());
								if (!core.Type.type_iseq(cf1.cf_type, cf2.cf_type)) {
									context.Typecore.display_error(ctx, "Cannot redefine field "+ cf1.cf_name + " with different type", p);
									context.Typecore.display_error(ctx, "First type was "+ st(cf1.cf_type), cf1.cf_pos);
									return core.Error.error("Second type was "+st(cf2.cf_type), cf2.cf_pos);
								}
								else {
									return true;
								}
							}
							catch (_:ocaml.Not_found) {
								return false;
							}
						}
						function mk_extension(t:core.Type.T) : core.Type.T {
							return switch (core.Type.follow(t)) {
								case TInst({cl_kind:KTypeParameter(_)},_):
									core.Error.error("Cannot structurally extend type parameters", p);
								case TMono(_):
									core.Error.error("Loop found in cascading signatures definitions. Please change order/import", p);
								case TAnon(a2):
									PMap.iter(function (_, cf) {is_redefined(cf, a2);}, a.a_fields);
									TAnon({a_fields: PMap.foldi(PMap.add, a.a_fields, a2.a_fields), a_status:new Ref(core.Type.AnonStatus.Extend([t]))});
								case _: 
									core.Error.error("Can only extend structures", p);
								
							}
						}
						function loop (t) {
							switch (core.Type.follow(t)) {
								case TAnon(a2):
									PMap.iter(function (f, cf) {
										if (!is_redefined(cf, a)) {
											a.a_fields.set(f, cf);
										}
									}, a2.a_fields);
								case _:
									core.Error.error("Can only extends structures", p);
							}
						}
						var il = List.map(function (tp) { return load_instance(allow_display, ctx, tp, false, p);}, tl);
						var tr = new Ref(None);
						var t:core.Type.T = TMono(tr);
						var r = context.Typecore.exc_protect(ctx, function (r) {
							r.set(core.Type.lazy_processing(function () { return t;}));

							var some = switch(il) {
								case [i]: mk_extension(i);
								case _: 
									List.iter(loop, il);
									a.a_status.set(Extend(il));
									ta;
							}
							tr.set(Some(some));
							return t;
						}, "constraint");
						TLazy(r);
					case _: throw false;
				}
			case CTAnonymous(l):
				function loop(acc:Map<String, core.Type.TClassField>, f:core.Ast.ClassField) : Map<String, core.Type.TClassField> {
					var n = f.cff_name.pack;
					var p = f.cff_pos;
					if (PMap.mem(n, acc)) {
						core.Error.error("Duplicate field declaration : "+n, p);
					}
					function topt (to) {
						return switch (to) {
							case None: core.Error.error("Explicit type required for field "+n, p);
							case Some(t): load_complex_type(ctx, allow_display, p, t);
						};
					}
					if (n == "new") {
						ctx.com.warning("Structures with new are deprecated, use haxe.Constraints.Constructible instead", p);
					}
					function no_expr (ne:Option<core.Ast.Expr>) {
						switch (ne) {
							case None:
							case Some({pos:p}): core.Error.error("Expression not allowed here", p);
						}
					}
					var pub = new Ref(true);
					var dyn = new Ref(false);
					var params = new Ref([]);
					var final_ = new Ref(false);
					List.iter(function (a:core.Ast.Access) {
						switch (a) {
							case APublic:
							case APrivate: pub.set(false);
							case ADynamic if (switch (f.cff_kind) { case FFun(_) : true; case _: false;}):
								dyn.set(true);
							case AFinal: final_.set(true);
							case AStatic, AOverride, AInline, ADynamic, AMacro:
								core.Error.error("Invalid access"+core.Ast.s_access(a), p);
						}
					}, f.cff_access);
					var _tmp:{fst:core.Type.T, snd:core.Type.FieldKind} = switch (f.cff_kind) {
						case FVar(t, e) if (final_.get()):
							no_expr(e);
							var t = switch (t) {
								case None:
									core.Error.error("Type required for structure property", p);
								case Some(_t):
									_t;
							}
							{fst:load_complex_type(ctx, allow_display, p, t), snd:Var({v_read:AccNormal, v_write:AccNever})};
						case FVar(Some({ct:CTPath({tpackage:[], tname:"Void"})}), _), FProp (_,_,Some({ct:CTPath({tpackage:[], tname:"Void"})}),_):
							core.Error.error("Fields of type Void are not allowed in structures", p);
						case FVar(t, e):
							no_expr(e);
							{fst: topt(t), snd:Var({v_read:AccNormal, v_write:AccNormal})};
						case FFun(fd):
							params.set(type_function_params_rec.get()(ctx, fd, f.cff_name.pack, p));
							no_expr(fd.f_expr);
							var old = ctx.type_params;
							ctx.type_params = params.get().concat(old);
							var args = List.map(function (farg) {
								no_expr(farg.value);
								return {name:farg.name.pack, opt:farg.opt, t:topt(farg.type)};
							}, fd.f_args);
							var t:{fst:core.Type.T, snd:core.Type.FieldKind} = {fst:TFun({args:args, ret:topt(fd.f_type)}), snd:Method((dyn.get() ? MethDynamic : MethDynamic))};
							ctx.type_params = old;
							t;
						case FProp(i1, i2, t, e):
							no_expr(e);
							function access (pn:core.Ast.PlacedName, get:Bool) : core.Type.VarAccess {
								return switch (pn.pack) {
									case "null": AccNo;
									case "never": AccNever;
									case "default": AccNormal;
									case "dynamic": AccCall;
									case "get" if (get): AccCall;
									case "set" if (!get): AccCall;
									case x if (get && x == ("get_"+n)) : AccCall;
									case x if (!get && x == ("set_"+n)) : AccCall;
									case _:
										core.Error.error("Custom property access is no longer supported in Haxe 3", f.cff_pos);
								}
							}
							var t = switch (t) {
								case None:
									core.Error.error("Type required for structure property", p);
								case Some(_t):
									_t;
							};
							{fst:load_complex_type(ctx, allow_display, p, t), snd:Var({v_read:access(i1, true), v_write:access(i2, false)})};
					}
					var t = _tmp.fst; var access = _tmp.snd;
					var t = if (core.Meta.has(Optional, f.cff_meta)) {
						ctx.t.tnull(t);
					}
					else {
						t;
					}
					var cf = core.Type.mk_field(n, t, p, f.cff_name.pos);
					cf.cf_public = pub.get();
					cf.cf_kind = access;
					cf.cf_params = params.get();
					cf.cf_doc = f.cff_doc;
					cf.cf_meta = f.cff_meta;
					init_meta_overloads(ctx, None, cf);
					if (ctx.is_display_file) {
						context.display.DisplayEmitter.check_display_metadata(ctx, cf.cf_meta);
						context.display.DisplayEmitter.maybe_display_field(ctx, cf.cf_name_pos, cf);
					}
					return PMap.add(n, cf, acc);
				}
				core.Type.mk_anon(ocaml.List.fold_left(loop, new Map<String, core.Type.TClassField>(), l));
			case CTFunction(args, r):
				switch (args) {
					case [{ct:CTPath({tpackage:[], tparams:[], tname:"Void"})}]:
						TFun({args:[], ret:load_complex_type(ctx, allow_display, p, r)});
					case _:
						TFun({args:List.map(function (t:core.Ast.TypeHint) {
							var t_opt = switch (t.ct) {
								case CTOptional(t): {fst:t, snd:true};
								case _: {fst:t, snd:false};
							}
							var n_t = switch (t_opt.fst.ct) {
								case CTNamed(n, t): {fst:n.pack, snd:t};
								case _: {fst:"", snd:t};
							}
							return {name:n_t.fst, opt:t_opt.snd, t:load_complex_type(ctx, allow_display, p, t)};
						}, args), ret:load_complex_type(ctx, allow_display, p, r)});
				}
		}
	}

	public static function init_meta_overloads (ctx:context.Typecore.Typer, co:Option<core.Type.TClass>, cf:core.Type.TClassField) {
		var overloads = new Ref<ImmutableList<core.Type.TClassField>>(Tl);
		function filter_meta(m:core.Ast.MetadataEntry) : Bool {
			return switch (m) {
				case {name:Overload}, {name:Value}: false;
				case _: true;
			}
		}
		var cf_meta = List.filter(filter_meta, cf.cf_meta);
		cf.cf_meta = List.filter(function (m:core.Ast.MetadataEntry) : Bool {
			return switch (m) {
				case {name:Overload, params:[{expr:EFunction(fname, f), pos:p}]}:
					if (fname != None) {
						core.Error.error("Function name must not be part of @:overload", p);
					}
					switch (f.f_expr) {
						case Some({expr:EBlock([])}):
						case _: core.Error.error("Overload must only declare an empty method body {}", p);
					}
					var old = ctx.type_params;
					switch (ctx.type_params) {
						case []:
						case l:
							ctx.type_params = List.filter( function(t) { return !ocaml.List.mem(t, cf.cf_params); }, ctx.type_params);
					}
					var params = type_function_params_rec.get()(ctx, f, cf.cf_name, p);
					ctx.type_params = List.concat(params, ctx.type_params);
					function topt (t) {
						return switch (t) {
							case None: core.Error.error("Explicit type required", p);
							case Some(_t): load_complex_type(ctx, true, p, _t);
						}
					}
					var args = List.map(function (farg) {
						return {name:farg.name.pack, opt:farg.opt, t:topt(farg.type)};
					}, f.f_args);
					var cf = cf.clone();
					cf.cf_type = TFun({args:args, ret:topt(f.f_type)});
					cf.cf_params = params;
					cf.cf_meta = cf_meta;
					generate_value_meta(ctx.com, co, cf, f.f_args);
					overloads.set(cf::overloads.get());
					ctx.type_params = old;
					false;
				case {name:Overload, params:[]} if (ctx.com.config.pf_overload):
					function topt (arg:core.Type.TSignatureArg) {
						switch (arg.t) {
							case TMono(t) if (t.get() == None):
								core.Error.error("Explicit type required for overload functions\nFor function argument '" + arg.name + "'", cf.cf_pos);
							case _:
						}
					}
					switch (core.Type.follow(cf.cf_type)) {
						case TFun({args:args}):
							List.iter(topt, args);
						case _: // could be a variable
					}
					true;
				case {name:Overload, params:[], pos:p}:
					core.Error.error("This platform does not support this kind of overload declaration. Try @:overload(function()... {}) instead", m.pos);
				case {name:Overload, pos:p}:
					core.Error.error("Invalid @:overload metadata format", m.pos);
				case _: true;
			}
		}, cf.cf_meta);
		cf.cf_overloads = List.rev(overloads.get());
	}

	public static function hide_params (ctx:context.Typecore.Typer) : Void -> Void {
		var old_m = ctx.m;
		var old_type_params = ctx.type_params;
		var old_deps = ctx.g.std.m_extra.m_deps;
		ctx.m = {
			curmod: ctx.g.std.clone(),
			module_types: [],
			module_using: [],
			module_globals: new Map<String, {a:core.Type.ModuleType, b:String, pos:core.Globals.Pos}>(),
			wildcard_packages: [],
			module_imports: []
		};
		ctx.type_params = [];
		return function () {
			ctx.m = old_m;
			ctx.type_params = old_type_params;
			// restore dependencies that might be have been wronly inserted
			ctx.g.std.m_extra.m_deps = old_deps;
		}
	}

	/*
	 * load a type while ignoring the current imports or local types
	 */
	public static function load_core_type (ctx:context.Typecore.Typer, name:String) : core.Type.T {
		var show = hide_params(ctx);
		var t = load_instance(false, ctx, {tp:{tpackage:[], tname:name, tparams:[], tsub:None}, pos:core.Globals.null_pos}, false, core.Globals.null_pos);
		show();
		core.Type.add_dependency(ctx.m.curmod, switch (t) {
			case TInst(c, _): c.cl_module;
			case TType(t, _): t.t_module;
			case TAbstract(a, _): a.a_module;
			case TEnum(e, _): e.e_module;
			case _: throw false;
		});
		return t;
		
	}

	/*
	 * load either a tye t or Null<Unknown> if not defined
	 */
	public static function load_type_hint(?opt:Bool=false, ctx:context.Typecore.Typer, pcur:core.Globals.Pos, t:Option<core.Ast.TypeHint>) : core.Type.T {
		var t = switch (t) {
			case None: core.Type.mk_mono();
			case Some({ct:t, pos:p}):
				try {
					load_complex_type(ctx, true, pcur, {ct:t, pos:p});
				}
				catch (exc:core.Error) {
					switch (exc.msg) {
						case Module_not_found({a:[], b:name}):
							if (context.display.Diagnostics.is_diagnostics_run(ctx)) {
								context.DisplayToplevel.handle_unresolved_identifier(ctx, name, p, true);
							}
							// Default to Dynamic in display mode
							if (ctx.com.display.dms_display) {
								core.Type.t_dynamic;
							}
							else {
								throw exc;
							}
						case _: throw exc;
					}
				}
		}
		return (opt) ? ctx.t.tnull(t) : t;
	}
	// ----------------------------------------------------------------------
	// Structure check

	public static function check_overriding (ctx:context.Typecore.Typer, c:core.Type.TClass, f:core.Type.TClassField) : Void {
		trace("Typeload.check_overriding");
		throw false;
	}

	public static function return_flow (ctx:context.Typecore.Typer, e:core.Type.TExpr) {
		function error() {
			context.Typecore.display_error(ctx, 'Missing return: ${core.Type.s_type(core.Type.print_context(), ctx.ret)}', e.epos);
			throw ocaml.Exit.instance;
		}
		var return_flow = return_flow.bind(ctx);
		function uncond(e:core.Type.TExpr) {
			return switch (e.eexpr) {
				case TIf(_), TWhile(_), TSwitch(_), TTry(_), TFunction(_):
				case TReturn(_), TThrow(_): throw ocaml.Exit.instance;
				case _: core.Type.iter(uncond, e);
			}
		}
		function has_unconditional_flow (e:core.Type.TExpr) : Bool {
			try {
				uncond(e);
				return false;
			}
			catch (_:ocaml.Exit) {
				return true;
			}
		}
		switch (e.eexpr) {
			case TReturn(_), TThrow(_):
			case TParenthesis(e), TMeta(_,e):
				return_flow(e);
			case TBlock(el):
				function loop(l:ImmutableList<core.Type.TExpr>) {
					switch (l) {
						case []: error();
						case [e]: return_flow(e);
						case e::_ if (has_unconditional_flow(e)):
						case _::l: loop(l);
					}
				}
				loop(el);
			case TIf(_, e1, Some(e2)):
				return_flow(e1); return_flow(e2);
			case TSwitch(v, cases, Some(e)):
				List.iter(function (c) {
					return_flow(c.e);
				}, cases);
				return_flow(e);
			case TSwitch({eexpr:TMeta({name:Exhaustive}, _)}, cases, None):
				List.iter(function (c) {
					return_flow(c.e);
				}, cases);
			case TTry(e, cases):
				return_flow(e);
				List.iter(function (c) {
					return_flow(c.e);
				}, cases);
			case TWhile({eexpr:TConst(TBool(true))}, e, _):
				// a special case for "inifite" while loops that have no break
				function loop(e:core.Type.TExpr) {
					switch (e.eexpr) {
						// ignore nested loops to not accidentally get one of its breaks
						case TWhile(_), TFor(_):
						case TBreak: error();
						case _: core.Type.iter(loop, e);
					}
				}
				loop(e);
			case _: error();

		}
	}

	// ----------------------------------------------------------------------
	// PASS 1 & 2 : Module and Class Structure

	public static function is_generic_parameter (ctx:context.Typecore.Typer, c:core.Type.TClass) : Bool {
		// first check field parameters, then class parameters
		return try {
			List.assoc_typeparams(c.cl_path.b, ctx.curfield.cf_params);
			core.Meta.has(Generic, ctx.curfield.cf_meta);
		}
		catch (_:ocaml.Not_found) {
			try {
				List.assoc_typeparams(c.cl_path.b, ctx.type_params);
				switch (ctx.curclass.cl_kind) {
					case KGeneric: true;
					case _: false;
				}
			}
			catch (_:ocaml.Not_found) {
				false;
			}
		}
	}

	public static function type_function_arg_value (ctx:context.Typecore.Typer, t:core.Type.T, c:Option<core.Ast.Expr>, do_display:Bool) : Option<core.Type.TConstant> {
		return switch (c) {
			case None: None;
			case Some(e):
				var p = e.pos;
				var _e = (do_display) ? context.display.ExprPreprocessing.process_expr(ctx.com, e) : e;
				var e = ctx.g.do_optimize(ctx, context.Typecore.type_expr(ctx, _e, WithType(t)));
				context.Typecore.unify(ctx, e.etype, t, p);
				function loop (e:core.Type.TExpr) : Option<core.Type.TConstant> {
					return switch (e.eexpr) {
						case TConst(c): Some(c);
						case TCast(e, None): loop(e);
						case _:
							if (!ctx.com.display.dms_display || ctx.com.display.dms_error_policy == EPCollect) {
								context.Typecore.display_error(ctx, "Parameter default value should be constant", p);
							}
							None;
					}
				}
				loop(e);
		}
	}

	// strict meta
	public static function get_strict_meta (ctx:context.Typecore.Typer, params:ImmutableList<core.Ast.Expr>, pos:core.Globals.Pos) : core.Ast.MetadataEntry {
		trace("TODO: typing.Typeload.get_strict_meta");
		throw false;
	}

	public static function check_strict_meta (ctx:context.Typecore.Typer, metas:core.Ast.Metadata) : core.Ast.Metadata {
		var pf = ctx.com.platform;
		return switch (pf) {
			case Cs, Java:
				var ret = new Ref<core.Ast.Metadata>([]);
				List.iter(function (m:core.Ast.MetadataEntry) {
					switch (m) {
						case {name:Strict, params:params, pos:pos}:
							try {
								ret.set(get_strict_meta(ctx, params, pos) :: ret.get());
							}
							catch (_:ocaml.Exit){}
						case _:
					}
				}, metas);
				ret.get();
			case _: [];
		}
	}
	// end of strict meta handling

	public static function add_constructor (ctx:context.Typecore.Typer, c:core.Type.TClass, force_constructor:Bool, p:core.Globals.Pos) : Void {
		trace("TODO: typing.Typeload.add_constructor");
	}

	public static function check_struct_init_constructor (ctx:context.Typecore.Typer, c:core.Type.TClass, p:core.Globals.Pos) : Void {
		switch (c.cl_constructor) {
			case Some(_):
			case None:
				var params = List.map(function (f) { return f.t; }, c.cl_params);
				var ethis = core.Type.mk(TConst(TThis), TInst(c, params), p);
				var _tmp = List.fold_left(function (arrs:{args:ImmutableList<{v:core.Type.TVar, c:Option<core.Type.TConstant>}>, el:ImmutableList<core.Type.TExpr>, tl:ImmutableList<core.Type.TSignatureArg>}, cf:core.Type.TClassField) {
					var args = arrs.args; var el = arrs.el; var tl = arrs.tl;
					return switch (cf.cf_kind) {
						case Var(_):
							var opt = core.Meta.has(Optional, cf.cf_meta);
							var t = (opt) ? ctx.t.tnull(cf.cf_type) : cf.cf_type;
							var v = core.Type.alloc_var(cf.cf_name, t, p);
							var ef = core.Type.mk(TField(ethis, FInstance(c, params, cf)), t, p);
							var ev = core.Type.mk(TLocal(v), v.v_type, p);
							var e = core.Type.mk(TBinop(OpAssign, ef, ev), ev.etype, p);
							{args:{v:v, c:None}::args, el:e::el, tl:{name:cf.cf_name, opt:opt, t:t}::tl};
						case Method(_): arrs;
					}
				}, {args:[], el:[], tl:[]}, List.rev(c.cl_ordered_fields));
				var args = _tmp.args; var el = _tmp.el; var tl = _tmp.tl;
				var tf:core.Type.TFunc = {
					tf_args: args,
					tf_type: ctx.t.tvoid,
					tf_expr: core.Type.mk(TBlock(el), ctx.t.tvoid, p)
				}
				var e = core.Type.mk(TFunction(tf), TFun({args:tl, ret:ctx.t.tvoid}), p);
				var cf = core.Type.mk_field("new", e.etype, p, core.Globals.null_pos);
				cf.cf_expr = Some(e);
				cf.cf_type = e.etype;
				cf.cf_meta = [{name:CompilerGenerated, params:[], pos:core.Globals.null_pos}];
				cf.cf_kind = Method(MethNormal);
				c.cl_constructor = Some(cf);
		}
	}

	// module Inheritance = struct ...

	public static function type_type_param (?enum_constructor:Bool=false, ctx:context.Typecore.Typer, path:core.Path, get_params:Void->core.Type.TypeParams, p:core.Globals.Pos, tp:core.Ast.TypeParam) : {name:String, t:core.Type.T} {
		var n = tp.tp_name.pack;
		var c = core.Type.mk_class(ctx.m.curmod, new core.Path(List.concat(path.a, [path.b]), n), tp.tp_name.pos, tp.tp_name.pos); 
		c.cl_params = type_type_params(ctx, c.cl_path, get_params, p, tp.tp_params);
		c.cl_kind = KTypeParameter([]);
		c.cl_meta = tp.tp_meta.clone();
		if (enum_constructor) {
			var _tmp:core.Ast.MetadataEntry = {name:EnumConstructorParam, params:[], pos:core.Globals.null_pos};
			c.cl_meta = _tmp::c.cl_meta;
		}
		var t:core.Type.T = TInst(c, List.map(function (p) { return p.t; }, c.cl_params));
		if (ctx.is_display_file && context.Display.is_display_position(tp.tp_name.pos)) {
			context.display.DisplayEmitter.display_type(ctx.com.display, t, tp.tp_name.pos);
		}
		return switch (tp.tp_constraints) {
			case []: {name:n, t:t};
			case _:
				function f (r:Ref<core.Type.TLazy>) : core.Type.T {
					r.set(core.Type.lazy_processing(function() { return t;}));
					var _ctx = ctx.clone();
					_ctx.g = ctx.g;
					_ctx.type_params = List.concat(ctx.type_params, get_params());
					var constr = List.map( function (e) { return load_complex_type(_ctx, true, p, e);}, tp.tp_constraints);
					// check against direct recursion
					function loop (t) {
						switch (core.Type.follow(t)) {
							case TInst(c2,_):
								if (c.equals(c2)) {
									core.Error.error("Recursive constraint parameter is not allowed", p);
								}
								switch (c2.cl_kind) {
									case KTypeParameter(cl):
										ocaml.List.iter(loop, cl);
									default:
								}
							default:
						}
					}
					ocaml.List.iter(loop, constr);
					c.cl_kind = KTypeParameter(constr);
					return t;
				}
				var r = context.Typecore.exc_protect(ctx, f, "constraint");
				{name:n, t:TLazy(r)};
		}
	}

	public static function type_type_params (?enum_constructor:Bool=false, ctx:context.Typecore.Typer, path:core.Path, get_params:Void->core.Type.TypeParams, p:core.Globals.Pos, tpl:ImmutableList<core.Ast.TypeParam>) : core.Type.TypeParams {
		var names:ImmutableList<String> = [];
		function f (tp:core.Ast.TypeParam) {
			if (List.exists(function (name) { return name == tp.tp_name.pack; }, names)) {
				context.Typecore.display_error(ctx, "Duplicate type parameter name: " + tp.tp_name.pack, tp.tp_name.pos);
			}
			names = tp.tp_name.pack :: names;
			return type_type_param(enum_constructor, ctx, path, get_params, p, tp);
		}
		return List.map(f, tpl);
	}

	public static function type_function_params (ctx:context.Typecore.Typer, fd:core.Ast.Func, fname:String, p:core.Globals.Pos) : core.Type.TypeParams {
		var params = new Ref<core.Type.TypeParams>([]);
		params.set(type_type_params(ctx, new core.Path([], fname), function () { return params.get(); }, p, fd.f_params));
		return params.get();
	}

	public static function save_function_state(ctx:context.Typecore.Typer) : Void->Void {
		var old_ret = ctx.ret.clone();
		var old_fun = ctx.curfun.clone();
		var old_opened = ctx.opened.clone();
		var locals = ctx.locals.clone();
		return function () {
			ctx.locals = locals;
			ctx.ret = old_ret;
			ctx.curfun = old_fun;
			ctx.opened = old_opened;
		}
	}

	public static function type_function_ (ctx:context.Typecore.Typer, args:ImmutableList<{name:String, opt:Option<core.Ast.Expr>, t:core.Type.T}>, ret:core.Type.T, fmode:context.Typecore.CurrentFun, f:core.Ast.Func, do_display:Bool, p:core.Globals.Pos) : {fst:core.Type.TExpr, snd:ImmutableList<{v:core.Type.TVar, c:Option<core.Type.TConstant>}>} {
		var fargs = List.map2(function (a, b) {
			var n:String = a.name; var c = a.opt; var t = a.t;
			var pn = b.name.pos; var m = b.meta;
			if (n.charAt(0) == "$") {
				core.Error.error("Function argument names starting with a dollar are not allowed", p);
			}
			var c = type_function_arg_value(ctx, t, c, do_display);
			var v = context.Typecore.add_local(ctx, n, t, p);
			v.v_meta = m;
			if (do_display && context.Display.is_display_position(pn)) {
				context.display.DisplayEmitter.display_variable(ctx.com.display, v, pn);
				if (n == "this") {
					var _tmp:core.Ast.MetadataEntry = {name:This, params:[], pos:core.Globals.null_pos};
					v.v_meta = _tmp :: v.v_meta;
				}
			}
			return {v:v, c:c};
		}, args, f.f_args);
		ctx.curfun = fmode;
		ctx.ret = ret;
		ctx.opened = [];
		var _e:core.Ast.Expr = switch (f.f_expr) {
			case None:
				if (ctx.com.display.dms_error_policy == EPIgnore) {
					/* when we don't care because we're in display mode, just act like
					the function has an empty block body. this is fine even if function
					defines a return type, because returns aren't checked in this mode
					*/
					{expr:EBlock([]),pos:p};
				}
				else {
					core.Error.error("Function body required", p);
				}
			case Some(e): e;
		}
		var e = if (!do_display) {
			context.Typecore.type_expr(ctx, _e, NoValue);
		}
		else {
			var e = context.display.ExprPreprocessing.process_expr(ctx.com, _e);
			try {
				if (context.Common.defined(ctx.com, NoCOpt)) {
					throw ocaml.Exit.instance;
				}
				context.Typecore.type_expr(ctx, optimization.Optimizer.optimize_completion_expr(e), NoValue);
			}
			catch (_:ocaml.Exit) { context.Typecore.type_expr(ctx, e, NoValue); }
			catch (err:syntax.parser.TypePath) {
				if (err.c == None) {
					context.Typecore.type_expr(ctx, e, NoValue);
				}
				else {
					throw err;
				}
			}
			catch (err:context.Display.DisplayException) {
				switch (err) {
					case DisplayType(t,_,_) if (switch (core.Type.follow(t)) { case TMono(_): true; case _: false;}):
						var _tmp = (ctx.com.display.dms_kind == DMToplevel) ? context.display.ExprPreprocessing.find_enclosing(ctx.com, e) : e;
						context.Typecore.type_expr(ctx, _tmp, NoValue);
					case _:
						throw err;
				}
			}
		}

		e = switch (e.eexpr) {
			case TMeta({name:MergeBlock}, e1={eexpr:TBlock(el)}) : e1;
			case _: e;
		}
		function has_return(e:core.Type.TExpr) : Bool {
			function loop (e:core.Type.TExpr) {
				return switch (e.eexpr) {
					case TReturn(Some(_)): throw ocaml.Exit.instance;
					case TFunction(_):
					case _: core.Type.iter(loop, e);
				}
			}
			try {
				loop(e);
				return false;
			}
			catch (_:ocaml.Exit) {
				return true;
			}
		}
		switch (core.Type.follow(ret)) {
			case TAbstract({a_path:{a:[], b:"Void"}}, _):
			/* We have to check for the presence of return expressions here because
				in the case of Dynamic ctx.ret is still a monomorph. If we indeed
				don't have a return expression we can link the monomorph to Void. We
				can _not_ use type_iseq to avoid the Void check above because that
				would turn Dynamic returns to Void returns. */
			case TMono(t) if (!has_return(e)):
				core.Type.link(t, ret, ctx.t.tvoid);
			case _ if (ctx.com.display.dms_error_policy == EPIgnore):
			case _:
				try {
					return_flow(ctx, e);
				}
				catch (_:ocaml.Exit) {}
		}
		function loop (e:core.Type.TExpr) {
			switch (e.eexpr) {
				case TCall({eexpr:TConst(TSuper)}, _): throw ocaml.Exit.instance;
				case TFunction(_):
				case _: core.Type.iter(loop, e);
			}
		}
		function has_super_constr() : Option<{fst:Bool, snd:core.Type.T}> {
			return switch (ctx.curclass.cl_super) {
				case None: None;
				case Some({c:csup, params:tl}):
					try {
						var cf = core.Type.get_constructor(function (f) { return f.cf_type; }, csup).snd;
						Some({fst:core.Meta.has(CompilerGenerated, cf.cf_meta), snd:TInst(csup,tl)});
					}
					catch (_:ocaml.Not_found) {
						None;
					}
			}
		}
		e = if (fmode != FunConstructor) { e; }
		else {
			var final_vars = new Map<String, core.Type.TClassField>();
			List.iter(function (cf:core.Type.TClassField) {
				switch (cf.cf_kind) {
					case Var(_) if (core.Meta.has(Final, cf.cf_meta) && cf.cf_expr==None):
						Hashtbl.add(final_vars, cf.cf_name, cf);
					case _:
				}
			}, ctx.curclass.cl_ordered_fields);
			if (Hashtbl.length(final_vars) > 0) {
				function find_inits(e:core.Type.TExpr) {
					switch (e.eexpr) {
						case TBinop(OpAssign, {eexpr:TField({eexpr:TConst(TThis)}, fa)}, e2):
							final_vars.remove(core.Type.field_name(fa));
							find_inits(e2);
						case _:
							core.Type.iter(find_inits, e);
					}
				}
				find_inits(e);
				Hashtbl.iter(function (_, cf) {
					context.Typecore.display_error(ctx, "final field "+cf.cf_name+" must be initialized immediately or in the constructor", cf.cf_pos);
				}, final_vars);
			}
			switch (has_super_constr()) {
				case Some({fst:was_forced, snd:t_super}):
					try {
						loop(e);
						if (was_forced) {
							var e_super = core.Type.mk(TConst(TSuper), t_super, e.epos);
							var e_super_call = core.Type.mk(TCall(e_super, []), ctx.t.tvoid, e.epos);
							core.Type.concat(e_super_call, e);
						}
						else {
							context.Typecore.display_error(ctx, "Missing super constructor call", p);
							e;
						}
					}
					catch (_:ocaml.Exit) {
						e;
					}
				case None: e;
			}
		}
		e = switch({f:ctx.curfun, s:ctx.vthis}) {
			case {f:(FunMember|FunConstructor), s:Some(v)}:
				var ev = core.Type.mk(TVar(v, Some(core.Type.mk(TConst(TThis), ctx.tthis, p))), ctx.t.tvoid, p);
				switch (e.eexpr) {
					case TBlock(l):
						var _e = e.clone();
						_e.eexpr = TBlock([ev].concat(l));
						_e;
					case _: core.Type.mk(TBlock([ev, e]), e.etype, p);
				}
			case _: e;
		}
		List.iter(function (r:Ref<core.Type.AnonStatus>) {
			r.set(Closed);
		}, ctx.opened);
		return {fst:e, snd:fargs};
	}

	public static function type_function (ctx:context.Typecore.Typer, args:ImmutableList<{name:String, opt:Option<core.Ast.Expr>, t:core.Type.T}>, ret:core.Type.T, fmode:context.Typecore.CurrentFun, f:core.Ast.Func, do_display:Bool, p:core.Globals.Pos) : {fst:core.Type.TExpr, snd:ImmutableList<{v:core.Type.TVar, c:Option<core.Type.TConstant>}>} {
		var save = save_function_state(ctx);
		try {
			var _tmp = type_function_(ctx, args, ret, fmode, f, do_display, p);
			save();
			return _tmp;
		}
		catch (err:Any) {
			save();
			throw err;
		}
	}

	public static function load_core_class (ctx:context.Typecore.Typer, c:core.Type.TClass) {
		var ctx2 = switch (ctx.g.core_api) {
			case None:
				var com2 = context.Common.clone(ctx.com);
				com2.defines.values = new Map<String, String>();
				context.Common.define(com2, CoreApi);
				context.Common.define(com2, Sys);
				if (ctx.in_macro) {
					context.Common.define(com2, Macro);
				}
				com2.class_path = ctx.com.std_path.clone();
				var ctx2 = ctx.g.do_create(com2);
				ctx.g.core_api = Some(ctx2);
				ctx2;
			case Some(c):
				c;
		}
		var tpath:core.Ast.TypePath = switch (c.cl_kind) {
			case KAbstractImpl(a):
				{tpackage:a.a_path.a, tname:a.a_path.b, tparams:[], tsub:None};
			case _:
				{tpackage:c.cl_path.a, tname:c.cl_path.b, tparams:[], tsub:None};
		}
		var t = load_instance(ctx2, {tp:tpath, pos:c.cl_pos}, true, c.cl_pos);
		context.Typecore.flush_pass(ctx2, PFinal, "core_final");
		return switch (t) {
			case TInst(ccore, _), TAbstract({a_impl:Some(ccore)}, _): ccore;
			case _: throw false;
		};
	}

	public static function init_core_api (ctx:context.Typecore.Typer, c:core.Type.TClass) : Void {
		var ccore = load_core_class(ctx, c);
		try {
			List.iter2(function (tp1, tp2) {
				var n1 = tp1.name; var t1 = tp1.t;
				var n2 = tp2.name; var t2 = tp2.t;
				switch ({fst:core.Type.follow(t1), snd:core.Type.follow(t2)}) {
					case {fst:TInst({cl_kind:KTypeParameter(l1)}, _), snd:TInst({cl_kind:KTypeParameter(l2)}, _)}:
						try {
							List.iter2(function (t1, t2) { return core.Type.type_eq(EqCoreType, t2, t1); }, l1, l2);
						}
						catch (_:ocaml.Invalid_argument) {
							core.Error.error("Type parameters must have the same number of constraints as core type", c.cl_pos);
						}
						catch (u:core.Type.Unify_error) {
							var l = u.l;
							context.Typecore.display_error(ctx, "Type parameter "+n2+ " has different constraint than in core type", c.cl_pos);
							context.Typecore.display_error(ctx, core.Error.error_msg(Unify(l)), c.cl_pos);
						}
					case {fst:t1, snd:t2}:
						Sys.print(core.Type.s_type(core.Type.print_context(), t1) + " " + core.Type.s_type(core.Type.print_context(), t2));
						throw false;
				}
			}, ccore.cl_params, c.cl_params);
		}
		catch (_:ocaml.Invalid_argument) {
			core.Error.error("Class must have the same number of type parameters as core type", c.cl_pos);
		}
		switch (c.cl_doc) {
			case None: c.cl_doc = ccore.cl_doc;
			case Some(_):
		}
		function compare_fields(f:core.Type.TClassField, f2:core.Type.TClassField) {
			var p = switch (f2.cf_expr) {
				case None: c.cl_pos;
				case Some(e): e.epos;
			}
			try {
				core.Type.type_eq(EqCoreType, core.Type.apply_params(ccore.cl_params, List.map(function (a) {return a.t;}, c.cl_params), f.cf_type), f2.cf_type);
			}
			catch (u:core.Type.Unify_error) {
				var l = u.l;
				context.Typecore.display_error(ctx, "Field "+f.cf_name+ " has different type than in core type", p);
				context.Typecore.display_error(ctx, core.Error.error_msg(Unify(l)), p);
			}
			if (f2.cf_public != f.cf_public) {
				switch ({fst:f2.cf_kind, snd:f.cf_kind}) {
					case {fst:Method(MethInline), snd:Method(MethNormal)}: // allow to add 'inline'
					case {fst:Method(MethNormal), snd:Method(MethInline)}: // allow to disable 'inline'
					case _:
						core.Error.error("Field "+f.cf_name+ " has different visibility than core type", p);
				}
			}
			switch ({fst:core.Type.follow(f.cf_type), snd:core.Type.follow(f2.cf_type)}) {
				case {fst:TFun({args:pl1}), snd:TFun({args:pl2})}:
					if (List.length(pl1) != List.length(pl2)) {
						core.Error.error("Argument count mismatch", p);
					}
					List.iter2(function (arg1, arg2) {
						if (arg1.name != arg2.name) {
							core.Error.error("Method parameter name '"+arg2.name+"' should be '"+arg1.name+"'", p);
						}
					}, pl1, pl2);
				case _:
			}
		}
		function check_fields (fcore:Map<String,core.Type.TClassField>, fl:Map<String,core.Type.TClassField>) {
			PMap.iter(function (i, f) {
				if (f.cf_public) {
					var f2 = try {
						PMap.find(f.cf_name, fl);
					}
					catch (_:ocaml.Not_found) {
						core.Error.error("Missing field " + i + " required by core type", c.cl_pos);
					}
					compare_fields(f, f2);
				}
			}, fcore);
			PMap.iter(function (i, f) {
				var p = switch (f.cf_expr) { case None: c.cl_pos; case Some(e): e.epos;}
				if (f.cf_public && !core.Meta.has(Hack, f.cf_meta) && !PMap.mem(f.cf_name, fcore) && !ocaml.List.memq(f, c.cl_overrides)) {
					core.Error.error("Public field " + i + " is not part of core type", p);
				}
			}, fl);

		}
		check_fields(ccore.cl_fields, c.cl_fields);
		check_fields(ccore.cl_statics, c.cl_statics);
		switch ({fst:ccore.cl_constructor, snd:c.cl_constructor}) {
			case {fst:None, snd:None}:
			case {fst:Some({cf_public:false})}:
			case {fst:Some(f), snd:Some(f2)}: compare_fields(f, f2);
			case {fst:None, snd:Some({cf_public:false})}:
			case _: core.Error.error("Constructor differs from core type", c.cl_pos);
		}
	}

	public static function check_global_metadata (ctx:context.Typecore.Typer, meta:core.Ast.Metadata, f_add:core.Ast.MetadataEntry->Void, mpath:core.Path, tpath:core.Path, so:Option<String>) {
		var sl1 = core.Ast.full_dot_path(mpath, tpath);
		var field_mode = switch (so) {
			case None:
				false;
			case Some(s):
				List.concat(sl1, [s]);
				true;
		}
		List.iter(function (gm) {
			var sl2 = gm.l; var m = gm.me; var recursive = gm.bs.a; var to_types = gm.bs.b; var to_fields = gm.bs.c;
			var add = ((field_mode && to_fields) || (!field_mode && to_types)) && (core.Ast.match_path(recursive, sl1, sl2));
			if (add) { f_add(m); }
		}, ctx.g.global_metadata);
		if (ctx.is_display_file) {
			context.Typecore.delay(ctx, PCheckConstraint, function() {
				context.display.DisplayEmitter.check_display_metadata(ctx, meta);
			});
		}
	}

	public static function patch_class (ctx:context.Typecore.Typer, c:core.Type.TClass, fields:ImmutableList<core.Ast.ClassField>) : ImmutableList<core.Ast.ClassField>{
		var path = switch (c.cl_kind) {
			case KAbstractImpl(a): a.a_path;
			case _: c.cl_path;
		}
		var h = try {
			Some(ocaml.Hashtbl.find(ctx.g.type_patches, path));
		}
		catch (_:ocaml.Not_found) {
			None;
		};
		return switch (h) {
			case None: fields;
			case Some(v):
				var h = v.map; var hcl = v.tp;
				c.cl_meta = List.concat(c.cl_meta, hcl.tp_meta);
				function loop (acc:ImmutableList<core.Ast.ClassField>, l:ImmutableList<core.Ast.ClassField>) {
					return switch(l) {
						case []: acc;
						case f::l:
							// patch arguments types
							switch (f.cff_kind) {
								case FFun(ff):
									function param (p:core.Ast.FunArg) : core.Ast.FunArg {
										try {
											var t2 = try {
												ocaml.Hashtbl.find(h, {s:"$"+f.cff_name.pack+ "__"+p.name.pack, b:false});
											}
											catch (_:ocaml.Not_found) {
												ocaml.Hashtbl.find(h, {s:"$"+p.name.pack, b:false});
											}
											return {name:p.name.clone(), opt:p.opt, meta:p.meta, type:switch (t2.tp_type) { case None: None; case Some(t): Some({ct:t, pos:core.Globals.null_pos});}, value:p.value};
										}
										catch (_:ocaml.Not_found) {
											return p;
										}
									}
									var _ff = ff.clone();
									_ff.f_args = List.map(param, ff.f_args);
									f.cff_kind = FFun(_ff);
								case _:
							}
							// other patches
							var match = try {
								var _tmp:core.Ast.Access = AStatic;
								Some(ocaml.Hashtbl.find(h, {s:f.cff_name.pack, b:ocaml.List.mem(_tmp, f.cff_access)}));
							}
							catch (_:ocaml.Not_found) { None; }
							return switch (match) {
								case None: loop([f].concat(acc), l);
								case Some({tp_remove:true}): loop(acc, l);
								case Some(p):
									f.cff_meta = List.concat(f.cff_meta, p.tp_meta);
									switch (p.tp_type) {
										case None:
										case Some(t):
											f.cff_kind = switch (f.cff_kind) {
												case FVar(_, e): FVar(Some({ct:t, pos:core.Globals.null_pos}), e);
												case FProp(get, set, _, eo): FProp(get, set, Some({ct:t, pos:core.Globals.null_pos}), eo);
												case FFun(f):
													var _f = f.clone();
													_f.f_type = Some({ct:t, pos:core.Globals.null_pos});
													FFun(_f);
											}
									}
									loop([f].concat(acc), l);
							}
					}
				}
				ocaml.List.rev(loop([], fields));
		};
	}
	
	public static function string_list_of_expr_path (expr:core.Ast.Expr) : ImmutableList<String> {
		return try {
			core.Ast.string_list_of_expr_path_raise(expr);
		}
		catch (_:ocaml.Exit) {
			core.Error.error("Invalid path", expr.pos);
		}
	}

	public static function build_enum_abstract (ctx:context.Typecore.Typer, c:core.Type.TClass, a:core.Type.TAbstract, fields:ImmutableList<core.Ast.ClassField>, p:core.Globals.Pos) : core.Ast.Expr {
		List.iter(function(field:core.Ast.ClassField) {
			switch (field.cff_kind) {
				case FVar(ct, eo) if (!List.mem(core.Ast.Access.AStatic, field.cff_access)):
					field.cff_access = [AStatic, (List.mem(core.Ast.Access.APrivate, field.cff_access)) ? APrivate : APublic];
					field.cff_meta = ({name:Enum, params:[], pos:core.Globals.null_pos}:core.Ast.MetadataEntry) :: ({name:Impl, params:[], pos:core.Globals.null_pos}:core.Ast.MetadataEntry) :: field.cff_meta;
					var ct = switch (ct) {
						case Some(_): ct;
						case None: Some(core.type.TExprToExpr.convert_type_(TAbstract(a, List.map(function (o) {return o.t; }, a.a_params))));
					}
					switch (eo) {
						case None:
							if (!c.cl_extern) {
								core.Error.error("Value required", field.cff_pos);
							}
							else {
								field.cff_kind = FProp({pack:"default", pos:core.Globals.null_pos}, {pack:"never", pos:core.Globals.null_pos}, ct, None);
							}
						case Some(e):
							field.cff_access = core.Ast.Access.AInline :: field.cff_access;
							var e:core.Ast.Expr = {expr:ECast(e, None), pos:e.pos};
							field.cff_kind = FVar(ct, Some(e));
					}
				case _:
			}
		}, fields);
		return {expr:EVars([{name:{pack:"", pos:core.Globals.null_pos}, type:Some({ct:CTAnonymous(fields), pos:p}), expr:None}]), pos:p};
	}

	public static function is_java_native_function (meta:core.Ast.Metadata) : Bool {
		try {
			return switch (core.Meta.get(Native, meta)) {
				case {name:Native, params:[]}: true;
				case _: false;
			}
		}
		catch (_:ocaml.Not_found) {
			return false;
		}
	}

	public static function build_module_def (ctx:context.Typecore.Typer, mt:core.Type.ModuleType, meta:core.Ast.Metadata, fvars:Void->ImmutableList<core.Ast.ClassField>, context_init:Void->Void, fbuild:core.Ast.Expr->Void) {
		function loop (fs:{fst:ImmutableList<Void->Void>, snd:Option<Void->Void>}, meta:core.Ast.MetadataEntry) : {fst:ImmutableList<Void->Void>, snd:Option<Void->Void>} {
			var f_build = fs.fst; var f_enum = fs.snd;
			// var args = meta.params; var p = meta.pos;
			return switch (meta) {
				case {name:Build, params:args, pos:p}:
					var f = function () {
						var _tmp = switch (args) {
							case [{expr:ECall(epath, el), pos:p}]: {fst:epath, snd:el};
							case _:
								core.Error.error("Invalid build parameters", p);
						}
						var epath = _tmp.fst; var el = _tmp.snd;
						var s = try {
							List.join(".",List.rev(string_list_of_expr_path(epath)));
						}
						catch (err:core.Error) {
							core.Error.error("Build call parameter must be a class path", err.pos);
						}
						if (ctx.in_macro) {
							core.Error.error("You cannot use @:build inside a macro : make sure that your type is not used in macro", p);
						}
						var old = ctx.g.get_build_infos;
						ctx.g.get_build_infos = function () { return Some({mt:mt, l:List.map(function (a) { return a.t; }, core.Type.t_infos(mt).mt_params), cfs:fvars()}); };
						context_init();
						var r = try {
							apply_macro(ctx, MBuild, s, el, p);
						}
						catch (e:Any) {
							ctx.g.get_build_infos = old;
							throw e;
						}
						ctx.g.get_build_infos = old;
						switch (r) {
							case None: core.Error.error("Build failure", p);
							case Some(e): fbuild(e);
						}
					}
					{fst:f::f_build, snd:f_enum};
				case {name:Enum, pos:p}:
					var f_e = function () {
						switch (mt) {
							case TClassDecl(c={cl_kind:KAbstractImpl(a)}):
								context_init();
								var e = build_enum_abstract(ctx, c, a, fvars(), p);
								fbuild(e);
							case _:
						}
					}
					{fst:f_build, snd:Some(f_e)};
				case _:
					{fst:f_build, snd:f_enum};
			}
		}
		// let errors go through to prevent resume if build fails
		var _tmp = List.fold_left(loop, {fst:[], snd:None}, meta);
		var f_build = _tmp.fst; var f_enum = _tmp.snd;
		List.iter(function (f) {
			f();
		}, List.rev(f_build));
		switch (f_enum) {
			case None:
			case Some(f): f();
		}
	}

	// module ClassInitializer
	// end module

	public static function check_module_types(ctx:context.Typecore.Typer, m:core.Type.ModuleDef, p:core.Globals.Pos, t:core.Type.ModuleType) : Void {
		var t = core.Type.t_infos(t);
		try {
			var m2 = Hashtbl.find(ctx.g.types_module, t.mt_path);
			if (!m.m_path.equals(m2) && core.Globals.s_type_path(m2).toLowerCase() == core.Globals.s_type_path(m.m_path).toLowerCase()) {
				core.Error.error("Module " + core.Globals.s_type_path(m2) + " is loaded with a different case than " + core.Globals.s_type_path(m.m_path), p);
			}
		}
		catch (_:ocaml.Not_found) {
			Hashtbl.add(ctx.g.types_module, t.mt_path, m.m_path);
		}
	}

	public static function add_module (ctx:context.Typecore.Typer, m:core.Type.ModuleDef, p:core.Globals.Pos) : Void {
		function f(t:core.Type.ModuleType) {
			return check_module_types(ctx, m, p, t);
		}
		List.iter(f, m.m_types);
		ctx.g.modules.set(m.m_path, m);
	}

	public static function handle_path_display (ctx:context.Typecore.Typer, path:ImmutableList<core.Ast.PlacedName>, p:core.Globals.Pos) {
		trace("TODO: typing.Typeload.handle_path_display");
	}

	/*
	 * In this pass, we can access load and access other modules types, but we cannot follow them or access their structure
	 * since they have not been setup. We also build a context_init list that will be evaluated the first time we evaluate
	 * an expression into the context
	 */
	public static function init_module_type (ctx:context.Typecore.Typer, context_init:Ref<ImmutableList<Void->Void>>, do_init:Void->Void, last:core.Ast.TypeDecl) {
		var decl = last.decl;
		var p = last.pos;
		function get_type(name:String) : core.Type.ModuleType {
			try {
				return List.find(function(t) {
					return core.Type.t_infos(t).mt_path.b == name;
				}, ctx.m.curmod.m_types);
			}
			catch (_:ocaml.Not_found) {
				throw false;
			}
		}
		function check_path_display (path:ImmutableList<core.Ast.PlacedName>, p:core.Globals.Pos) {
			// We cannot use ctx.is_display_file because the import could come from an import.hx file.
			switch (ctx.com.display.dms_kind) {
				case DMDiagnostics(b):
					if (b || context.Display.is_display_file(p.pfile) && !p.pfile.endsWith("import.hx")) {
						context.display.ImportHandling.add_import_position(ctx.com, p, path);
						return;
					}
				case DMStatistics, DMUsage(_):
					context.display.ImportHandling.add_import_position(ctx.com, p, path);
					return;
				case _:
			}
			if (context.Display.is_display_file(p.pfile)) {
				 handle_path_display(ctx, path, p);
			}
		}
		switch (decl) {
			case EImport(i):
				var path = i.pns;
				var mode = i.mode;
				ctx.m.module_imports = {pns:path, mode:mode} :: ctx.m.module_imports;
				check_path_display(path, p);
				function loop (acc:ImmutableList<core.Ast.PlacedName>, l:ImmutableList<core.Ast.PlacedName>) {
					return switch (l) {
						case x::l if (core.Ast.is_lower_ident(x.pack)):
							loop(x::acc, l);
						case rest:
							{fst:List.rev(acc), snd:rest};
					}
				}
				var _tmp = loop([], path);
				var pack = _tmp.fst; var rest = _tmp.snd;
				switch (rest) {
					case []:
						switch (mode) {
							case IAll:
								ctx.m.wildcard_packages = {l:List.map(function (e) {return e.pack;}, pack),pos:p} :: ctx.m.wildcard_packages;
							case _:
								switch (List.rev(path)) {
									case []:
										throw context.Display.DisplayException.DisplayToplevel(context.DisplayToplevel.collect(ctx, true));
									case {pos:p}::_:
										core.Error.error("Module name must start with an uppercase letter", p);
								}
						}
					case {pack:tname, pos:p2}::rest:
						var p1 = switch (pack) {
							case []: p2;
							case {pos:p1}::_: p1;
						}
						var p_type = core.Ast.punion(p1, p2);
						var md = ctx.g.do_load_module(ctx, new core.Path(List.map(function (p) {return p.pack;}, pack), tname) , p_type);
						var types = md.m_types;
						function no_private (t:{mt:core.Type.ModuleType, pos:core.Globals.Pos}) : Bool {
							return !(core.Type.t_infos(t.mt).mt_private);
						}
						function chk_private (t:core.Type.ModuleType, p:core.Globals.Pos) {
							if (core.Type.t_infos(t).mt_private) {
								core.Error.error("You can't import a private type", p);
							}
						}
						function has_name (name:String, t:core.Type.ModuleType) : Bool {
							return core.Type.t_infos(t).mt_path.b == name;
						}
						function get_type (tname:String) : core.Type.ModuleType {
							var t = try {
								List.find(has_name.bind(tname), types);
							}
							catch (_:ocaml.Not_found) {
								var s = core.type.StringError.string_error(tname, List.map(function(mt:core.Type.ModuleType) {return core.Type.t_infos(mt).mt_path.b;}, types), "Module " + core.Globals.s_type_path(md.m_path) + " does not define type " + tname);
								core.Error.error(s, p_type);
							}
							chk_private(t, p_type);
							return t;
						}
						function rebind (t:core.Type.ModuleType, name:String) : core.Type.ModuleType {
							if (!(name.charCodeAt(0) >= "A".code && name.charCodeAt(0) <= "Z".code)) {
								core.Error.error("Type aliases must start with an uppercase letter", p);
							}
							var f = ctx.g.do_build_instance(ctx, t, p_type).f;
							// create a temp private typedef, does not register it in module
							return TTypeDecl({
								t_path: new core.Path(List.concat(md.m_path.a,["_"+md.m_path.b]), name),
								t_module: md,
								t_pos: p,
								t_name_pos: core.Globals.null_pos,
								t_private: true,
								t_doc: None,
								t_meta: [],
								t_params: core.Type.t_infos(t).mt_params,
								t_type: f(List.map(function (p) {return p.t;}, core.Type.t_infos(t).mt_params))
							});
						}
						function add_static_init (t:core.Type.ModuleType, name:Option<String>, s:String) {
							var name = switch (name) {
								case None: s;
								case Some(n): n;
							}
							switch (core.Type.resolve_typedef(t)) {
								case TClassDecl(c):
									c.cl_build();
									PMap.find(s, c.cl_statics);
									ctx.m.module_globals.set(name, {a:TClassDecl(c), b:s, pos:p});
								case TEnumDecl(e):
									PMap.find(s, e.e_constrs);
									ctx.m.module_globals.set(name, {a:TEnumDecl(e), b:s, pos:p});
								case _: throw ocaml.Not_found.instance;
							}
						}
						switch (mode) {
							case INormal, IAsName(_):
								var name = switch (mode) { case IAsName(n): Some(n); case _: None; };
								switch (rest) {
									case []:
										switch (name) {
											case None:
												ctx.m.module_types = List.filter(no_private, List.concat(List.map(function (t) { return {mt:t, pos:p}; }, types), ctx.m.module_types));
											case Some(newname):
												ctx.m.module_types = {mt:rebind(get_type(tname), newname), pos:p} :: ctx.m.module_types;
										}
									case [{pack:tsub, pos:p2}]:
										var pu = core.Ast.punion(p1, p2);
										try {
											var tsub = List.find(has_name.bind(tsub), types);
											chk_private(tsub, pu);
											ctx.m.module_types = {mt:switch(name){
												case None: tsub;
												case Some(n): rebind(tsub, n);
											}, pos:p} :: ctx.m.module_types;
										}
										catch (_:ocaml.Not_found) {
											// this might be a static property, wait later to check
											var tmain = get_type(tname);
											context_init.set(function() {
												try {
													add_static_init(tmain, name, tsub);
												}
												catch (_:ocaml.Not_found) {
													core.Error.error(core.Globals.s_type_path(core.Type.t_infos(tmain).mt_path) + "has no field or subtype "+tsub, p);
												}
											}::context_init.get());
										}
									case {pack:tsub, pos:p2}::{pack:fname, pos:p3}::rest:
										switch (rest) {
											case []:
											case {pack:n, pos:p}::_:
												core.Error.error("Unexpected "+n, p);
										}
										var tsub = get_type(tsub);
										context_init.set(function () {
											try {
												add_static_init(tsub, name, fname);
											}
											catch (_:ocaml.Not_found) {
												core.Error.error(core.Globals.s_type_path(core.Type.t_infos(tsub).mt_path)+" has no field "+fname, core.Ast.punion(p, p3));
											}
										}::context_init.get());
								}
							case IAll:
								var t = switch (rest) {
									case []: get_type(tname);
									case [{pack:tsub}]: get_type(tsub);
									case _::{pack:n, pos:p}::_:
										core.Error.error("Unexpected "+n, p);
								}
								context_init.set(function() {
									switch (core.Type.resolve_typedef(t)) {
										case TClassDecl(c):
											c.cl_build;
											PMap.iter( function (_, cf){
												if (!core.Type.has_meta(NoImportGlobal, cf.cf_meta)) {
													ctx.m.module_globals.set(cf.cf_name, {a:TClassDecl(c), b:cf.cf_name, pos:p});
												}
											}, c.cl_statics);
										case TAbstractDecl(a):
											switch (a.a_impl) {
												case Some(c):
													c.cl_build;
													PMap.iter( function (_, cf){
														if (!core.Type.has_meta(NoImportGlobal, cf.cf_meta)) {
															ctx.m.module_globals.set(cf.cf_name, {a:TClassDecl(c), b:cf.cf_name, pos:p});
														}
													}, c.cl_statics);
												case None:
													core.Error.error("No statics to import from this type", p);
											}
										case TEnumDecl(e):
											PMap.iter( function (_, cf){
												if (!core.Type.has_meta(NoImportGlobal, cf.ef_meta)) {
													ctx.m.module_globals.set(cf.ef_name, {a:TEnumDecl(e), b:cf.ef_name, pos:p});
												}
											}, e.e_constrs);
										case _: core.Error.error("No statics to import from this type", p);
									}
								}::context_init.get());
						}
				}
			case EUsing(path):
				check_path_display(path, p);
				var t:core.Ast.TypePath = switch(List.rev(path)) {
					case {pack:s1}::{pack:s2}::sl:
						if (core.Ast.is_lower_ident(s2)) {
							{
								tpackage: List.rev(s2::List.map(function (pn) { return pn.pack; }, sl)),
								tname: s1,
								tsub: None,
								tparams: []
							};
						}
						else {
							{
								tpackage: List.rev(List.map(function (pn) { return pn.pack; }, sl)),
								tname: s2,
								tsub: Some(s1),
								tparams: []
							};
						}
					case {pack:s1}::sl:
						{
							tpackage: List.rev(List.map(function (pn) { return pn.pack; }, sl)),
							tname: s1,
							tsub: None,
							tparams: []
						};
					case []:
						throw context.Display.DisplayException.DisplayToplevel(context.DisplayToplevel.collect(ctx, true));
				}
				// do the import first
				var types:ImmutableList<core.Type.ModuleType> = switch (t.tsub) {
					case None:
						var md = ctx.g.do_load_module(ctx, new core.Path(t.tpackage, t.tname), p);
						var types = List.filter(function (t) { return !core.Type.t_infos(t).mt_private; }, md.m_types);
						ctx.m.module_types = List.concat(List.map(function (t) { return {mt:t, pos:p}; }, types),ctx.m.module_types);
						types;
					case Some(_):
						var t = load_type_def(ctx, p, t);
						ctx.m.module_types = {mt:t, pos:p} :: ctx.m.module_types;
						[t];
				}
				// delay the using since we need to resolve typedefs
				function filter_classes (types:ImmutableList<core.Type.ModuleType>) : ImmutableList<{tc:core.Type.TClass, pos:core.Globals.Pos}> {
					function loop(acc, types:ImmutableList<core.Type.ModuleType>) : ImmutableList<{tc:core.Type.TClass, pos:core.Globals.Pos}> {
						return switch (types) {
							case td::l:
								switch (core.Type.resolve_typedef(td)) {
									case TClassDecl(c), TAbstractDecl({a_impl:Some(c)}):
										loop({tc:c, pos:p}::acc, l);
									case _:
										loop(acc, l);
								}
							case []: acc;
						}
					}
					return loop([], types);
				}
				context_init.set(function () {
					ctx.m.module_using = List.concat(filter_classes(types), ctx.m.module_using);
				}::context_init.get());
			case EClass(d):
				var c = switch (get_type(d.d_name.pack)) { case TClassDecl(c): c; case _: throw false;};
				if (ctx.is_display_file && context.Display.is_display_position(d.d_name.pos)) {
					context.display.DisplayEmitter.display_module_type(ctx.com.display, switch (c.cl_kind) { case KAbstractImpl(a): TAbstractDecl(a); case _: TClassDecl(c); } ,d.d_name.pos);
				}
				check_global_metadata(ctx, c.cl_meta, function (m) {
					c.cl_meta = m :: c.cl_meta;
				}, c.cl_module.m_path, c.cl_path, None);
				var herits = d.d_flags;
				c.cl_extern = List.mem(core.Ast.ClassFlag.HExtern, herits);
				c.cl_interface = List.mem(core.Ast.ClassFlag.HInterface, herits);
				var prev_build_count = new Ref(build_count.get() -1);
				function build () {
					var fl = typing.typeload.Inheritance.set_heritance(ctx, c, herits, p);
					function build () : core.Type.BuildState {
						c.cl_build = function () { return Building([c]); };
						try {
							List.iter(function(f) { f(); }, fl);
							typing.typeload.ClassInitializer.init_class(ctx, c, p, do_init, d.d_flags, d.d_data);
							c.cl_build = function () { return Built; };
							build_count.set(build_count.get()+1);
							List.iter(function(tp) { core.Type.follow(tp.t); }, c.cl_params);
							return Built;
						}
						catch (err:Build_canceled) {
							var state = err.bs;
							c.cl_build = context.Typecore.make_pass(ctx, build);
							function rebuild () {
								context.Typecore.delay_late(ctx, PBuildClass, function () { c.cl_build(); });
							}
							return switch (state) {
								case Built: throw false;
								case Building(cl):
									if (build_count.get() == prev_build_count.get()) {
										var arr = List.map(function (c) { return core.Globals.s_type_path(c.cl_path); }, cl);
										core.Error.error("Loop in class building prevent compiler termination ("+ List.join(",", arr) +")", c.cl_pos);
									}
									prev_build_count.set(build_count.get());
									rebuild();
									Building(c::cl);
								case BuildMacro(f):
									f.set(rebuild::f.get());
									state;
							}

						}
						catch (_:Bool) { throw false; }
						catch (exn:Any) {
							c.cl_build = function () { return Built; };
							throw exn;
						}
					}
					return build();
				}
				ctx.pass = PBuildClass;
				ctx.curclass = c;
				c.cl_build = context.Typecore.make_pass(ctx, build);
				ctx.pass = PBuildModule;
				ctx.curclass = core.Type.null_class();
				context.Typecore.delay(ctx, PBuildClass, function() { c.cl_build(); });
				if ((ctx.com.platform.equals(Java) || ctx.com.platform.equals(Cs)) && !c.cl_extern) {
					context.Typecore.delay(ctx, PTypeField, function () {
						var metas = check_strict_meta(ctx, c.cl_meta);
						if (metas != Tl) { // metas != []
							c.cl_meta = List.concat(metas, c.cl_meta);
						}
						function run_field(cf:core.Type.TClassField) {
							var metas = check_strict_meta(ctx, cf.cf_meta);
							if (metas != Tl) { cf.cf_meta = List.concat(metas, cf.cf_meta); }
							List.iter(run_field, cf.cf_overloads);
						}
						List.iter(run_field, c.cl_ordered_statics);
						List.iter(run_field, c.cl_ordered_fields);
						switch (c.cl_constructor) {
							case Some(f): run_field(f);
							case _:
						}

					});
				}
			case EEnum(d):
				var e = switch (get_type(d.d_name.pack)) { case TEnumDecl(e): e; case _: throw false; }
				if (ctx.is_display_file && context.Display.is_display_position(d.d_name.pos)) {
					context.display.DisplayEmitter.display_module_type(ctx.com.display, TEnumDecl(e), d.d_name.pos);
				}
				var _ctx = ctx.clone(); _ctx.g = ctx.g;
				var ctx = _ctx;
				ctx.type_params = e.e_params;
				var h = try {
					Some(Hashtbl.find(ctx.g.type_patches, e.e_path));
				}
				catch (_:ocaml.Not_found) {
					None;
				}
				check_global_metadata(ctx, e.e_meta, function (m) { e.e_meta = m::e.e_meta; }, e.e_module.m_path, e.e_path, None);
				switch (h) {
					case None:
					case Some({map:h, tp:hcl}):
						Hashtbl.iter(function(_,_) { core.Error.error("Field type patch not supported for enums", e.e_pos);}, h);
						e.e_meta = List.concat(e.e_meta, hcl.tp_meta);
				}
				var constructs = new Ref(d.d_data);
				function get_constructs() {
					return List.map( function (c:core.Ast.EnumConstructor) : core.Ast.ClassField {
						var kind:core.Ast.ClassFieldKind = switch {f:c.ec_args, s:c.ec_params} {
							case {f:[], s:[]}: FVar(c.ec_type, None);
							case _:
								FFun({
									f_params:c.ec_params,
									f_type:c.ec_type,
									f_expr: None,
									f_args: List.map(function (a) : core.Ast.FunArg {
										return {
											name:{pack:a.name, pos:core.Globals.null_pos},
											opt: a.opt,
											meta: [],
											type: Some(a.type),
											value: None
										};
									}, c.ec_args)
								});
						}
						return {
							cff_name: c.ec_name,
							cff_doc: c.ec_doc,
							cff_meta: c.ec_meta,
							cff_pos: c.ec_pos,
							cff_access: [],
							cff_kind: kind
						};
					}, constructs.get());
				}
				function init() {
					List.iter(function (f) { f(); }, context_init.get());
				}
				build_module_def(ctx, TEnumDecl(e), e.e_meta, get_constructs, init, function (expr:core.Ast.Expr) : Void {
					var e = expr.expr; var p = expr.pos;
					switch (e) {
						case EVars([{type:Some({ct:CTAnonymous(fields), pos:p}), expr:None}]):
							constructs.set(List.map(function (f:core.Ast.ClassField): core.Ast.EnumConstructor {
								var args = []; var params = []; var t = null;
								switch (f.cff_kind) {
									case FVar(_t, None): t = _t;
									case FFun({f_params:pl, f_type:_t, f_expr:(None|Some({expr:EBlock([])})), f_args:al}):
										args = List.map(function (fa:core.Ast.FunArg) {
											return switch (fa.type) {
												case None: core.Error.error("Missing function parameter type", f.cff_pos);
												case Some(t): {name:fa.name.pack, opt:fa.opt, type:t};
											}
										}, al);
										params = pl;
										t = _t;
									case _:
										core.Error.error("Invalid enum constructor in @:build result", p);
								}
								return {
									ec_name: f.cff_name,
									ec_doc: f.cff_doc,
									ec_meta: f.cff_meta,
									ec_pos: f.cff_pos,
									ec_args: args,
									ec_params: params,
									ec_type: t
								}
							}, fields));
						case _:
							core.Error.error("Invalid enum constructor in @:build result", p);
					}
				});
				var et:core.Type.T = TEnum(e, List.map(function (p) { return p.t; }, e.e_params));
				var names = new Ref<ImmutableList<String>>([]);
				var index = new Ref(0);
				var is_flat = new Ref(false);
				var fields = new Ref(new Map<String, core.Type.TClassField>());
				List.iter(function (c:core.Ast.EnumConstructor) {
					var p = c.ec_pos;
					var params = new Ref<core.Type.TypeParams>([]);
					params.set(type_type_params(true, ctx, new core.Path([], c.ec_name.pack) ,function () { return params.get(); }, c.ec_pos, c.ec_params));
					var _ctx = ctx.clone(); _ctx.g = ctx.g;
					var ctx = _ctx;
					ctx.type_params = List.concat(params.get(), ctx.type_params);
					var rt = switch (c.ec_type) {
						case None: et;
						case Some(_t):
							var t = load_complex_type(ctx, true, p, _t);
							switch (core.Type.follow(t)) {
								case TEnum(te,_) if (te.equals(e)): // te == e
								case _: core.Error.error("Explicit enum type must be of the same enum type",p);
							}
							t;
					}
					var t:core.Type.T = switch (c.ec_args) {
						case []: rt;
						case l:
							is_flat.set(false);
							var pnames = new Ref(new Map<String, Bool>());
							TFun({args:List.map(function (f:{name:String, opt:Bool, type:core.Ast.TypeHint}) {
								var s = f.name; var opt = f.opt; var t = f.type.ct; var tp = f.type.pos;
								switch (t) {
									case CTPath({tpackage:[], tname:"Void"}):
										core.Error.error("Arguments of type Void are not allowed in enum constructors", c.ec_pos);
									case _:
								}
								if  (PMap.mem(s, pnames.get())) {
									core.Error.error("Duplicate parameter '"+ s + "' in enum constructor "+c.ec_name, p);
								}
								pnames.get().set(s, true);
								return {name:s, opt:opt, t:load_type_hint(opt, ctx, p, Some({ct:t, pos:tp}))};
							}, l), ret:rt});
					}
					if (PMap.mem(c.ec_name.pack, e.e_constrs)) {
						core.Error.error("Duplicate constructor " + c.ec_name.pack, p);
					}
					var f:core.Type.TEnumField = {
						ef_name: c.ec_name.pack,
						ef_type: t,
						ef_pos: p,
						ef_name_pos: c.ec_name.pos,
						ef_doc: c.ec_doc,
						ef_index: index.get(),
						ef_params: params.get(),
						ef_meta: c.ec_meta
					};
					var cf = core.Type.mk_field(f.ef_name, f.ef_type, p, f.ef_name_pos);
					cf.cf_kind = switch (core.Type.follow(f.ef_type)) {
						case TFun(_): Method(MethNormal);
						case _: Var({v_read:AccNormal, v_write:AccNo});
					}
					cf.cf_doc = f.ef_doc;
					cf.cf_params = f.ef_params;
					if (ctx.is_display_file && context.Display.is_display_position(p)) {
						context.display.DisplayEmitter.display_enum_field(ctx.com.display, f, p);
					}
					e.e_constrs = PMap.add(f.ef_name, f, e.e_constrs);
					fields.set(PMap.add(cf.cf_name, cf, fields.get()));
					index.set(index.get() + 1);
					names.set(c.ec_name.pack :: names.get());
				}, constructs.get());
				e.e_names = List.rev(names.get());
				// e.e_extern = e.e_extern; // in the compiler code
				e.e_type.t_params = e.e_params;
				e.e_type.t_type = TAnon({a_fields:fields.get(), a_status:new Ref<core.Type.AnonStatus>(EnumStatics(e))});
				if (is_flat.get()) {
					e.e_meta = ({name:FlatEnum, params:[], pos:core.Globals.null_pos} : core.Ast.MetadataEntry) ::e.e_meta;
				}
				if ((ctx.com.platform == Java || ctx.com.platform == Cs) && !e.e_extern) {
					context.Typecore.delay(ctx, PTypeField, function () {
						var metas = check_strict_meta(ctx, e.e_meta);
						e.e_meta = List.concat(metas, e.e_meta);
						PMap.iter(function (_, ef) {
							var metas = check_strict_meta(ctx, ef.ef_meta);
							if (metas != Tl) {
								ef.ef_meta = List.concat(metas, ef.ef_meta);
							}
						}, e.e_constrs);
					});
				}
			case ETypedef(d):
				var t = switch (get_type(d.d_name.pack)) { 
					case TTypeDecl(t): t;
					case _: throw false;
				}
				if (ctx.is_display_file && context.Display.is_display_position(d.d_name.pos)) {
					context.display.DisplayEmitter.display_module_type(ctx.com.display, TTypeDecl(t), d.d_name.pos);
				}
				check_global_metadata(ctx, t.t_meta, function (m) {
					t.t_meta = m :: t.t_meta;
				}, t.t_module.m_path, t.t_path, None);
				var _ctx = ctx.clone(); _ctx.g = ctx.g;
				var ctx = _ctx;
				ctx.type_params = t.t_params;
				var tt = load_complex_type(ctx, true, p, d.d_data);
				tt = switch (d.d_data.ct) {
					case CTExtend(_): tt;
					case CTPath({tpackage:["haxe", "macro"], tname:"MacroType"}):
						// we need to follow MacrotYpe immediately since it might define other module types that we will load afterwards
						if (t.t_type.equals(core.Type.follow(tt))) {
							core.Error.error("Recursive typedef is not allowed", p);
						}
						tt;
					case _:
						if (core.Meta.has(Eager, d.d_meta)) {
							core.Type.follow(tt);
						}
						else {
							function check_rec(tt:core.Type.T) {
								if (tt.equals(t.t_type)) {
									core.Error.error("Recursive typedef is not allowed", p);
								}
								switch (tt) {
									case TMono(r):
										switch (r.get()) {
											case None:
											case Some(t): check_rec(t);
										}
									case TLazy(f):
										check_rec(core.Type.lazy_type(f));
									case TType(td, tl):
										if (td.equals(t)) {
											core.Error.error("Recursive typedef is not allowed", p);
											check_rec(core.Type.apply_params(td.t_params, tl, td.t_type));
										}
									case _:
								}
							}
							var r = context.Typecore.exc_protect(ctx, function (r) {
								r.set(core.Type.lazy_processing(function () { return tt; }));
								check_rec(tt);
								return tt;
							}, "typedef_rec_check");
							TLazy(r);
						}

				}
				switch (t.t_type) {
					case TMono(r):
						switch (r.get()) {
							case None: r.set(Some(tt));
							case Some(_): throw false;
						}
					case _: throw false;
				}
				if (ctx.com.platform.equals(Cs) && t.t_meta != Tl) {
					context.Typecore.delay(ctx, PTypeField, function () {
						var metas = check_strict_meta(ctx, t.t_meta);
						if (metas != Tl) {
							t.t_meta = List.concat(metas, t.t_meta);
						}
					});
				}
			case EAbstract(d):
				var a = switch (get_type(d.d_name.pack)) {
					case TAbstractDecl(a): a;
					case _: throw false;
				}
				if (ctx.is_display_file && context.Display.is_display_position(d.d_name.pos)) {
					context.display.DisplayEmitter.display_module_type(ctx.com.display, TAbstractDecl(a), d.d_name.pos);
				}
				check_global_metadata(ctx, a.a_meta, function (m) { a.a_meta = m :: a.a_meta; }, a.a_module.m_path, a.a_path, None);
				var _ctx = ctx.clone(); _ctx.g = ctx.g;
				var ctx = _ctx;
				ctx.type_params = a.a_params;
				var is_type = new Ref(false);
				function load_type(t:core.Ast.TypeHint, from:Bool) {
					var _t = load_complex_type(ctx, true, p, t);
					_t = if (!core.Meta.has(CoreType, a.a_meta)) {
						if (is_type.get()) {
							var r = context.Typecore.exc_protect(ctx, function (r) {
								r.set(core.Type.lazy_processing( function() { return _t; }));
								var at = core.Type.monomorphs(a.a_params, a.a_this);
								try {
									if (from){
										core.Type.unify(_t, at);
									}
									else {
										core.Type.unify(at, _t);
									}
								}
								catch (_:core.Type.Unify_error) {
									core.Error.error("You can only declare from/to with compatible types", p);
								}
								return _t;
							}, "constraint");
							TLazy(r);
						}
						else {
							core.Error.error("Missing underlying type declaration or @:coreType declaration", p);
						}
					}
					else {
						if (core.Meta.has(Callable, a.a_meta)) {
							core.Error.error("@:coreType abstracts cannot be @:callable", p);
						}
						_t;
					}
					return _t;
				}
				List.iter(function(flag:core.Ast.AbstractFlag) {
					switch (flag) {
						case AFromType(t): a.a_from = load_type(t, true) :: a.a_from;
						case AToType(t): a.a_to = load_type(t, true) :: a.a_to;
						case AIsType(t): 
							if (a.a_impl == None) {
								core.Error.error("Abstracts with underlying type must have an implementation",a.a_pos);
							}
							if (core.Meta.has(CoreType, a.a_meta)) {
								core.Error.error("@:coreType abstracts cannot have an underlying type", p);
							}
							var at = load_complex_type(ctx, true, p, t);
							context.Typecore.delay(ctx, PForce, function() {
								switch (core.Type.follow(at)) {
									case TAbstract(a2, _) if ( a.equals(a2) ):
										core.Error.error("Abstract underlying type cannot be recursive", a.a_pos);
									case _:
								}
							});
							a.a_this = at;
							is_type.set(true);
						case AExtern:
							switch (a.a_impl) {
								case Some(c):
									c.cl_extern = true;
								case None: // Hmmmm....
							}
						case APrivAbstract:
					}
				}, d.d_flags);
				if (!is_type.get()) {
					if (core.Meta.has(CoreType, a.a_meta)) {
						a.a_this = TAbstract(a, List.map(function (p) { return p.t; }, a.a_params));
					}
					else {
						core.Error.error("Abstract is missing underlying type declaration", a.a_pos);
					}
				}
		}
	}

	public static function module_pass_2 (ctx:context.Typecore.Typer, m:core.Type.ModuleDef, decls:ImmutableList<{fst:core.Type.ModuleType, snd:core.Ast.TypeDecl}>, tdecls:ImmutableList<core.Ast.TypeDecl>, p:core.Globals.Pos) : Void {
		/*
		 * here is an additional PASS 1 phase, which define the type parameters for all module types.
		 * Constraints are handled lazily (no other type is loaded) because they might be recursive anyway
		 */
		List.iter(function (d:{fst:core.Type.ModuleType, snd:core.Ast.TypeDecl}) {
			switch (d) {
				case {fst:TClassDecl(c), snd:{decl:EClass(d), pos:p}}:
						c.cl_params = type_type_params(ctx, c.cl_path, function () { return c.cl_params.clone();}, p, d.d_params);
						if (core.Meta.has(Generic, c.cl_meta) && c.cl_params != Tl) {
							c.cl_kind = KGeneric;
						}
						if (core.Meta.has(GenericBuild, c.cl_meta)) {
							if (ctx.in_macro) {
								core.Error.error("@:genericBuild cannot be used in macros", c.cl_pos);
							}
							c.cl_kind = KGenericBuild(d.d_data);
						}
						if (c.cl_path == new core.Path(["haxe", "macro"], "MacroType")) {
							c.cl_kind = KMacroType;
						}
				case {fst:TEnumDecl(e), snd:{decl:EEnum(d), pos:p}} :
					e.e_params = type_type_params(ctx, e.e_path, function () { return e.e_params.clone();}, p, d.d_params);
				case {fst:TTypeDecl(t), snd:{decl:ETypedef(d), pos:p}}:
					t.t_params = type_type_params(ctx, t.t_path, function () { return t.t_params.clone();}, p, d.d_params);
				case {fst:TAbstractDecl(a), snd:{decl:EAbstract(d),pos:p}}:
					a.a_params = type_type_params(ctx, a.a_path, function () { return a.a_params.clone();}, p, d.d_params);
				default: throw false;
			}
		}, decls);
		// setup module types
		var context_init = new Ref<ImmutableList<Void->Void>>([]);
		function do_init() {
			switch (context_init.get()) {
				case []:
				case l:
					context_init.set([]);
					List.iter(function (f) { f(); }, List.rev(l));
			}
		}
		List.iter(init_module_type.bind(ctx, context_init, do_init), tdecls);
	}

	public static function type_types_into_module (ctx:context.Typecore.Typer, m:core.Type.ModuleDef, tdecls:ImmutableList<core.Ast.TypeDecl>, p:core.Globals.Pos) {
		var tmp = module_pass_1(ctx, m, tdecls, p);
		var decls = tmp.fst;
		var tdecls = tmp.snd;
		var types = List.map(function (e) { return e.fst; }, decls);
		List.iter(function (t) {
			check_module_types(ctx, m, p, t);
		}, types);
		m.m_types = List.concat(m.m_types, types);
		// define the per-module context for the next pass
		var ctx:context.Typecore.Typer = {
			com: ctx.com,
			g: ctx.g,
			t: ctx.t,
			// t: ctx.t.clone(),
			m: {
				curmod:m,
				module_types: List.map(function (t) { return {mt:t, pos:core.Globals.null_pos}; }, ctx.g.std.m_types),
				module_using: [],
				module_globals: new Map<String, {a:core.Type.ModuleType, b:String, pos:core.Globals.Pos}>(),
				wildcard_packages: [],
				module_imports: []
			},
			is_display_file: ctx.com.display.dms_display && context.Display.is_display_file(m.m_extra.m_file),
			meta:[],
			this_stack: [],
			with_type_stack: [],
			call_argument_stack: [],
			pass: PBuildModule,
			on_error: function (ctx, msg, p) { ctx.com.error(msg, p); },
			macro_depth: ctx.macro_depth,
			curclass: core.Type.null_class(),
			curfield: core.Type.null_field(),
			tthis: ctx.tthis,
			ret: ctx.ret,
			locals: new Map<String, core.Type.TVar>(),
			type_params: [],
			curfun: FunStatic,
			untyped_: false,
			in_macro: ctx.in_macro,
			in_display: false,
			in_loop: false,
			opened: [],
			in_call_args: false,
			vthis: None
		};
		if (!ctx.g.std.equals(core.Type.null_module)) {
			core.Type.add_dependency(m, ctx.g.std);
			load_core_type(ctx, "String");
		}
		module_pass_2(ctx, m, decls, tdecls, p);
		return ctx;
	}

	/*
	 * Creates a module context for [m] and types [tdecls] using it.
	 */
	public static function handle_import_hx (ctx:context.Typecore.Typer, m:core.Type.ModuleDef, decls:ImmutableList<core.Ast.TypeDecl>, p:core.Globals.Pos) {
		var path_split:ImmutableList<String> = switch (List.rev(core.Path.get_path_parts(m.m_extra.m_file))) {
			case []: [];
			case _::l: l;
		}

		function join (l:ImmutableList<String>) : String {
			return List.join(core.Path.path_sep,List.rev("import.hx"::l));
		}
		function loop (path:ImmutableList<String>, pack:ImmutableList<String>) : ImmutableList<String> {
			return switch {f:path, s:pack} {
				case {f:_, s:[]}: [join(path)];
				case {f:p::_path, s:_::_pack}:
					join(path) :: loop(_path, _pack);
				case _: [];
			}
		}
		var candidates = loop(path_split, m.m_path.a);
		function make_import_module(path:String, r:ImmutableList<core.Ast.TypeDecl>) {
			ctx.com.parser_cache.set(path, r);
			// We use the file path as module name to make it unique. This may or may not be a good idea...
			var m_import = make_module(ctx, new core.Path([], path), path, p);
			m_import.m_extra.m_kind = MImport;
			add_module(ctx, m_import, p);
			return m_import;
		}

		function f(acc:ImmutableList<core.Ast.TypeDecl>,path:String) : ImmutableList<core.Ast.TypeDecl> {
			var decls = try {
				var r = Hashtbl.find(ctx.com.parser_cache, path);
				var mimport = Hashtbl.find(ctx.g.modules, new core.Path([], path));
				if (!mimport.m_extra.m_kind.equals(MFake)) {
					core.Type.add_dependency(m, mimport);
				}
				r;
			}
			catch (_:ocaml.Not_found) {
				if (sys.FileSystem.exists(path)) {
					var tmp = parse_file(ctx.com, path, p);
					var r = tmp.decls;
					List.iter(function (p:core.Ast.TypeDecl) {
						switch (p.decl) {
							case EImport(_), EUsing(_):
							default:
								core.Error.error("Only import and using is allowed in import.hx files", p.pos);
						}
					}, r);
					core.Type.add_dependency(m, make_import_module(path, r));
					r;
				}
				else {
					var r:ImmutableList<core.Ast.TypeDecl> = [];
					// Add empty decls so we don't check the file system all the time.
					make_import_module(path, r).m_extra.m_kind = MFake;
					r;
				}
			}
			return List.concat(decls, acc);
		}
		return List.fold_left(f, decls, candidates);
	}

	/*
	 * Creates a new module and types [tdecls] into it.
	 */
	public static function type_module (ctx:context.Typecore.Typer, mpath:core.Path, file:String, ?is_extern:Bool=false, tdecls:ImmutableList<core.Ast.TypeDecl>, p:core.Globals.Pos) : core.Type.ModuleDef{
		var m = make_module(ctx, mpath, file, p);
		ctx.g.modules.set(m.m_path, m);
		var tdecls = handle_import_hx(ctx, m, tdecls, p);
		var ctx = type_types_into_module(ctx, m, tdecls, p);
		if (is_extern) {
			m.m_extra.m_kind = MExtern;
		}
		if (ctx.is_display_file) {
			switch (ctx.com.display.dms_kind) {
				case DMResolve(s):
					resolve_position_by_path(ctx, {tname:s, tpackage:[], tsub:None, tparams:[]}, p);
				case _:
			}
		}
		return m;
	}

	public static function resolve_module_file (com:context.Common.Context, m:core.Path, remap:Ref<ImmutableList<String>>, p:core.Globals.Pos) : String {
		var forbid = new Ref(false);
		function compose_path (no_rename:Bool) : String {
			var _tmp = switch (m) {
				case {a:[], b:name}: name;
				case {a:x::l, b:name}:
					var l:ImmutableList<String> = l; // Until bug fixed
					var x:String = try {
						switch (PMap.find(x, com.package_rules)) {
							case Forbidden: forbid.set(true); x;
							case Directory(d): (no_rename) ? x : d;
							case Remap(d):
								remap.set(d::l); d;
						}
					}
					catch (_:ocaml.Not_found) {
						x;
					}
					List.join("/", (x::l)) + "/" + name;
			};
			return _tmp + ".hx";
		}
		
		var file = try {
			context.Common.find_file(com, compose_path(false));
		}
		catch (_:ocaml.Not_found) {
			context.Common.find_file(com, compose_path(true));
		}

		file = switch (m.b.toLowerCase()) {
			case "con", "aux", "prn", "nul", "com1", "com2", "com3", "lpt1", "lpt2", "lpt3":
				// these names are reserved by the OS - old DOS legacy, such files cannot be easily created but are reported as visible
				if (Sys.systemName() == "Windows") {
					if (sys.FileSystem.exists(file)) {
						(sys.FileSystem.stat(file).size > 0) ? file : throw ocaml.Not_found.instance;
					}
					else {
						throw ocaml.Not_found;
					}
				}
				else {
					file;
				}
			default: file;
		};
		// if we try to load a std.xxxx class and resolve a real std file, the package name is not valid, ignore
		switch (m.a) {
			case "std" :: _:
				var file = core.Path.unique_full_path(file);
				if (List.exists(function (path) { return StringTools.startsWith(file, try {core.Path.unique_full_path(path);} catch (_:Any) { path; }); }, com.std_path)) {
					throw ocaml.Not_found.instance;
				}
			case _:
		}
		if (forbid.get()) {
			var decls = parse_hook.get()(com, file, p).decls;
			function loop(decls:ImmutableList<core.Ast.TypeDecl>) : core.Ast.Metadata {
				return switch (decls) {
					case ({decl:EImport(_)}|{decl:EUsing(_)}) :: decls: loop(decls);
					case {decl:EClass(d)} :: _: d.d_meta;
					case {decl:EEnum(d)} :: _: d.d_meta;
					case {decl:EAbstract(d)} :: _: d.d_meta;
					case {decl:ETypedef(d)} :: _: d.d_meta;
					case [] : [];
				}
			}
			var meta = loop(decls);
			if (!core.Meta.has(NoPackageRestrict, meta)) {
				var x = switch (m.a) {
					case []: throw false;
					case x::_: x;
				}
				throw new context.Typecore.Forbid_package(
					{pack:x, m:m, p:p},
					[],
					(context.Common.defined(com, Macro)) ? "macro" : core.Globals.platform_name(com.platform)
				);
			}
		}
		return file;
	}

	public static function parse_module (ctx:context.Typecore.Typer, m:core.Path, p:core.Globals.Pos) : {file:String, decls:ImmutableList<core.Ast.TypeDecl>} {
		var remap = new Ref(m.a);
		var file = resolve_module_file(ctx.com, m, remap, p);
		var ph = parse_hook.get()(ctx.com, file, p);
		var pack = ph.pack;
		var decls = ph.decls;
		
		if (!pack.equals(remap.get())) {
			function spack(m:ImmutableList<String>) : String {
				return (m == Tl) ? "<empty>" : List.join(".", m);
			}
			if (p == core.Globals.null_pos) {
				context.Typecore.display_error(ctx, "Invalid commandline class : "+core.Globals.s_type_path(m) + " should be " + core.Globals.s_type_path(new core.Path(pack, m.b)), p);
			}
			else {
				context.Typecore.display_error(ctx, "Invalid package : "+spack(m.a) + " should be " + spack(pack), p);
			}
		}

		if (!m.a.equals(remap.get())) {
			// build typedefs to redirect to real package
			var _tmp : ImmutableList<core.Ast.TypeDecl> = [
				{decl:core.Ast.TypeDef.EImport(
					{pns:List.map(function (s:String) {return {pack:s, pos:core.Globals.null_pos}; }, List.concat(remap.get(),[m.b])) ,
				 	mode:INormal}
				), pos:core.Globals.null_pos}
			];
			var d = List.rev(List.fold_left( function (acc:ImmutableList<core.Ast.TypeDecl>, td:core.Ast.TypeDecl) : ImmutableList<core.Ast.TypeDecl> {
				var t = td.decl; var p = td.pos;
				function build (f:Dynamic, d:core.Ast.Definition<Dynamic, Dynamic>): ImmutableList<core.Ast.TypeDecl>{
					var priv = List.mem(f, d.d_flags);
					var data:core.Ast.TypePath = if (priv) {
						{tpackage:[], tname:"Dynamic", tparams:[], tsub:None};
					}
					else {
						{
						tpackage:remap.get(),
						tname:d.d_name.pack,
						tparams:List.map(
							function (tp) : core.Ast.TypeParamOrConst {
								return TPType({
									ct:CTPath({
										tpackage: [],
										tname: tp.tp_name.pack,
										tparams: [],
										tsub: None
									}),
									pos:core.Globals.null_pos
								});
							}, d.d_params),
						tsub:None
						}
					}
					return {
						decl:core.Ast.TypeDef.ETypedef({
							d_name: d.d_name,
							d_doc: None,
							d_meta: [],
							d_params: d.d_params,
							d_flags: (priv) ? [EPrivate] : [],
							d_data: {ct:CTPath(data), pos:core.Globals.null_pos}
						}),
						pos:p
					}::acc;
				}
				return switch (t) {
					case EClass(d) : build(core.Ast.ClassFlag.HPrivate,d);
					case EEnum(d) : build(core.Ast.EnumFlag.EPrivate,d);
					case ETypedef(d) : build(core.Ast.EnumFlag.EPrivate,d);
					case EAbstract(d) : build(core.Ast.AbstractFlag.APrivAbstract,d);
					case EImport(_), EUsing(_): acc;
				}
			}, _tmp, decls));
			return {file:file, decls:d};
		}
		else {
			return {file:file, decls:decls};
		}
	}

	public static function load_module (ctx:context.Typecore.Typer, m:core.Path, p:core.Globals.Pos) : core.Type.ModuleDef {
		var m2 = try {
			Hashtbl.find(ctx.g.modules,m);
		}
		catch (_:ocaml.Not_found) {
			switch (type_module_hook.get()(ctx, m, p)) {
				case Some(m): m;
				case None:
					var is_extern = new Ref(false);
					var _tmp = try {
						parse_module(ctx, m, p);
					}
					catch (_:ocaml.Not_found) {
						function loop(l:ImmutableList<core.Path->core.Globals.Pos->Option<{s:String, pack:core.Ast.Package}>>) : {file:String,  decls:ImmutableList<core.Ast.TypeDecl>} {
							return switch (l) {
								case []: throw new core.Error(Module_not_found(m), p);
								case load::l:
									switch (load(m, p)) {
										case None: loop(l);
										case Some(s):
											{file:s.s, decls:s.pack.decls};
									}
							}
						}
						is_extern.set(true);
						loop(ctx.com.load_extern_type);
					}
					var file =_tmp.file; var decls = _tmp.decls;
					try {
						type_module(ctx, m, file, decls, p);
					}
					catch (err:context.Typecore.Forbid_package) {
						if (p == core.Globals.null_pos) { throw err; }
						else {
							var inf = err.a; var pl = err.pl; var pf = err.pf;
							throw new context.Typecore.Forbid_package(inf, p::pl, pf);
						}
					}
			};
		}
		core.Type.add_dependency(ctx.m.curmod, m2);
		if (ctx.pass == PTypeField) {
			context.Typecore.flush_pass(ctx, PBuildClass, "load_module");
		}
		return m2;
	}

	// type generic_context = { ...

	public static function make_generic (ctx:context.Typecore.Typer, ps:core.Type.TypeParams, pt:ImmutableList<core.Type.T>, p:core.Globals.Pos) : Generic_context {
		function loop (l1:core.Type.TypeParams, l2:ImmutableList<core.Type.T>) : ImmutableList<{fst:core.Type.T, snd:core.Type.T}>{
			return switch ({f:l1, s:l2}) {
				case {f:[], s:[]}: [];
				case {f:{name:x, t:TLazy(f)}::l1}: loop({name:x, t:core.Type.lazy_type(f)}::l1, l2);
				case {f:{t:t1}::l1, s:t2::l2}: {fst:t1, snd:t2}::loop(l1, l2);
				case _: throw false;
			}
		}
		var name = List.join("_", List.map2(function (tp, t) {
			function s_type_path_underscore(path:core.Path) : String {
				var p = path.a; var s = path.b;
				return switch (p) {
					case []: s;
					case _: List.join("_", p) + "_" + s;
				}
			}
			var loop_tl : ImmutableList<core.Type.T>->String = null;
			function loop (top, t) {
				return switch (core.Type.follow(t)) {
					case TInst(c, tl): s_type_path_underscore(c.cl_path) + loop_tl(tl);
					case TEnum(en, tl): s_type_path_underscore(en.e_path) + loop_tl(tl);
					case TAbstract(a, tl): s_type_path_underscore(a.a_path) + loop_tl(tl);
					case _ if (!top): "_"; // allow unknown/incompatible types as type parameters to retain old behavior
					case TMono(_): throw new GenericException("Could not determine type for parameter "+tp.name, p);
					case TDynamic(_): "Dynamic";
					case t: throw new GenericException("Type parameter must be a class or enum instance (found "+core.Type.s_type(core.Type.print_context(), t) + ")", p);
				}
			}
			loop_tl = function (tl) {
				return switch (tl) {
					case []: "";
					case tl: "_" + List.join("_", List.map(loop.bind(false), tl));
				}
			}
			loop(true, t);
		}, ps, pt));
		var _ctx = ctx.clone(); _ctx.g = ctx.g;
		return {
			// ctx:ctx, 
			ctx:_ctx, 
			subst: loop(ps, pt),
			name: name,
			p: p.clone(),
			mg: None
		};
	}

	public static function generic_substitute_type (gctx:Generic_context, t:core.Type.T) : core.Type.T {
		return switch (t) {
			case TInst(c2={cl_kind:KGeneric}, tl2):
				// maybe loop, or generate cascading generics
				var f = gctx.ctx.g.do_build_instance(gctx.ctx, TClassDecl(c2), gctx.p).f;
				var t = f(List.map(generic_substitute_type.bind(gctx), tl2));
				switch ({fst:core.Type.follow(t), snd:gctx.mg}) {
					case {fst:TInst(c, _), snd:Some(m)}:
						core.Type.add_dependency(m, c.cl_module);
					case _:
				}
				return t;
			case _:
				try {
					generic_substitute_type(gctx, List.assq(t, gctx.subst));
				}
				catch (_:ocaml.Not_found) {
					core.Type.map(generic_substitute_type.bind(gctx), t);
				}
		}
	}

	public static function generic_substitute_expr (gctx:Generic_context, e:core.Type.TExpr) : core.Type.TExpr {
		var vars = new Map<Int, core.Type.TVar>();
		function build_var (v:core.Type.TVar) : core.Type.TVar {
			return try {
				Hashtbl.find(vars, v.v_id);
			}
			catch (_:ocaml.Not_found) {
				var v2 = core.Type.alloc_var(v.v_name, generic_substitute_type(gctx, v.v_type), v.v_pos);
				v2.v_meta = v.v_meta;
				Hashtbl.add(vars, v.v_id, v2);
				v2;
			}
		}
		function build_expr(e:core.Type.TExpr) : core.Type.TExpr {
			return switch (e.eexpr) {
				case TField(e1, FInstance(c={cl_kind:KGeneric}, tl, cf)):
					var f = gctx.ctx.g.do_build_instance(gctx.ctx, TClassDecl(c), gctx.p).f;
					var t = f(List.map(generic_substitute_type.bind(gctx), tl));
					var fa = try {
						core.Type.quick_field(t, cf.cf_name);
					}
					catch (_:ocaml.Not_found) {
						core.Error.error("Type "+core.Type.s_type(core.Type.print_context(), t)+" has no field "+cf.cf_name+" (possible typing order issue)", e.epos);
					}
					var _e = e.clone();
					_e.eexpr = TField(e1, fa);
					build_expr(_e);
				case TTypeExpr(TClassDecl(c={cl_kind:KTypeParameter(_)})):
					function loop (subst:ImmutableList<{fst:core.Type.T, snd:core.Type.T}>) : core.Type.T {
						return switch (subst) {
							case {fst:t1, snd:t2}::subst:
								switch (core.Type.follow(t1)) {
									case TInst(c2,_) if (c.equals(c2)): t2;
									case _: loop(subst);
								}
							case []: throw ocaml.Not_found.instance;
						}
					}
					return try {
						var t = loop(gctx.subst);
						switch (core.Type.follow(t)) {
							case TInst({cl_kind:KExpr(e)}, _): context.Typecore.type_expr(gctx.ctx, e, Value);
							case _: core.Error.error("Only Const type parameters can be used as value", e.epos);
						}
					}
					catch (_:ocaml.Not_found) {
						e;
					}
				case _:
					core.Type.map_expr_type(build_expr, generic_substitute_type.bind(gctx), build_var, e);
			};
		}
		return build_expr(e);
	}

	public static function get_short_name () : Void->String {
		var i = new Ref(-1);
		return function () {
			i.set(i.get()+1);
			return "Hx___short___hx_type_"+i.get();
		}
	}

	public static function build_generic (ctx:context.Typecore.Typer, c:core.Type.TClass, p:core.Globals.Pos, tl:ImmutableList<core.Type.T>) : core.Type.T {
		var pack = c.cl_path.a;
		var recurse = new Ref(false);
		function check_recursive(t) {
			switch (core.Type.follow(t)) {
				case TInst(c2, tl):
					switch (c2.cl_kind) {
						case KTypeParameter(tl):
							if (!is_generic_parameter(ctx, c2) && core.Type.has_ctor_constraint(c2)) {
								core.Error.error("Type parameters with a constructor cannot be used non-genercially",p);
							}
							recurse.set(true);
						case _:
					}
					List.iter(check_recursive, tl);
				case _:
			}
		}
		List.iter(check_recursive, tl);
		return if (recurse.get() || !(ctx.com.display.dms_full_typing)) {
			TInst(c, tl); // build a normal instance
		}
		else {
			var gctx = make_generic(ctx, c.cl_params, tl, p);
			var name = c.cl_path.b + "_" + gctx.name;
			return try {
				load_instance(ctx, {tp:{tpackage:pack, tname:name, tparams:[], tsub:None}, pos:p}, false, p);
			}
			catch (err:core.Error) {
				switch (err.msg) {
					case Module_not_found(path) if (path.equals(new core.Path(pack, name))):
						var m = try {
							Hashtbl.find(ctx.g.modules, Hashtbl.find(ctx.g.types_module, c.cl_path));
						}
						catch (_:ocaml.Not_found) {
							throw false;
						}
						c.cl_build(); // make sure the super class is alread setup
						var mg:core.Type.ModuleDef = {
							m_id: core.Type.alloc_mid(),
							m_path: path, //new core.Path(pack, name),
							m_types: [],
							m_extra: core.Type.module_extra(core.Globals.s_type_path(path), m.m_extra.m_sign, 0.0, MFake, m.m_extra.m_check_policy)
						}
						gctx.mg = Some(mg);
						var cg = core.Type.mk_class(mg, path, c.cl_pos, core.Globals.null_pos);
						mg.m_types = [TClassDecl(cg)];
						ctx.g.modules.set(mg.m_path, mg);
						core.Type.add_dependency(mg, m);
						core.Type.add_dependency(ctx.m.curmod, mg);
						// ensure that type parameters are set in dependencies
						var dep_stack = new Ref<ImmutableList<core.Type.T>>([]);
						var add_dep : core.Type.ModuleDef->ImmutableList<core.Type.T>->Void = null;
						function loop (t:core.Type.T) {
							if (!List.memq(t, dep_stack.get())) {
								dep_stack.set(t::dep_stack.get());
								switch (t) {
									case TInst(c, tl): add_dep(c.cl_module, tl);
									case TEnum(e, tl): add_dep(e.e_module, tl);
									case TType(t, tl): add_dep(t.t_module, tl);
									case TAbstract(a, tl): add_dep(a.a_module, tl);
									case TMono(r):
										switch (r.get()) {
											case None:
											case Some(t): loop(t);
										}
									case TLazy(f):
										loop(core.Type.lazy_type(f));
									case TDynamic(t2):
										if (t == t2.get()) {}
										else {
											loop(t2.get());
										}
									case TAnon(a):
										PMap.iter(function (_, f) {return loop(f.cf_type);}, a.a_fields);
									case TFun({args:args, ret:ret}):
										List.iter(function (arg) { return loop(arg.t); }, args);
										loop(ret);
								}
							}
						}
						add_dep = function (m, tl) {
							core.Type.add_dependency(mg, m);
							List.iter(loop, tl);
						};
						List.iter(loop, tl);
						function build_field(cf_old:core.Type.TClassField) {
							// We have to clone the type parameters (issue #4672). We cannot substitute the constraints immediately because
			   				// we need the full substitution list first.
							var _tmp = List.fold_left(function(a, b) {
								var subst = a.fst; var params = a.snd; var s = b.name; var t = b.t;
								return switch (core.Type.follow(t)) {
									case t=TInst(c, tl):
										var _c = c.clone(); 
										_c.cl_module = mg;
										var t2:core.Type.T = TInst(_c, tl);
										return {fst:{fst:t, snd:t2} :: subst, snd:{name:s, t:t2} ::params};
									case _: throw false;
								};
							}, {fst:[], snd:[]}, cf_old.cf_params);
							var param_subst = _tmp.fst; var params = _tmp.snd;
							var gctx = gctx.clone();
							gctx.subst = List.concat(param_subst, gctx.subst);
							var cf_new = cf_old.clone();
							cf_new.cf_pos = cf_old.cf_pos.clone(); // copy; // wtf ?
							// Type parameter constraints are substituted here.
							cf_new.cf_params = List.rev_map(function(tp) {
								var s = tp.name; var t = tp.t;
								return switch (core.Type.follow(t)) {
									case TInst(c={cl_kind:KTypeParameter(tl1)}, _):
										var tl1 = List.map(generic_substitute_type.bind(gctx), tl1);
										c.cl_kind = KTypeParameter(tl1);
										{name:s, t:t};
									case _: throw false;
								}
							}, params);
							function f() {
								var t = generic_substitute_type(gctx, cf_old.cf_type);
								core.Type.follow(t);
								try {
									switch (cf_old.cf_expr) {
										case None:
											switch (cf_old.cf_kind) {
												case Method(_) if (!c.cl_interface && !c.cl_extern):
													context.Typecore.display_error(ctx, "Field "+cf_new.cf_name+" has no expression (possible typing order issue", cf_new.cf_pos);
													context.Typecore.display_error(ctx, "While building "+core.Globals.s_type_path(cg.cl_path), p);
												case _:
											}
										case Some(e):
											cf_new.cf_expr = Some(generic_substitute_expr(gctx, e));
									}
								}
								catch(err:core.Type.Unify_error) {
									core.Error.error(core.Error.error_msg(Unify(err.l)), cf_new.cf_pos);
								}
								return t;
							}
							var r = context.Typecore.exc_protect(ctx, function (r) {
								var t = core.Type.mk_mono();
								r.set(core.Type.lazy_processing(function() { return t;}));
								return context.Typecore.unify_raise(ctx, f(), t, p);
							}, "build_generic");
							cf_new.cf_type = TLazy(r);
							return cf_new;
						}
						if (c.cl_init != None || c.cl_dynamic != None) {
							core.Error.error("This class can't be generic", p);
						}
						List.iter(function (cf:core.Type.TClassField) {
							switch (cf.cf_kind) {
								case Method(MethMacro) if (!ctx.in_macro):
								case _: core.Error.error("A generic class can't have static fields", cf.cf_pos);
							}
						}, c.cl_ordered_statics);
						cg.cl_super = switch (c.cl_super) {
							case None: None;
							case Some({c:cs, params:pl}):
								var ts = core.Type.follow(core.Type.apply_params(c.cl_params, tl, TInst(cs, pl)));
								var _tmp = typing.typeload.Inheritance.check_extends(ctx, c, ts, p);
								var cs = _tmp.c; var pl = _tmp.params;
								switch (cs.cl_kind) {
									case KGeneric:
										switch (build_generic(ctx, cs, p, pl)) {
											case TInst(_cs, _pl): Some({c:_cs, params:_pl});
											case _: throw false;
										}
									case _: Some({c:cs, params:pl});
								}
						};
						add_constructor(ctx, cg, false, p);
						cg.cl_kind = KGenericInstance(c, tl);
						cg.cl_meta = ({name:NoDoc, params:[], pos:core.Globals.null_pos} : core.Ast.MetadataEntry) :: cg.cl_meta;
						if (core.Type.has_meta(Keep, c.cl_meta)) {
							cg.cl_meta = ({name:Keep, params:[], pos:core.Globals.null_pos} : core.Ast.MetadataEntry):: cg.cl_meta;
						}
						cg.cl_interface = c.cl_interface;
						cg.cl_constructor = switch ({fst:cg.cl_constructor, snd:c.cl_constructor, trd:c.cl_super}) {
							case {snd:Some(cf)}: Some(build_field(cf));
							case {fst:Some(ctor)}: Some(ctor);
							case {fst:None, snd:None, trd:None}: None;
							case _: core.Error.error("Please define a constructor for this class in order to use it as generic", c.cl_pos);
						}
						cg.cl_implements = List.map(function (imp:{c:core.Type.TClass, params:core.Type.TParams} ) {
							return switch core.Type.follow(generic_substitute_type(gctx, TInst(imp.c, List.map(generic_substitute_type.bind(gctx), imp.params)))) {
								case TInst(i, tl): {c:i, params:tl};
								case _: throw false;
							}
						}, c.cl_implements);
						cg.cl_ordered_fields = List.map(function (f) {
							var _f = build_field(f);
							cg.cl_fields.set(_f.cf_name, _f);
							return _f;
						}, c.cl_ordered_fields);
						cg.cl_overrides = List.map(function (f) {
							return try {
								PMap.find(f.cf_name, cg.cl_fields);
							}
							catch (_:ocaml.Not_found) {
								throw false;
							}
						}, c.cl_overrides);
						// In rare cases the class name can become too long, so let's shorten it (issue #3090).
						if (cg.cl_path.b.length >  254) {
							var n = get_short_name()();
							cg.cl_meta = ({name:Native, params:[{expr:EConst(CString(n)), pos:p}], pos:core.Globals.null_pos} : core.Ast.MetadataEntry) :: cg.cl_meta;
						}
						TInst(cg, []);
					case _:
						throw err;
				}
			}
		}
	}

	// --------------------------------------------------------------------------
	// MACRO TYPE
	public static function get_macro_path (ctx:context.Typecore.Typer, e:core.Ast.Expr, args:ImmutableList<core.Ast.Expr>, p:core.Globals.Pos) {
		function loop (e:core.Ast.Expr) : ImmutableList<String> {
			return switch (e.expr) {
				case EField(e, f): [f].concat(loop(e));
				case EConst(CIdent(i)): [i];
				case _: core.Error.error("Invalid macro call", p);
			}
		}
		var path = switch (e.expr) {
			case EConst(CIdent(i)):
				var path = try {
					if (!PMap.mem(i, ctx.curclass.cl_statics)) {
						throw ocaml.Not_found.instance;
					}
					ctx.curclass.cl_path;
				}
				catch(_:ocaml.Not_found) {
					try {
						core.Type.t_infos(PMap.find(i, ctx.m.module_globals).a).mt_path;
					}
					catch(_:ocaml.Not_found) {
						core.Error.error("Invalid macro call", p);
					}
				}
				i :: path.b :: path.a;
			case _: loop(e);
		};
		return switch (path) {
			case meth::cl::path:
				{fst:new core.Path(List.rev(path), cl), snd:meth, trd:args};
			case _:
				core.Error.error("Invalid macro call", p);
		}
	}

	public static function build_macro_type (ctx:context.Typecore.Typer, pl:ImmutableList<core.Type.T>, p:core.Globals.Pos) : core.Type.T {
		var _tmp = switch (pl) {
			case [TInst({cl_kind:KExpr({expr:ECall(e, args)})}, _)],
				[TInst({cl_kind:KExpr({expr:EArrayDecl([{expr:ECall(e, args)}])})}, _)]:
					get_macro_path(ctx, e, args, p);
			case _:
				core.Error.error("MacroType requires a single expression call parameter", p);
		}
		var path = _tmp.fst; var field = _tmp.snd; var args = _tmp.trd;
		var old = ctx.ret;
		var t = switch (ctx.g.do_macro(ctx, MMacroType, path, field, args, p)) {
			case None: core.Type.mk_mono();
			case _: ctx.ret;
		}
		ctx.ret = old;
		return t;
	}

	public static function build_macro_build (ctx:context.Typecore.Typer, c:core.Type.TClass, pl:ImmutableList<core.Type.T>, cfl:ImmutableList<core.Ast.ClassField>, p:core.Globals.Pos) : core.Type.T {
		var _tmp = switch(core.Meta.get(GenericBuild, c.cl_meta)) {
			case {params:[{expr:ECall(e, args)}]}:
				get_macro_path(ctx, e, args, p);
			case _:
				core.Error.error("genericBuild requires a single expression call parameter", p);
		}
		var path = _tmp.fst; var field = _tmp.snd; var args = _tmp.trd;
		var old = {fst:ctx.ret, snd:ctx.g.get_build_infos};
		ctx.g.get_build_infos = function () {
			return Some({mt:TClassDecl(c), l:pl, cfs:cfl});
		}
		var t = switch (ctx.g.do_macro(ctx, MMacroType, path, field, args, p)) {
			case None: core.Type.mk_mono();
			case Some(_): ctx.ret;
		}
		ctx.ret = old.fst;
		ctx.g.get_build_infos = old.snd;
		return t;
	}

	// --------------------------------------------------------------------------
	// API EVENTS

	public static function build_instance (ctx:context.Typecore.Typer, mtype:core.Type.ModuleType, p:core.Globals.Pos) : {types:core.Type.TypeParams, path:core.Path, f:ImmutableList<core.Type.T>->core.Type.T} {
		return switch (mtype) {
			case TClassDecl(c):
				if (EnumValueTools.getIndex(ctx.pass) > EnumValueTools.getIndex(context.Typecore.TyperPass.PBuildClass)) {
					c.cl_build();
				}
				function build (f:Void->core.Type.T, s:String) : core.Type.T {
					var r = context.Typecore.exc_protect(ctx, function (r) {
						var t = core.Type.mk_mono();
						r.set(core.Type.lazy_processing(function () {return t;}));
						var tf = f();
						context.Typecore.unify_raise(ctx, tf, t, p);
						core.Type.link_dynamic(t, tf);
						if (EnumValueTools.getIndex(ctx.pass) > EnumValueTools.getIndex(context.Typecore.TyperPass.PBuildClass)) {
							context.Typecore.flush_pass(ctx, PBuildClass, "after_build_instance");
						}
						return t;
					}, s);
					return TLazy(r);
				}
				var ft = function (pl:ImmutableList<core.Type.T>) : core.Type.T {
					return switch (c.cl_kind) {
						case KGeneric: build(function() { return build_generic(ctx, c, p, pl); }, "build_generic");
						case KMacroType: build(function() { return build_macro_type(ctx, pl, p); }, "macro_type");
						case KGenericBuild(cfl): build(function() { return build_macro_build(ctx, c, pl, cfl, p); }, "generic_build");
						case _: TInst(c, pl);
					}
				}
				{types:c.cl_params, path:c.cl_path, f:ft};
			case TEnumDecl(e):
				{types:e.e_params, path:e.e_path, f:function (t) { return TEnum(e, t); }};
			case TTypeDecl(t):
				{types:t.t_params, path:t.t_path, f:function (tl) { return TType(t, tl); }};
			case TAbstractDecl(a):
				{types:a.a_params, path:a.a_path, f:function (t) { return TAbstract(a, t); }};
		};
	}

}