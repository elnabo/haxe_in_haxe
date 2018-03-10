package optimization;

import haxe.ds.ImmutableList;
import haxe.ds.Option;

import ocaml.List;
import ocaml.PMap;
import ocaml.Ref;

using ocaml.Cloner;

enum Inline_object_kind {
	IOKCtor(cf:core.Type.TClassField, is_extern:Bool, vars:ImmutableList<core.Type.TVar>);
	IOKStructure;
	IOKArray(i:Int);
}

typedef Inline_object = {
	io_kind: Inline_object_kind,
	io_expr: core.Type.TExpr,
	io_pos: core.Globals.Pos,
	io_has_untyped: Bool,
	io_cancelled: Bool,
	io_declared: Bool,
	io_aliases: ImmutableList<Inline_var>,
	io_fields: PMap<String, Inline_var>,
	io_id_start: Int,
	io_id_end: Int
}

enum Inline_var_kind {
	IVKField(obj:Inline_object, s:String, eo:Option<core.Type.TExpr>);
	IVKLocal;
}

enum Inline_var_state {
	IVSUnassigned;
	IVSAliasing (obj:Inline_object);
	IVSCancelled;
}

typedef Inline_var = {
	iv_var: core.Type.TVar,
	iv_state: Inline_var_state,
	iv_kind: Inline_var_kind,
	iv_closed: Bool
}

class InlineConstructors {
	static inline function abs (i:Int) : Int {
		return Std.int(Math.abs(i));
	}

	public static function inline_constructors (ctx:context.Typecore.Typer, e:core.Type.TExpr) : core.Type.TExpr {
		var inline_objs = new Ref<PMap<Int, Inline_object>>(PMap.empty());
		var vars = new Ref<PMap<Int, Inline_var>>(PMap.empty());
		var scoped_ivs = new Ref<ImmutableList<Inline_var>>(Tl);
		function get_io (ioid:Int):Inline_object {
			return PMap.find(ioid, inline_objs.get());
		}
		function get_iv (vid:Int): Inline_var {
			return PMap.find(abs(vid), vars.get());
		}
		var cancel_iv:(Inline_var, core.Globals.Pos)->Void = null;
		var cancel_v:(core.Type.TVar, core.Globals.Pos)->Void = null;
		function cancel_io (io:Inline_object, p:core.Globals.Pos) : Void {
			if (!io.io_cancelled) {
				io.io_cancelled = true;
				List.iter(function (iv:Inline_var) { cancel_iv(iv, p); }, io.io_aliases);
				PMap.iter(function (_, iv:Inline_var) { cancel_iv(iv, p); }, io.io_fields);
				switch (io.io_kind) {
					case IOKCtor(_, is_extern, vars):
						List.iter(function (v) { if (v.v_id < 0) { cancel_v(v, p); }}, vars);
						if (is_extern) {
							context.Typecore.display_error(ctx, "Extern constructor could not be inlined", io.io_pos);
							context.Typecore.display_error(ctx, "Cancellation happened here", p);
						}
					case _:
				}
			}
		}
		cancel_iv = function (iv:Inline_var, p:core.Globals.Pos) {
			if (iv.iv_state != IVSCancelled) {
				var old = iv.iv_state;
				iv.iv_state = IVSCancelled;
				switch (old) {
					case IVSAliasing(io): cancel_io(io, p);
					case _:
				}
				var remove = switch (iv.iv_kind) {
					case IVKField(io, _, _): io.io_cancelled;
					case IVKLocal: true;
				}
				if (remove) {
					var v = iv.iv_var;
					vars.set(PMap.remove(abs(v.v_id), vars.get()));
					v.v_id = abs(v.v_id);
				}
			}
		}
		cancel_v = function (v:core.Type.TVar, p:core.Globals.Pos) {
			try {
				var iv = get_iv(v.v_id);
				cancel_iv(iv, p);
			}
			catch (_:ocaml.Not_found) {}
		}

		function set_iv_alias (iv:Inline_var, io:Inline_object) {
			if (iv.iv_state != IVSUnassigned || io.io_cancelled) {
				cancel_io(io, io.io_pos);
				cancel_iv(iv, io.io_pos);
			}
			else {
				iv.iv_state = IVSAliasing(io);
				io.io_aliases = iv :: io.io_aliases;
			}
		}

		function add (v:core.Type.TVar, kind:Inline_var_kind) : Inline_var {
			var iv = {
				iv_var: v,
				iv_state: IVSUnassigned,
				iv_kind: kind,
				iv_closed: false
			}
			v.v_id = -1*v.v_id;
			vars.set(PMap.add(abs(v.v_id), iv, vars.get()));
			return iv;
		}

		function get_io_field (io:Inline_object, s:String) : Inline_var {
			return PMap.find(s, io.io_fields);
		}

		function alloc_io_field_full (io:Inline_object, fname:String, constexpr_option:Option<core.Type.TExpr>, t:core.Type.T, p:core.Globals.Pos) : Inline_var {
			var v = core.Type.alloc_var(fname, t, p);
			var iv = add(v, IVKField(io, fname, constexpr_option));
			io.io_fields = PMap.add(fname, iv, io.io_fields);
			return iv;
		}

		function alloc_const_io_field (io:Inline_object, fname:String, constexpr:core.Type.TExpr) : Inline_var {
			var iv = alloc_io_field_full(io, fname, Some(constexpr), constexpr.etype, constexpr.epos);
			iv.iv_state = IVSCancelled;
			return iv;
		}

		function alloc_io_field (io:Inline_object, fname:String, t:core.Type.T, p:core.Globals.Pos) : Inline_var {
			return alloc_io_field_full(io, fname, None, t, p);
		}

		function int_field_name (i:Int) : String {
			return
			if (i<0) { "n"+(-1*i); }
			else { ""+i; }
		}

		function is_extern_ctor (c:core.Type.TClass, cf:core.Type.TClassField) : Bool { return c.cl_extern || core.Meta.has(Extern, cf.cf_meta); }

		function make_expr_for_list (el:ImmutableList<core.Type.TExpr>, t:core.Type.T, p:core.Globals.Pos) : core.Type.TExpr {
			return switch (el) {
				case []: return core.Type.mk(TBlock([]), ctx.t.tvoid, p);
				case [e]: e;
				case _: core.Type.mk(TBlock(el), t, p);
			}
		}

		function make_expr_for_rev_list (el:ImmutableList<core.Type.TExpr>, t:core.Type.T, p:core.Globals.Pos) : core.Type.TExpr {
			return make_expr_for_list(List.rev(el), t, p);
		}

		var current_io_id = new Ref(0);
		function increment_io_id (e:core.Type.TExpr) {
			switch (e.eexpr) {
				case TObjectDecl(_), TArrayDecl(_), TNew(_): current_io_id.set(current_io_id.get()+1);
				case _:
			}
		}

		function analyze_aliases (seen_ctors:ImmutableList<core.Type.TClassField>, captured:Bool, is_lvalue:Bool, e:core.Type.TExpr) : Option<Inline_var> {
			increment_io_id(e);
			function mk_io (?has_untyped:Bool=false, iok:Inline_object_kind, id:Int, expr:core.Type.TExpr) : Inline_object {
				var io = {
					io_kind: iok,
					io_expr: expr,
					io_pos: e.epos,
					io_cancelled: false,
					io_declared: false,
					io_fields: PMap.empty(),
					io_aliases: Tl,
					io_id_start: id,
					io_id_end: id,
					io_has_untyped: has_untyped
				};
				inline_objs.set(PMap.add(id, io, inline_objs.get()));
				return io;
			}
			function analyze_aliases_in_lvalue (e:core.Type.TExpr) {
				return analyze_aliases(seen_ctors, captured, true, e);
			}
			function analyze_aliases_in_ctor(cf:core.Type.TClassField, captured:Bool, e:core.Type.TExpr) {
				return analyze_aliases(cf::seen_ctors, captured, false, e);
			}
			function analyze_aliases_(captured:Bool, e:core.Type.TExpr) {
				return analyze_aliases(seen_ctors, captured, false, e);
			}
			function handle_field_case (te:core.Type.TExpr, fname:String, validate_io:Inline_object->Bool) : Option<Inline_var> {
				return switch (analyze_aliases_(true, te)) {
					case Some(iv={iv_state:IVSAliasing(io)}) if (validate_io(io)):
						try {
							var fiv = get_io_field(io, fname);
							if (!core.Type.type_iseq_strict(fiv.iv_var.v_type, e.etype)) {
								throw ocaml.Not_found.instance;
							}
							function iv_is_const(iv:Inline_var) { return iv.iv_kind.match(IVKField(_, _, Some(_))); }
							if (is_lvalue && iv_is_const(fiv)) { throw ocaml.Not_found.instance; }
							if (fiv.iv_closed) { throw ocaml.Not_found.instance; }
							if (!captured || (!is_lvalue && fiv.iv_state == IVSUnassigned)) {
								cancel_iv(fiv, e.epos);
							}
							Some(fiv);
						}
						catch (_:ocaml.Not_found) {
							cancel_iv(iv, e.epos);
							None;
						}
					case Some(iv):
						cancel_iv(iv, e.epos);
						None;
					case _: None;
				}
			}
			return
			switch [e.eexpr, e.etype] {
				case [TNew(c={cl_constructor:Some(cf={cf_kind:Method(MethInline), cf_expr:Some({eexpr:TFunction(tf)})})}, tl, pl), _] if (captured && !List.memq(cf, seen_ctors)):
					var io_id = current_io_id.get();
					function loop(tmp:{fst:ImmutableList<core.Type.TVar>, snd:ImmutableList<core.Type.TExpr>, trd:ImmutableList<core.Type.TExpr>}, el:ImmutableList<core.Type.TExpr>) : {fst:ImmutableList<core.Type.TVar>, snd:ImmutableList<core.Type.TExpr>, trd:ImmutableList<core.Type.TExpr>} {
						var vs = tmp.fst; var decls = tmp.snd; var es = tmp.trd;
						return switch (el) {
							case e :: el:
								switch (e.eexpr) {
									case TConst(_): loop({fst:vs, snd:decls, trd:e::es}, el);
									case _:
										var v = core.Type.alloc_var("arg", e.etype, e.epos);
										var decle = core.Type.mk(TVar(v, Some(e)), ctx.t.tvoid, e.epos);
										var io_id_start = current_io_id.get();
										analyze_aliases_(true, decle);
										var mde:core.Ast.MetadataEntry = {name:InlineConstructorArgument(v.v_id, io_id_start), params:[], pos:e.epos};
										var e = core.Type.mk(TMeta(mde, e), e.etype, e.epos);
										loop({fst:v::vs, snd:decle::decls, trd:e::es}, el);
								}
							case []: {fst:vs, snd:List.rev(decls), trd:List.rev(es)};
						}
					}
					var _tmp = loop({fst:Tl, snd:Tl, trd:Tl}, pl);
					var argvs = _tmp.fst; var argvdecls = _tmp.snd; var pl = _tmp.trd;
					var cname = c.cl_path.b;
					var v = core.Type.alloc_var("inl"+cname, e.etype, e.epos);
					switch (optimization.Optimizer.type_inline_ctor(ctx, c, cf, tf, core.Type.mk(TLocal(v), TInst(c, tl), e.epos), pl, e.epos)) {
						case Some(inlined_expr):
							var has_untyped = core.Meta.has(HasUntyped, cf.cf_meta);
							var io = mk_io(has_untyped, IOKCtor(cf, is_extern_ctor(c, cf), argvs), io_id, inlined_expr);
							function loop (c:core.Type.TClass, tl:ImmutableList<core.Type.T>) {
								var apply = core.Type.apply_params.bind(c.cl_params, tl);
								List.iter(function (cf:core.Type.TClassField) {
									switch [cf.cf_kind, cf.cf_expr] {
										case [Var(_), _]:
											var fieldt = apply(cf.cf_type);
											alloc_io_field(io, cf.cf_name, fieldt, v.v_pos);
										case _:
									}
								}, c.cl_ordered_fields);
								switch (c.cl_super) {
									case Some({c:c, params:tl}): loop(c, List.map(apply, tl));
									case None:
								}
							}
							loop(c, tl);
							var iv = add(v, IVKLocal);
							set_iv_alias(iv, io);
							io.io_id_start = current_io_id.get();
							analyze_aliases_in_ctor(cf, true, io.io_expr);
							io.io_id_end = current_io_id.get();
							Some(iv);
						case _:
							List.iter(function (v) { cancel_v(v, v.v_pos); }, argvs);
							if (is_extern_ctor(c, cf)) { context.Typecore.display_error(ctx, "Extern constructor could not be inlined", e.epos); }
							None;
					}
				case [TNew(c={cl_constructor:Some(cf={cf_kind:Method(MethInline), cf_expr:Some(_)})}, _, pl), _] if (is_extern_ctor(c, cf)):
					core.Error.error("Extern constructor could not be inlined", e.epos);
				case [TObjectDecl(fl), _] if (captured && fl != Tl && List.for_all(function (tmp) { var s = tmp.name; return syntax.Lexer.is_valid_identifier(s); }, fl)):
					var v = core.Type.alloc_var("inlobj", e.etype, e.epos);
					var ev = core.Type.mk(TLocal(v), v.v_type, e.epos);
					var el = List.map(function (tmp) {
						var s = tmp.name; var e = tmp.expr;
						var ef = core.Type.mk(TField(ev, FDynamic(s)), e.etype, e.epos);
						var e = core.Type.mk(TBinop(OpAssign, ef, e), e.etype, e.epos);
						return e;
					}, fl);
					var io_expr = make_expr_for_list(el, ctx.t.tvoid, e.epos);
					var io = mk_io(IOKStructure, current_io_id.get(), io_expr);
					List.iter(function (tmp) { var s = tmp.name; var e = tmp.expr; alloc_io_field(io, s, e.etype, v.v_pos); }, fl);
					var iv = add(v, IVKLocal);
					set_iv_alias(iv, io);
					List.iter(function (e) { analyze_aliases_(true, e); }, el);
					io.io_id_end = current_io_id.get();
					Some(iv);
				case [TArrayDecl(el), TInst(_, [elemtype])] if (captured):
					var len = List.length(el);
					var v = core.Type.alloc_var("inlarr", e.etype, e.epos);
					var ev = core.Type.mk(TLocal(v), v.v_type, e.epos);
					var el = List.mapi(function (i, e) {
						var ef = core.Type.mk(TArray(ev, core.Type.mk(TConst(TInt(i)), e.etype, e.epos)), elemtype, e.epos);
						return core.Type.mk(TBinop(OpAssign, ef, e), elemtype, e.epos);
					}, el);
					var io_expr = make_expr_for_list(el, ctx.t.tvoid, e.epos);
					var io = mk_io(IOKArray(len), current_io_id.get(), io_expr);
					alloc_const_io_field(io, "length", core.Type.mk(TConst(TInt(len)), ctx.t.tint, e.epos));
					for (i in 0...len) {
						alloc_io_field(io, int_field_name(i), elemtype, v.v_pos);
					}
					var iv = add(v, IVKLocal);
					set_iv_alias(iv, io);
					List.iter(function (e) { analyze_aliases_(true, e); }, el);
					io.io_id_end = current_io_id.get();
					Some(iv);
				case [TVar(v, None), _]: add(v, IVKLocal); None;
				case [TVar(v, Some(rve)), _]:
					switch (analyze_aliases_(true, rve)) {
						case Some({iv_state:IVSAliasing(io)}):
							var iv = add(v, IVKLocal);
							set_iv_alias(iv, io);
						case _:
					}
					None;
				case [TBinop(OpAssign, lve, rve), _]:
					switch (analyze_aliases_in_lvalue(lve)) {
						case Some(iv={iv_state:IVSUnassigned}):
							switch (analyze_aliases_(true, rve)) {
								case Some({iv_state:IVSAliasing(io)}):
									scoped_ivs.set(iv::scoped_ivs.get());
									set_iv_alias(iv, io);
								case _: cancel_iv(iv, lve.epos);
							}
							Some(iv);
						case Some(iv):
							cancel_iv(iv, e.epos);
							analyze_aliases_(false, rve);
							None;
						case _:
							analyze_aliases_(false, rve);
							None;
					}
				case [TField(te, fa), _]:
					handle_field_case(te, core.Type.field_name(fa), function(_) { return true; });
				case [TArray(te, {eexpr:TConst(TInt(i))}), _]:
					var i:haxe.Int32 = i;
					function validate_io (io:Inline_object) {
						return switch (io.io_kind) {
							case IOKArray(l) if (i >= 0 && i <l): true;
							case _: false;
						}
					}
					handle_field_case(te, int_field_name(i), validate_io);
				case [TLocal(v), _] if (v.v_id < 0):
					var iv = get_iv(v.v_id);
					if (iv.iv_closed || !captured) { cancel_iv(iv, e.epos); }
					Some(iv);
				case [TBlock(el), _]:
					function loop (el:ImmutableList<core.Type.TExpr>) {
						return switch (el) {
							case [e]: analyze_aliases_(captured, e);
							case e :: el: analyze_aliases_(true, e); loop(el);
							case []: None;
						}
					}
					loop(el);
				case [TMeta({name:InlineConstructorArgument(vid, _)}, _), _]:
					try {
						var iv = get_iv(vid);
						if (iv.iv_closed || !captured) { cancel_iv(iv, e.epos); }
						Some(get_iv(vid));
					}
					catch (_:ocaml.Not_found) {
						None;
					}
				case [TParenthesis(e), _], [TMeta(_, e), _], [TCast(e, None), _]:
					analyze_aliases_(captured, e);
				case [_, _]:
					var old = scoped_ivs.get();
					scoped_ivs.set([]);
					function f(e:core.Type.TExpr) { analyze_aliases_(false, e); }
					core.Type.iter(f, e);
					List.iter(function (iv) { iv.iv_closed = true; }, scoped_ivs.get());
					scoped_ivs.set(old);
					None;
			}
		}
		analyze_aliases([], false, false, e);
		current_io_id.set(0);
		var get_io_var_decls:Inline_object->ImmutableList<core.Type.TExpr> = null;
		function get_iv_var_decls(iv:Inline_var):ImmutableList<core.Type.TExpr> {
			return switch (iv) {
				case {iv_state:IVSAliasing(io)}: get_io_var_decls(io);
				case {iv_kind: IVKField(_,_,Some(_))}: [];
				case {iv_state:IVSCancelled}:
					var v = iv.iv_var;
					[core.Type.mk(TVar(v, None), ctx.t.tvoid, v.v_pos)];
				case _: [];
			}
		}
		get_io_var_decls = function (io:Inline_object) {
			if (io.io_declared) { return []; }
			else {
				io.io_declared = true;
				return PMap.foldi(function (_, iv, acc) {
					return List.append(acc, get_iv_var_decls(iv));
				}, io.io_fields, Tl);
			}
		}
		var included_untyped = new Ref(false);
		function final_map (?unwrap_block:Bool=false, e:core.Type.TExpr) : {fst:ImmutableList<core.Type.TExpr>, snd:Option<Inline_object>} {
			increment_io_id(e);
			function default_case(e:core.Type.TExpr) : {fst:ImmutableList<core.Type.TExpr>, snd:Option<Inline_object>}{
				function f(e:core.Type.TExpr) {
					var el = final_map(e).fst;
					return make_expr_for_rev_list(el, e.etype, e.epos);
				}
				return {fst:[core.Type.map_expr(f, e)], snd:None};
			}
			return
			switch (e.eexpr) {
				case TObjectDecl(_), TArrayDecl(_), TNew(_):
					try {
						var io = get_io(current_io_id.get());
						if (io.io_cancelled) {
							var result = default_case(e);
							current_io_id.set(io.io_id_end);
							result;
						}
						else {
							if (io.io_has_untyped) { included_untyped.set(true); }
							current_io_id.set(io.io_id_start);
							var el = final_map(true, io.io_expr).fst;
							var el = List.append(el, get_io_var_decls(io));
							// assert !current_io_id = io.io_id_end
							if (current_io_id.get() != io.io_id_end) {
								trace("Shall not be seen"); std.Sys.exit(255);
							}
							{fst:el, snd:Some(io)};
						}
					}
					catch (_:ocaml.Not_found) {
						default_case(e);
					}
				case TVar(v, None) if (v.v_id < 0):
					{fst:get_iv_var_decls(get_iv(v.v_id)), snd:None};
				case TVar(v, Some(e)) if (v.v_id < 0):
					var el = get_iv_var_decls(get_iv(v.v_id));
					var e = final_map(true, e).fst;
					{fst:List.append(e, el), snd:None};
				case TBinop(OpAssign, lve, rve):
					var _tmp = final_map(lve);
					var lvel = _tmp.fst; var lvo = _tmp.snd;
					var _tmp = final_map(rve);
					var rvel = _tmp.fst; var rvo = _tmp.snd;
					switch (lvo) {
						case Some(io):
							{fst:List.append(rvel, lvel), snd:lvo};
						case None:
							var rve = make_expr_for_rev_list(rvel, rve.etype, rve.epos);
							switch (lvel) {
								case []: trace("Shall not be seen"); std.Sys.exit(255); throw false;
								case e :: el:
									var e = core.Type.mk(TBinop(OpAssign, e, rve), e.etype, e.epos);
									{fst:e::el, snd:None};
							}
					}
				case TField(te, fa):
					var _tmp = final_map(te);
					var tel = _tmp.fst; var thiso = _tmp.snd;
					switch (thiso) {
						case Some(io):
							var fname = core.Type.field_name(fa);
							switch (get_io_field(io, fname)) {
								case {iv_state: IVSAliasing(io)}:
									{fst:tel, snd:Some(io)};
								case iv:
									var newexpr = switch (iv.iv_kind) {
										case IVKField(_,_,Some(constexpr)):
											constexpr.with({epos:e.epos});
										case _: core.Type.mk(TLocal(iv.iv_var), e.etype, e.epos);
									}
									{fst:newexpr::tel, snd:None};
							}
						case None:
							var te = make_expr_for_rev_list(tel, te.etype, te.epos);
							{fst:[core.Type.mk(TField(te, fa), e.etype, e.epos)], snd:None};
					}
				case TArray(te, indexexpr={eexpr:TConst(TInt(i))}):
					var _tmp = final_map(te);
					var tel = _tmp.fst; var thiso = _tmp.snd;
					switch (thiso) {
						case Some(io):
							var i:Int = i;
							var fname = int_field_name(i);
							switch (get_io_field(io, fname)) {
								case {iv_state:IVSAliasing(io)}:
									{fst:tel, snd:Some(io)};
								case iv:
									var local = core.Type.mk(TLocal(iv.iv_var), e.etype, e.epos);
									{fst:local::tel, snd:None};
							}
						case None:
							var te = make_expr_for_rev_list(tel, te.etype, te.epos);
							{fst:[core.Type.mk(TArray(te, indexexpr), e.etype, e.epos)], snd:None};
					}
				case TLocal(v) if (v.v_id < 0):
					switch (get_iv(v.v_id)) {
						case {iv_state:IVSAliasing(io)}:
							{fst:[], snd:Some(io)};
						case iv:
							{fst:[core.Type.mk(TLocal(iv.iv_var), e.etype, e.epos)], snd:None};
					}
				case TBlock(el):
					function loop(acc:ImmutableList<core.Type.TExpr>, el:ImmutableList<core.Type.TExpr>) : {fst:ImmutableList<core.Type.TExpr>, snd:Option<Inline_object>} {
						return switch (el) {
							case []: {fst:acc, snd:None};
							case [e]:
								var _tmp = final_map(unwrap_block, e);
								var el_ = _tmp.fst; var io = _tmp.snd;
								{fst:List.append(el_, acc), snd:io};
							case e :: el:
								var el_ = final_map(unwrap_block, e).fst;
								loop(List.append(el_, acc), el);
						}
					}
					var _tmp = loop([], el);
					var el = _tmp.fst; var io = _tmp.snd;
					var el:ImmutableList<core.Type.TExpr> = (unwrap_block || ocaml.Option.is_some(io)) ? el : [core.Type.mk(TBlock(List.rev(el)), e.etype, e.epos)];
					{fst:el, snd:io};
				case TMeta({name:InlineConstructorArgument(_, io_id_start)}, e):
					var old_io_id = current_io_id.get();
					current_io_id.set(io_id_start);
					var result = final_map(e);
					current_io_id.set(old_io_id);
					result;
				case TParenthesis(e_), TCast(e_, None), TMeta(_, e_):
					var _tmp = final_map(e_);
					var el = _tmp.fst; var io = _tmp.snd;
					switch (io) {
						case Some(io): {fst:el, snd:Some(io)};
						case None:
							var e_ = make_expr_for_rev_list(el, e_.etype, e_.epos);
							{fst:[core.Type.map_expr(function (_) { return e_; }, e)], snd:None};
					}
				case _: default_case(e);
			}
		}
		return
		if (PMap.for_all(function (_, io:Inline_object) { return io.io_cancelled; }, inline_objs.get())) {
			PMap.iter(function (_, iv:Inline_var) {
				var v = iv.iv_var;
				if (v.v_id < 0) { v.v_id = -1*v.v_id; }
			}, vars.get());
			e;
		}
		else {
			var el = final_map(e).fst;
			var cf = ctx.curfield;
			if (included_untyped.get() && !core.Meta.has(HasUntyped, cf.cf_meta)) {
				cf.cf_meta = ({name:HasUntyped, params:[], pos:e.epos} : core.Ast.MetadataEntry) :: cf.cf_meta;
			}
			var e = make_expr_for_rev_list(el, e.etype, e.epos);
			function get_pretty_name (iv:Inline_var) : String {
				return switch (iv.iv_kind) {
					case IVKField(io, fname, None):
						try {
							function is_user_variable (iv:Inline_var) { return core.Meta.has(UserVariable, iv.iv_var.v_meta); }
							var iv = List.find(is_user_variable, io.io_aliases);
							get_pretty_name(iv) + "_" + fname;
						}
						catch (_:ocaml.Not_found) {
							get_pretty_name(List.hd(io.io_aliases)) + "_" + fname;
						}
					case _: iv.iv_var.v_name;
				}
			}
			PMap.iter(function (_, iv:Inline_var) {
				var v = iv.iv_var;
				if (v.v_id < 0) {
					v.v_id = -1 * v.v_id;
					v.v_name = get_pretty_name(iv);
				}
			}, vars.get());
			e;
		}
	}

}