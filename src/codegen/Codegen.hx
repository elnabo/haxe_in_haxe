package codegen;

import core.Type.TClass;
import core.Type.TClassField;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
import ocaml.List;
import ocaml.PMap;
import ocaml.Ref;

using equals.Equal;

class Dump {
	public static function dump_types_(com:context.Common.Context, s_expr:core.Type.TExpr->String) : Void {
		trace("TODO: dump_types_");
		throw false;
	}

	public static function dump_types (com:context.Common.Context) : Void {
		trace("TODO: dump_types");
		throw false;
	}

	public static function dump_dependencies (?target_override:Option<String>=null, com:context.Common.Context) : Void {
		if (target_override == null) { target_override = None; }
		trace("TODO: dump_dependencies");
		throw false;
	}
}

class UnificationCallback {
	public static final tf_stack = new Ref(Tl);

	public static function check_call_params (f:(core.Type.TExpr, core.Type.T)->core.Type.TExpr, el:ImmutableList<core.Type.TExpr>, tl:ImmutableList<core.Type.TSignatureArg>) {
		function loop (acc:ImmutableList<core.Type.TExpr>, el:ImmutableList<core.Type.TExpr>, tl:ImmutableList<core.Type.TSignatureArg>) {
			return switch [el, tl] {
				case [e :: el, a :: tl]:
					var t = a.t;
					loop(f(e, t)::acc, el, tl);
				case [[], []]:
					acc;
				case [[], _]:
					acc;
				case [e :: el, []]:
					loop(e::acc, el, []);
			}
		}
		return List.rev(loop([], el, tl));
	}

	public static function check_call (f:(core.Type.TExpr, core.Type.T)->core.Type.TExpr, el:ImmutableList<core.Type.TExpr>, t:core.Type.T) {
		return switch (core.Type.follow(t)) {
			case TFun({args:args}):
				check_call_params(f, el, args);
			case _:
				List.map(function (e:core.Type.TExpr) { return f(e, core.Type.t_dynamic); }, el);
		}
	}
}

class Codegen {

	/* -------------------------------------------------------------------------- */
	/* TOOLS */

	public static function has_properties (c:TClass) : Bool {
		return List.exists(function (f:TClassField) {
			return switch (f.cf_kind) {
				case Var({v_read:AccCall}): true;
				case Var({v_write:AccCall}): true;
				case _ if (core.Meta.has(Accessor, f.cf_meta)): true;
				case _: false;
			}
		}, c.cl_ordered_fields) || (switch (c.cl_super) { case Some({c:c}): has_properties(c); case _: false;});
	}

	public static function get_properties (fields:ImmutableList<TClassField>) : ImmutableList<{fst:String, snd:String}> {
		return List.fold_left( function (acc:ImmutableList<{fst:String, snd:String}>, f:TClassField) {
			return
			if (core.Meta.has(Accessor, f.cf_meta)) {
				{fst:f.cf_name, snd:f.cf_name} :: acc;
			}
			else {
				var acc =  switch (f.cf_kind) {
					case Var({v_read:AccCall}): {fst:"get_"+f.cf_name, snd:"get_"+f.cf_name} :: acc;
					case _: acc;
				}
				switch (f.cf_kind) {
					case Var({v_write:AccCall}): {fst:"set_"+f.cf_name, snd:"set_"+f.cf_name} :: acc;
					case _: acc;
				}
			}
		}, Tl, fields);
	}

	public static function update_cache_dependencies (t:core.Type.ModuleType) : Void {
		var check_field:(m:core.Type.ModuleDef, cf:core.Type.TClassField)->Void = null;
		function check_t (m:core.Type.ModuleDef, t:core.Type.T) : Void {
			switch (t) {
				case TInst(c, tl):
					core.Type.add_dependency(m, c.cl_module);
					List.iter(check_t.bind(m), tl);
				case TEnum(en, tl):
					core.Type.add_dependency(m, en.e_module);
					List.iter(check_t.bind(m), tl);
				case TType(t, tl):
					core.Type.add_dependency(m, t.t_module);
					List.iter(check_t.bind(m), tl);
				case TAbstract(a, tl):
					core.Type.add_dependency(m, a.a_module);
					List.iter(check_t.bind(m), tl);
				case TFun({args:targs, ret:tret}):
					List.iter(function (arg) { var t = arg.t; check_t(m, t); }, targs);
					check_t(m, tret);
				case TAnon(an):
					PMap.iter(function (_, cf) { check_field(m, cf); }, an.a_fields);
				case TMono(r):
					switch (r.get()) {
						case Some(t): check_t(m, t);
						case _:
					}
				case TLazy(f):
					check_t(m, core.Type.lazy_type(f));
				case TDynamic(_.get()=>t):
					if (t == core.Type.t_dynamic) {}
					else {
						check_t(m, t);
					}
			}
		}
		check_field = function (m, cf) {
			check_t(m, cf.cf_type);
		}
		switch (t) {
			case TClassDecl(c):
				List.iter(check_field.bind(c.cl_module), c.cl_ordered_statics);
				List.iter(check_field.bind(c.cl_module), c.cl_ordered_fields);
				switch (c.cl_constructor) {
					case None:
					case Some(cf): check_field(c.cl_module,cf);
				}
			case _:
		}
	}

	/* -------------------------------------------------------------------------- */
	/* FIX OVERRIDES */

	/*
	 * on some platforms which doesn't support type parameters, we must have the
	 * exact same type for overridden/implemented function as the original one
	 */

	public static function find_field (com:context.Common.Context, c:core.Type.TClass, f:core.Type.TClassField) {
		return try {
			switch (c.cl_super) {
				case None:
					throw ocaml.Not_found.instance;
				case Some(v):
					if (v.c.cl_path.equals(new core.Path(["cpp"], "FastIterator"))) {
						throw ocaml.Not_found.instance; // This is a strongly typed 'extern' and the usual rules don't apply
					}
					find_field(com, v.c, f);

			}
		}
		catch (_:ocaml.Not_found) {
			try {
				if (com.platform == Cpp || com.platform == Hl) { // uses delegation for interfaces
					throw ocaml.Not_found.instance;
				}
				function loop (arr:ImmutableList<{c:core.Type.TClass, params:core.Type.TParams}>) {
					switch (arr) {
						case []: throw ocaml.Not_found.instance;
						case {c:c}::l:
							try {
								return find_field(com, c, f);
							}
							catch (_:ocaml.Not_found) {
								return loop(l);
							}
					}
				}
				loop(c.cl_implements);

			}
			catch (_:ocaml.Not_found) {
				var f = PMap.find(f.cf_name, c.cl_fields);
				switch (f.cf_kind) {
					case Var({v_read:AccRequire(_)}):
						throw ocaml.Not_found.instance;
					case _:
				}
				f;
			}
		}
	}

	public static function fix_override (com:context.Common.Context, c:core.Type.TClass, f:core.Type.TClassField, fd:Option<core.Type.TFunc>) {
		trace("TODO-FINISH: codegen.Codegen.fix_override");
		var f2 = try {
			Some(find_field(com, c, f));
		}
		catch (_:ocaml.Not_found) {
			None;
		}
		throw false;
		switch (f2) {
			case Some(ff2):
				switch (core.Type.follow(ff2.cf_type)) {
					case TFun(tf):
						switch (fd) {
							case Some(ffd):
								var changed_argfs = [];
								var prefix = "_tmp_";
								var nargs = [];
								// for (i in 0...tf.args.length) {
								// 	var cur = ffd.tf_args[i];
								// 	var other = tf.args;
								// }

								// 		let targs, tret = (match follow f2.cf_type with TFun (args,ret) -> args, ret | _ -> assert false) in
								// 		let changed_args = ref [] in
								// 		let prefix = "_tmp_" in
								// 		let nargs = List.map2 (fun ((v,ct) as cur) (_,_,t2) ->
								// 			try
								// 				type_eq EqStrict (monomorphs c.cl_params (monomorphs f.cf_params v.v_type)) t2;
								// 				(* Flash generates type parameters with a single constraint as that constraint type, so we
								// 				have to detect this case and change the variable (issue #2712). *)
								// 				begin match follow v.v_type with
								// 					| TInst({cl_kind = KTypeParameter [tc]} as cp,_) when com.platform = Flash ->
								// 						if List.mem_assoc (snd cp.cl_path) c.cl_params then raise (Unify_error [])
								// 					| _ ->
								// 						()
								// 				end;
								// 				cur
								// 			with Unify_error _ ->
								// 				let v2 = alloc_var (prefix ^ v.v_name) t2 v.v_pos in
								// 				changed_args := (v,v2) :: !changed_args;
								// 				v2,ct
								// 		) fd.tf_args targs in
								// 		let fd2 = {
								// 			tf_args = nargs;
								// 			tf_type = tret;
								// 			tf_expr = (match List.rev !changed_args with
								// 				| [] -> fd.tf_expr
								// 				| args ->
								// 					let e = fd.tf_expr in
								// 					let el = (match e.eexpr with TBlock el -> el | _ -> [e]) in
								// 					let p = (match el with [] -> e.epos | e :: _ -> e.epos) in
								// 					let el_v = List.map (fun (v,v2) ->
								// 						mk (TVar (v,Some (mk (TCast (mk (TLocal v2) v2.v_type p,None)) v.v_type p))) com.basic.tvoid p
								// 					) args in
								// 					{ e with eexpr = TBlock (el_v @ el) }
								// 			);
								// 		} in
								// 		(* as3 does not allow wider visibility, so the base method has to be made public *)
								// 		if Common.defined com Define.As3 && f.cf_public then f2.cf_public <- true;
								// 		let targs = List.map (fun(v,c) -> (v.v_name, Option.is_some c, v.v_type)) nargs in
								// 		let fde = (match f.cf_expr with None -> assert false | Some e -> e) in
								// 		f.cf_expr <- Some { fde with eexpr = TFunction fd2 };
								// 		f.cf_type <- TFun(targs,tret);
							case None:
								switch (core.Type.follow(ff2.cf_type)) {
									case TFun(tf):
										f.cf_type = core.Type.T.TFun({args:tf.args, ret:tf.ret});
									default:
										throw false;
								}
						}
					default: throw false;
				}

			case None:
		}
	}

	public static function fix_overrides (com:context.Common.Context, t:core.Type.ModuleType) {
		switch (t) {
			case TClassDecl(c):
				// overrides can be removed from interfaces
				if (c.cl_interface) {
					c.cl_ordered_fields = List.filter(function (f:core.Type.TClassField) {
						try {
							if (find_field(com, c, f).equals(f)) { throw ocaml.Not_found.instance; }
							c.cl_fields = PMap.remove(f.cf_name, c.cl_fields);
							return false;
						}
						catch (_:ocaml.Not_found) {
							return true;
						}
					}, c.cl_ordered_fields);
				}

				List.iter( function (f:core.Type.TClassField) {
					switch ({f:f.cf_expr, s:f.cf_kind}) {
						case {f:Some({eexpr:TFunction(fd)}), s:Method(MethNormal|MethInline)}:
							fix_override(com, c, f, Some(fd));
						case {f:None, s:Method(MethNormal|MethInline)} if (c.cl_interface):
							fix_override(com, c, f, None);
						case _:
					}
				}, c.cl_ordered_fields);
			default:
		}
	}

	public static function map_source_header (com:context.Common.Context, f:String->Void) : Void {
		return switch (context.Common.defined_value_safe(com, SourceHeader)) {
			case "":
			case s: f(s);
		}
	}

	/* -------------------------------------------------------------------------- */
	/* MISC FEATURES */

	public static inline function bytes_serialize (data:String) {
		return haxe.crypto.Base64.encode(haxe.io.Bytes.ofString(data));
	}
}