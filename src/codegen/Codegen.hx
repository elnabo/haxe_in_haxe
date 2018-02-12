package codegen;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
import ocaml.List;
import ocaml.PMap;

class Codegen {

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
					if (v.c.cl_path == new core.Path(["cpp"], "FastIterator")) {
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
							if (find_field(com, c, f) == f) { throw ocaml.Not_found.instance; }
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
}