package codegen;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
import ocaml.List;
import ocaml.PMap;

class Overloads {
	public static function same_overload_args (?get_vmtype:Option<core.Type.T->core.Type.T>, t1:core.Type.T, t2:core.Type.T, f1:core.Type.TClassField, f2:core.Type.TClassField) : Bool {
		var get_vmtype = switch (get_vmtype) {
			case None: function (f) { return f; };
			case Some(f): f;
		}
		if (List.length(f1.cf_params) != List.length(f2.cf_params)) {
			return false;
		}
		function follow_skip_null (t:core.Type.T) : core.Type.T {
			return switch (t) {
				case TMono(r):
					switch (r.get()) {
						case Some(t): follow_skip_null(t);
						case _: t;
					}
				case TLazy(f):
					follow_skip_null(core.Type.lazy_type(f));
				case TAbstract(a={a_path:{a:[], b:"Null"}}, [p]):
					TAbstract(a, [core.Type.follow(p)]);
				case TType (t, tl):
					follow_skip_null(core.Type.apply_params(t.t_params, tl, t.t_type));
				case _: t;
			}
		}
		function same_arg (t1:core.Type.T, t2:core.Type.T) : Bool {
			var t1 = get_vmtype(follow_skip_null(t1));
			var t2 = get_vmtype(follow_skip_null(t2));
			return switch ({fst:t1, snd:t2}) {
				case {fst:TType(_), snd:TType(_)}: core.Type.type_iseq(t1, t2);
				case {fst:TType(_)}: false;
				case {snd:TType(_)}: false;
				case _: core.Type.type_iseq(t1, t2);
			}
		}
		
		var _tmp = core.Type.follow(core.Type.apply_params(f1.cf_params, List.map(function (a) { return a.t; }, f2.cf_params), t1));
		return switch ({fst:_tmp, snd:core.Type.follow(t2)}) {
			case {fst:TFun(f1), snd:TFun(f2)}:
				try {
					List.for_all2(function (arg1, arg2) {
						return same_arg(arg1.t, arg2.t);
					}, f1.args, f2.args);
				}
				catch (_:ocaml.Invalid_argument) {
					false;
				}
			case _: throw false;
		}
	}

	// retrieves all overloads from class c and field i, as (Type.t * tclass_field) list
	public static function get_overloads (c:core.Type.TClass, i:String) : ImmutableList<{fst:core.Type.T, snd:core.Type.TClassField}> {
		var ret:ImmutableList<{fst:core.Type.T, snd:core.Type.TClassField}> = try {
			var f = PMap.find(i, c.cl_fields);
			switch (f.cf_kind) {
				case Var(_):
					// @:libType may generate classes that have a variable field in a superclass of an overloaded method
					[];
				case Method(_):
					{fst:f.cf_type, snd:f} :: List.map(function (f:core.Type.TClassField) { return {fst:f.cf_type , snd:f}; }, f.cf_overloads);
			}
		}
		catch (_:ocaml.Not_found) { 
			Tl; // [];
		}
		var rsup = switch (c.cl_super) {
			case None if (c.cl_interface):
				var ifaces = List.concat(List.map(function (p1:{c:core.Type.TClass, params:core.Type.TParams}) {
					var c = p1.c; var tl = p1.params;
					return List.map(function (p2) {
						var t = p2.fst; var f = p2.snd;
						return {fst:core.Type.apply_params(c.cl_params, tl, t), snd:f};
					}, get_overloads(c, i));
				}, c.cl_implements));
				List.append(ret, ifaces);
			case None: ret;
			case Some({c:c, params:tl}):
				List.append(ret, List.map(function (p) {
					var t = p.fst; var f = p.snd;
					return {fst:core.Type.apply_params(c.cl_params, tl, t), snd:f};
				}, get_overloads(c, i)));
		}

		return List.append(ret, List.filter(function (p:{fst:core.Type.T, snd:core.Type.TClassField}) {
				var t = p.fst; var f = p.snd;
				return !List.exists(function (p2:{fst:core.Type.T, snd:core.Type.TClassField}) {
					var t2 = p2.fst; var f2 = p2.snd;
					return same_overload_args(None, t, t2, f, f2);
				}, ret);
			}, rsup));
	}
}