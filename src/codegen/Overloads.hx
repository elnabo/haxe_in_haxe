package codegen;

import haxe.ds.Option;

class Overloads {
	public static function same_overload_args (?get_vmtype:Option<core.Type.T->core.Type.T>, t1:core.Type.T, t2:core.Type.T, f1:core.Type.TClassField, f2:core.Type.TClassField) : Bool {
		var get_vmtype = switch (get_vmtype) {
			case None: function (f) { return f; };
			case Some(f): f;
		}
		if (f1.cf_params.length != f2.cf_params.length) {
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
				case TAbstract(a={a_path:path}, arr) if (path.a.length==0 && path.b == "Null" && arr.length == 1):
					TAbstract(a, [core.Type.follow(arr[0])]);
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
		
		var _tmp = core.Type.follow(core.Type.apply_params(f1.cf_params, f2.cf_params.map(function (a) { return a.t; }), t1));
		return switch ({fst:_tmp, snd:core.Type.follow(t2)}) {
			case {fst:TFun(f1), snd:TFun(f2)}:
				try {
					ocaml.List.for_all2(function (arg1, arg2) {
						return same_arg(arg1.t, arg2.t);
					}, f1.args, f2.args);
				}
				catch (_:ocaml.Invalid_argument) {
					false;
				}
			case _: throw false;
		}
	}
}