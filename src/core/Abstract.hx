package core;

import haxe.ds.ImmutableList;
import ocaml.List;
import ocaml.Ref;

class Abstract {

	public static function has_direct_to (ab:core.Type.TAbstract, pl:core.Type.TParams, b:core.Type.T) : Bool {
		return List.exists(core.Type.unify_to.bind(ab, pl, b, false), ab.a_to);
	}
	public static function has_direct_from (ab:core.Type.TAbstract, pl:core.Type.TParams, a:core.Type.T, b:core.Type.T) : Bool {
		return List.exists(core.Type.unify_from.bind(ab, pl, a, b, false), ab.a_from);
	}
	public static function find_field_to (ab:core.Type.TAbstract, pl:core.Type.TParams, b:core.Type.T) : {t:core.Type.T, cf:core.Type.TClassField} {
		return List.find(core.Type.unify_to_field.bind(ab, pl, b), ab.a_to_field);
	}
	public static function find_field_from (ab:core.Type.TAbstract, pl:core.Type.TParams, a:core.Type.T, b:core.Type.T) : {t:core.Type.T, cf:core.Type.TClassField} {
		return List.find(core.Type.unify_from_field.bind(ab, pl, a, b), ab.a_from_field);
	}
	public static function find_to_from (f:core.Type.TAbstract->core.Type.TParams->(Void->{t:core.Type.T, cf:core.Type.TClassField})->core.Type.TExpr, ab_left:core.Type.TAbstract, tl_left:core.Type.TParams, ab_right:core.Type.TAbstract, tl_right:core.Type.TParams, tleft:core.Type.T, tright:core.Type.T) : core.Type.TExpr {
		return
		if (has_direct_to(ab_right, tl_right, tleft) || has_direct_from(ab_left, tl_left, tright, tleft)) {
			throw ocaml.Not_found.instance;
		}
		else {
			try {
				f(ab_right, tl_right, function () { return find_field_to(ab_right, tl_right, tleft); });
			}
			catch (_:ocaml.Not_found) {
				f(ab_left, tl_left, function() { return find_field_from(ab_left, tl_left, tright, tleft); } );
			}
		}
	}
	public static function find_to (ab:core.Type.TAbstract, pl:core.Type.TParams, b:core.Type.T) : {t:core.Type.T, cf:core.Type.TClassField} {
		return
		if (core.Type.follow(b) == core.Type.t_dynamic) {
			List.find(function (arg:{t:core.Type.T, cf:core.Type.TClassField}) { var t = arg.t; return core.Type.follow(t) == core.Type.t_dynamic; }, ab.a_to_field);
		}
		else if (has_direct_to(ab, pl , b)) {
			throw ocaml.Not_found.instance; // legacy compatibility
		}
		else {
			find_field_to(ab, pl , b);
		}
	}
	public static function find_from (ab:core.Type.TAbstract, pl:core.Type.TParams, a:core.Type.T, b:core.Type.T) : {t:core.Type.T, cf:core.Type.TClassField}{
		return
		if (core.Type.follow(a) == core.Type.t_dynamic) {
			List.find(function (arg:{t:core.Type.T, cf:core.Type.TClassField}) { var t = arg.t; return core.Type.follow(t) == core.Type.t_dynamic; }, ab.a_from_field);
		}
		else if (has_direct_from(ab, pl, a, b)) {
			throw ocaml.Not_found.instance; // legacy compatibility
		}
		else {
			find_field_from(ab, pl, a, b);
		}
	}

	public static var underlying_type_stack = new Ref<core.Type.TParams>([]);

	public static function get_underlying_type (a:core.Type.TAbstract, pl:core.Type.TParams) : core.Type.T {
		function maybe_recurse (t:core.Type.T) {
			underlying_type_stack.set(core.Type.T.TAbstract(a, pl)::underlying_type_stack.get());
			function loop (t:core.Type.T) : core.Type.T {
				return switch(t) {
					case TMono(r):
						switch (r.get()) {
							case Some(t): loop(t);
							case _: t;
						}
					case TLazy(f):
						loop(core.Type.lazy_type(f));
					case TAbstract(a={a_path:{a:[], b:"Null"}}, [t1]):
						TAbstract(a, [loop(t1)]);
					case TType(t, tl):
						loop(core.Type.apply_params(t.t_params, tl, t.t_type));
					case TAbstract(a, tl) if (!core.Meta.has(CoreType, a.a_meta)):
						if (List.exists(core.Type.fast_eq.bind(t), underlying_type_stack.get())) {
							var pctx = core.Type.print_context();
							var s = List.join("->", List.map(function (t) {  return core.Type.s_type(pctx, t); }, List.rev(t::underlying_type_stack.get())));
							underlying_type_stack.set([]);
							core.Error.error("Abstract chain detected: "+s, a.a_pos);
						}
						get_underlying_type(a, tl);
					case _: t;
				}
			}
			var t = loop(t);
			underlying_type_stack.set(List.tl(underlying_type_stack.get()));
			return t;
		}
		return
		try {
			if (!core.Meta.has(MultiType, a.a_meta)) { throw ocaml.Not_found.instance; }
			var m = core.Type.mk_mono();
			find_to(a, pl, m);
			maybe_recurse(core.Type.follow(m));
		}
		catch (_:ocaml.Not_found) {
			if (core.Meta.has(CoreType, a.a_meta)) {
				core.Type.t_dynamic;
			}
			else {
				maybe_recurse(core.Type.apply_params(a.a_params, pl, a.a_this));
			}
		}
	}

	public static function follow_with_abstracts (t:core.Type.T) : core.Type.T {
		return switch (core.Type.follow(t)) {
			case TAbstract(a, tl) if (!core.Meta.has(CoreType, a.a_meta)):
				follow_with_abstracts(get_underlying_type(a, tl));
			case t: t;
		}
	}
}