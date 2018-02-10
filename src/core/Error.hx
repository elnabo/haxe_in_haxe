package core;

enum CallError {
	Not_enough_arguments (l:Array<core.Type.TSignatureArg>);
	Too_many_arguments;
	Could_not_unify (error_msg:ErrorMsg);
	Cannot_skip_non_nullable (s:String);
}

enum ErrorMsg {
	Module_not_found (path:core.Path);
	Type_not_found (path:core.Path, s:String);
	Unify (l:Array<core.Type.UnifyError>);
	Custom (s:String);
	Unknown_ident (s:String);
	Stack (e1:ErrorMsg, e2:ErrorMsg);
	Call_error (ce:CallError);
	No_constructor (mt:core.Type.ModuleType);
}


// ocaml: exception Error
// class ErrorException {
// 	public var error_msg:ErrorMsg;
// 	public var pos:core.Globals.Pos;
// 	public function new(error_msg:ErrorMsg, pos:core.Globals.Pos) {
// 		this.error_msg = error_msg;
// 		this.pos = pos;
// 	}
// }

class Fatal_error {
	public var s:String;
	public var pos:core.Globals.Pos;
	public function new(s:String, pos:core.Globals.Pos) {
		this.s = s;
		this.pos = pos;
	}
}

class Error {
	public var msg:ErrorMsg;
	public var pos:core.Globals.Pos;
	public function new(msg:ErrorMsg, pos:core.Globals.Pos) {
		this.msg = msg;
		this.pos = pos;
	}

	// public static function of(msg:ErrorMsg, pos:{file:String, min:Int, max:Int}) {
		// return new Error(msg, new core.Globals.Pos(pos.file, pos.min, pos.max));
	// }
	public static function string_source(t:core.Type.T) : Array<String> {
		return switch (t) {
			case TInst(c, _): c.cl_ordered_fields.map(function (cf) {return cf.cf_name;});
			case TAnon(a): ocaml.PMap.fold(function (cf, acc:Array<String>) {acc.unshift(cf.cf_name); return acc;}, a.a_fields, []);
			case TAbstract({a_impl:Some(c)}, _): c.cl_ordered_statics.map(function (cf) {return cf.cf_name;});
			case _: [];
		};
	}

	public static function short_type (ctx, t:core.Type.T) : String {
		var tstr = core.Type.s_type(ctx, t);
		return (tstr.length > 150) ? tstr.substr(0 ,147) + "..." : tstr;
	}

	public static function unify_error_msg (ctx, err:core.Type.UnifyError) : String {
		return switch (err) {
			case Cannot_unify (t1, t2):
				core.Type.s_type(ctx, t1) + " should be " + core.Type.s_type(ctx, t2);
			case Invalid_field_type (s):
				"Invalid type for field " + s + " :";
			case Has_no_field (t, n):
				core.type.StringError.string_error(n, string_source(t), short_type(ctx, t)) + " has no field "+n;
			case Has_no_runtime_field (t, n):
				core.Type.s_type(ctx, t)+"."+n+" is not accessible at runtime";
			case Has_extra_field (t, n):
				short_type(ctx, t) + " has extra field "+n;
			case Invalid_kind (f, a, b):
				switch ({fst:a, snd:b}) {
					case {fst:Var(va), snd:Var(vb)}:
						var name:String; var stra:String; var strb:String;
						if (va.v_read == vb.v_read) {
							name = "setter"; stra = core.Type.s_access(false, va.v_write); strb = core.Type.s_access(false, vb.v_write);
						}
						else if (va.v_write == vb.v_write) {
							name = "getter"; stra = core.Type.s_access(true, va.v_read); strb = core.Type.s_access(true, vb.v_read);
						}
						else {
							name = "access"; stra = "("+core.Type.s_access(true, va.v_read)+","+core.Type.s_access(false, va.v_write)+")"; strb = "("+core.Type.s_access(true, vb.v_read)+","+core.Type.s_access(false, vb.v_write)+")";
						}
						"Inconsistent " + name + " for field " + f + " : " + stra + " should be " + strb;
					case _:
						"Field " + f + " is " + core.Type.s_kind(a) + " but should be " + core.Type.s_kind(b);
				}
			case Invalid_visibility (n):
				"The field " + n + " is not public";
			case Not_matching_optional (n):
				"Optional attribute of parameter " + n + " differs";
			case Cant_force_optional:
				"Optional parameters can't be forced";
			case Invariant_parameter (_):
				"Type parameters are invariant";
			case Constraint_failure (name):
				"Constraint check failure for " + name;
			case Missing_overload (cf, t):
				cf.cf_name + " has no overload for " + core.Type.s_type(ctx, t);
			case Unify_custom (msg):
				msg;
		};
	}

	public static function error_msg (m:ErrorMsg) : String {
		return switch (m) {
			case Module_not_found(m): "Type not found : "+core.Globals.s_type_path(m);
			case Type_not_found(m, t): "Module "+core.Globals.s_type_path(m)+" does not define type "+t;
			case Unify(l):
				var ctx = core.Type.print_context();
				l.map(unify_error_msg.bind(ctx)).join("\n");
			case Unknown_ident(s): "Unknown identifier : "+s;
			case Custom(s): s;
			case Stack(m1, m2):
				error_msg(m1) + "\n" + error_msg(m2);
			case Call_error(err): s_call_error(err);
			case No_constructor(mt): core.Globals.s_type_path(core.Type.t_infos(mt).mt_path) + " does not have a constructor";

		}
	}

	public static function s_call_error (e:CallError) : String {
		return switch (e) {
			case Not_enough_arguments(tl):
				var pctx = core.Type.print_context();
				"Not enough arguments, expected "+tl.map(function (arg) { return arg.name + ":"+short_type(pctx, arg.t); }).join(", ");
			case Too_many_arguments: "Too many arguments";
			case Could_not_unify (err): error_msg(err);
			case Cannot_skip_non_nullable(s): "Cannot skip non-nullable argument "+s;
		}
	}
	
	public static function error(msg:String, p:core.Globals.Pos) : Dynamic {
		throw new Error(Custom(msg),p);
	}

	public static function raise_error(err:ErrorMsg, p:core.Globals.Pos) : Dynamic {
		throw new Error(err, p);
	}

}