package context.display;

import ocaml.Hashtbl;
import ocaml.Ref;

using equals.Equal;

class DeprecationCheck {

	public static var curclass = new Ref<core.Type.TClass>(core.Type.null_class());
	public static var warned_positions = new Map<core.Globals.Pos, Bool>();

	public static function print_deprecation_message (com:context.Common.Context, meta:core.Ast.MetadataEntry, s:String, p_usage:core.Globals.Pos) : Void {
		var s = switch (meta) {
			case {params:[{expr:EConst(CString(s))}]}:s;
			case _: 'Usage of this ${s} is deprecated';
		}
		if (!Hashtbl.mem(warned_positions, p_usage)) {
			Hashtbl.replace(warned_positions, p_usage, true);
			com.warning(s, p_usage);
		}
	}

	public static function check_meta (com:context.Common.Context, meta:core.Ast.Metadata, s:String, p_usage:core.Globals.Pos) : Void {
		try {
			print_deprecation_message(com, core.Meta.get(Deprecated, meta), s, p_usage);
		}
		catch (_:ocaml.Not_found) {}
	}

	public static function check_cf (com:context.Common.Context, cf:core.Type.TClassField, p:core.Globals.Pos) : Void {
		check_meta(com, cf.cf_meta, "field", p);
	}

	public static function check_class (com:context.Common.Context, c:core.Type.TClass, p:core.Globals.Pos) : Void {
		if (!c.equals(curclass.get())) {
			check_meta(com, c.cl_meta, "class", p);
		}
	}

	public static function check_enum (com:context.Common.Context, en:core.Type.TEnum, p:core.Globals.Pos) : Void {
		check_meta(com, en.e_meta, "enum", p);
	}

	public static function check_ef (com:context.Common.Context, ef:core.Type.TEnumField, p:core.Globals.Pos) : Void {
		check_meta(com, ef.ef_meta, "enum field", p);
	}

	public static function check_typedef (com:context.Common.Context, t:core.Type.TDef, p:core.Globals.Pos) : Void {
		check_meta(com, t.t_meta, "typedef", p);
	}

	public static function check_module_type (com:context.Common.Context, mt:core.Type.ModuleType, p:core.Globals.Pos) : Void {
		switch (mt) {
			case TClassDecl(c): check_class(com, c, p);
			case TEnumDecl(en): check_enum(com, en, p);
			case _:
		}
	}

	public static function run (com:context.Common.Context) : Void {
		trace("TODO: context.display.DeprecationCheck.run");
	}
}