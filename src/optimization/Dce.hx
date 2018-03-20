package optimization;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
import core.Type.ModuleDef;
import core.Type.ModuleType;
import core.Type.TClass;
import core.Type.TClassField;
import core.Type.TEnum;
import core.Type.TExpr;

import ocaml.Hashtbl;
import ocaml.List;
import ocaml.PMap;

@:structInit
class Dce {
	public final com: context.Common.Context;
	public final full: Bool;
	public final std_dirs: ImmutableList<String>;
	public final debug : Bool;
	public final follow_expr: (Dce, TExpr) -> Void;
	public final dependent_types: Hashtbl<core.Path, ImmutableList<ModuleType>>;
	public var curclass: TClass;
	public var added_fields: ImmutableList<{c:TClass, cf:TClassField, stat:Bool}>;
	public var marked_fields: ImmutableList<TClassField>;
	public var marked_maybe_fields: ImmutableList<TClassField>;
	public var t_stack: ImmutableList<core.Type.T>;
	public var ts_stack: ImmutableList<core.Type.T>;
	public var features: Hashtbl<String, ImmutableList<{c:TClass, cf:TClassField, stat:Bool}>>;
	public function new (com, full, std_dirs, debug, follow_expr, dependent_types, curclass, added_fields, marked_fields, marked_maybe_fields, t_stack, ts_stack, features) {
		this.com = com; this.full = full;
		this.std_dirs = std_dirs; this.debug = debug;
		this.follow_expr = follow_expr; this.dependent_types = dependent_types;
		this.curclass = curclass; this.added_fields = added_fields;
		this.marked_fields = marked_fields; this.marked_maybe_fields = marked_maybe_fields;
		this.t_stack = t_stack; this.ts_stack = ts_stack;
		this.features = features;
	}

	// checking

	// check for @:keepSub metadata, which forces @:keep on child classes
	public static function super_forces_keep (c:TClass) : Bool {
		return core.Meta.has(KeepSub, c.cl_meta) || (switch (c.cl_super) {
			case Some({c:csup}): super_forces_keep(csup);
			case _: false;
		});
	}

	public static function overrides_extern_field (cf:TClassField, c:TClass) : Bool {
		function is_extern (c:TClass, cf:TClassField) : Bool { return c.cl_extern && cf.cf_expr == None; }
		function loop (c:TClass, cf:TClassField) : Bool {
			return switch (c.cl_super) {
				case None: false;
				case Some({c:c}):
					try {
						var cf = PMap.find(cf.cf_name, c.cl_fields);
						(is_extern(c, cf)) ? true : loop(c, cf);
					}
					catch (_:ocaml.Not_found) {
						false;
					}
			}
		}
		return loop(c, cf);
	}

	public static function is_std_file (dce:Dce, file:String) : Bool {
		return List.exists(StringTools.startsWith.bind(file), dce.std_dirs);
	}

	/* check if a class is kept entirely */
	public static function keep_whole_class (dce:Dce, c:TClass) : Bool {
		return core.Meta.has(Keep, c.cl_meta)
			|| !(dce.full || is_std_file(dce, c.cl_module.m_extra.m_file) || core.Type.has_meta(Dce, c.cl_meta))
			|| super_forces_keep(c)
			|| (switch (c) {
				case {cl_path:{a:[], b:("Math"|"Array")}} if (dce.com.platform == Js): false;
				case {cl_extern:true}, {cl_path:{a:["flash", "_Boot"], b:"RealBoot"}}: true;
				case {cl_path:{a:[], b:"String"}}, {cl_path:{a:[], b:"Array"}}: !(dce.com.platform == Js);
				case _: false;
			});
	}

	public static function keep_whole_enum (dce:Dce, en:TEnum) : Bool {
		return core.Meta.has(Keep, en.e_meta) ||
			!(dce.full || is_std_file(dce, en.e_module.m_extra.m_file) || core.Type.has_meta(Dce, en.e_meta));
	}

	/*
		Check if a field is kept.
		`keep_field` is checked to determine the DCE entry points, i.e. all fields that have `@:keep` or kept for other reasons.
		And then it is used at the end to check which fields can be filtered from their classes.
	*/
	public static function keep_field (dce:Dce, cf:TClassField, c:TClass, is_static:Bool) : Bool {
		return core.Meta.has(Keep, cf.cf_meta)
			|| core.Meta.has(Used, cf.cf_meta)
			|| cf.cf_name == "__init__"
			|| !(core.Type.is_physical_field(cf))
			|| (!is_static && overrides_extern_field(cf, c))
			|| ( cf.cf_name == "new"
					&& switch (c.cl_super) { // parent class kept constructor
						case Some({c:(csup={cl_constructor:Some(ctor)})}): keep_field(dce, ctor, csup, false);
						case _: false;
					});
	}

	// marking
	public static function check_feature (dce:Dce, s:String) {
		try {
			var l = Hashtbl.find(dce.features, s);
			List.iter (function (tmp) {
				var c = tmp.c; var cf = tmp.cf; var stat = tmp.stat;
				mark_field(dce, c, cf, stat);
			}, l);
			Hashtbl.remove(dce.features, s);
		}
		catch (_:ocaml.Not_found) {
		}
	}

	public static function check_and_add_feature (dce:Dce, s:String) {
		check_feature(dce, s);
		Hashtbl.replace(dce.curclass.cl_module.m_extra.m_features, s, true);
	}

	/* mark a field as kept */
	public static function mark_field (dce:Dce, c:TClass, cf:TClassField, stat:Bool) : Void {
		function add (cf:TClassField) : Void {
			if (!core.Meta.has(Used, cf.cf_meta)) {
				cf.cf_meta = ({name:Used, params:Tl, pos:cf.cf_pos} : core.Ast.MetadataEntry) :: cf.cf_meta;
				dce.added_fields = {c:c, cf:cf, stat:stat} :: dce.added_fields;
				dce.marked_fields = cf :: dce.marked_fields;
				check_feature(dce, '${core.Globals.s_type_path(c.cl_path)}.${cf.cf_name}');
			}
		}
		if (cf.cf_name == "new") {
			function loop (c:TClass) {
				switch (c.cl_constructor) {
					case Some(cf): add(cf);
					case None:
				}
				switch (c.cl_super) {
					case Some({c:csup}): loop(csup);
					case None:
				}
			}
			loop(c);
		}
		else {
			if (!PMap.mem(cf.cf_name, (stat) ? c.cl_statics : c.cl_fields)) {
				switch (c.cl_super) {
					case None: add(cf);
					case Some({c:c}): mark_field(dce, c, cf, stat);
				}
			}
			else {
				add(cf);
			}
			if (!stat) {
				switch (c.cl_constructor) {
					case None:
					case Some(ctor):
						mark_field(dce, c, ctor, false);
				}
			}
		}
	}

	public static function update_marked_class_fields (dce:Dce, c:TClass) {
		// mark all :?used fields as surely :used now
		List.iter (function (cf:TClassField) {
			if (core.Meta.has(MaybeUsed, cf.cf_meta)) {
				mark_field(dce, c, cf, true);
			}
		}, c.cl_ordered_statics);
		List.iter (function (cf:TClassField) {
			if (core.Meta.has(MaybeUsed, cf.cf_meta)) {
				mark_field(dce, c, cf, false);
			}
		}, c.cl_ordered_fields);
		// we always have to keep super classes and implemented interfaces
		switch (c.cl_init) {
			case None:
			case Some(init):
				dce.follow_expr(dce, init);
		}
		List.iter(function (tmp) { var c = tmp.c; mark_class(dce, c); }, c.cl_implements);
		switch (c.cl_super) {
			case None:
			case Some({c:csup, params:pl}):
				mark_class(dce, csup);
		}
	}

	/* mark a class as kept. If the class has fields marked as @:?keep, make sure to keep them */
	public static function mark_class (dce:Dce, c:TClass) : Void {
		if (!core.Meta.has(Used, c.cl_meta)) {
			c.cl_meta = ({name:Used, params:Tl, pos:c.cl_pos} : core.Ast.MetadataEntry) :: c.cl_meta;
			check_feature(dce, '${core.Globals.s_type_path(c.cl_path)}.*');
			update_marked_class_fields(dce, c);
		}
	}

	public static function mark_enum (dce:Dce, en:TEnum) : Void {
		trace("TODO: mark_enum");
		throw false;
	}

	public static function mark_abstract (dce:Dce, a:core.Type.TAbstract) : Void {
		if (!core.Meta.has(Used, a.a_meta)) {
			check_feature(dce, '${core.Globals.s_type_path(a.a_path)}.*');
			a.a_meta = ({name:Used, params:Tl, pos:a.a_pos} : core.Ast.MetadataEntry) :: a.a_meta;
		}
	}

	/* mark a type as kept */
	public static function mark_t (dce:Dce, p:core.Globals.Pos, t:core.Type.T) : Void {
		if (!List.exists(function (t2:core.Type.T) { return core.Type.fast_eq(t, t2); }, dce.t_stack)) {
			dce.t_stack = t :: dce.t_stack;
			switch (core.Type.follow(t)) {
				case TInst(c={cl_kind:KTypeParameter(tl)}, pl):
					if (!core.Meta.has(Used, c.cl_meta)) {
						c.cl_meta = ({name:Used, params:Tl, pos:c.cl_pos} : core.Ast.MetadataEntry) :: c.cl_meta;
						List.iter(mark_t.bind(dce, p), tl);
					}
					List.iter(mark_t.bind(dce, p), pl);
				case TInst(c, pl):
					mark_class(dce, c);
					List.iter(mark_t.bind(dce, p), pl);
				case TFun({args:args, ret:ret}):
					List.iter (function (arg) { var t = arg.t; mark_t(dce, p, t); }, args);
					mark_t(dce, p, ret);
				case TEnum(e, pl):
					mark_enum(dce, e);
					List.iter(mark_t.bind(dce, p), pl);
				case TAbstract(a, pl) if (core.Meta.has(MultiType, a.a_meta)):
					try {
						mark_t(dce, p, context.typecore.AbstractCast.find_multitype_specialization(dce.com, a, pl, p).snd);
					}
					catch (_:core.Error) {
					}
				case TAbstract(a, pl):
					mark_abstract(dce, a);
					List.iter(mark_t.bind(dce, p), pl);
					if (!core.Meta.has(CoreType, a.a_meta)) {
						mark_t(dce, p, core.Abstract.get_underlying_type(a, pl));
					}
				case TLazy(_), TDynamic(_), TType(_), TAnon(_), TMono(_):
			}
			dce.t_stack = List.tl(dce.t_stack);
		}
	}

	public static function mark_mt (dce:Dce, mt:ModuleType) : Void {
		switch (mt) {
			case TClassDecl(c):
				mark_class(dce, c);
			case TEnumDecl(e):
				mark_enum(dce, e);
			case TAbstractDecl(a):
				// abstract 'feature' is defined as the abstract type beeing used as a value, not as a type
				if (!core.Meta.has(ValueUsed, a.a_meta)) {
					a.a_meta = ({name:ValueUsed, params:Tl, pos:a.a_pos} : core.Ast.MetadataEntry) :: a.a_meta;
				}
				mark_abstract(dce, a);
			case TTypeDecl(_):
		}
	}

	/* find all dependent fields by checking implementing/subclassing types */
	public static function mark_dependent_fields (dce:Dce, csup:TClass, n:String, stat:Bool) : Void {
		function loop (c:TClass) {
			try {
				var cf = PMap.find(n, (stat) ? c.cl_statics : c.cl_fields);
				/* if it's clear that the class is kept, the field has to be kept as well. This is also true for
					extern interfaces because we cannot remove fields from them */
				if (core.Meta.has(Used, c.cl_meta) || (csup.cl_interface && csup.cl_extern)) {
					mark_field(dce, c, cf, stat);
				}
				// otherwise it might be kept if the class is kept later, so mark it as :?used
				else if (!core.Meta.has(MaybeUsed, cf.cf_meta)) {
					cf.cf_meta = ({name:MaybeUsed, params:Tl, pos:cf.cf_pos} : core.Ast.MetadataEntry) :: cf.cf_meta;
					dce.marked_maybe_fields = cf :: dce.marked_maybe_fields;
				}
			}
			catch (_:ocaml.Not_found) {
				// if the field is not present on current class, it might come from a base class
				switch (c.cl_super) {
					case None:
					case Some({c:csup}): loop(csup);
				}
			}
		}
		function loop_inheritance (c:TClass) {
			loop(c);
			List.iter(function (d) { loop_inheritance(d); }, c.cl_descendants);
		}
		loop_inheritance(csup);
	}

	// expr and field evaluation
	public static function opt (f:TExpr->Void, e:Option<TExpr>) : Void {
		switch (e) {
			case None:
			case Some(e): f(e);
		}
	}

	public static function to_string (dce:Dce, t:core.Type.T) : Void {
		switch (t) {
			case TInst(c, tl):
				field(dce, c, "toString", false);
			case TType(tt, tl):
				if (!List.exists(function (t2) { return core.Type.fast_eq(t, t2); }, dce.ts_stack)) {
					dce.ts_stack = t :: dce.ts_stack;
					to_string(dce, core.Type.apply_params(tt.t_params, tl, tt.t_type));
				}
			case TAbstract(a={a_impl:Some(c)}, tl):
				if (core.Meta.has(CoreType, a.a_meta)) {
					field(dce, c, "toString", false);
				}
				else {
					to_string(dce, core.Abstract.get_underlying_type(a, tl));
				}
			case TMono(r):
				switch (r.get()) {
					case Some(t): to_string(dce, t);
					case _:
				}
			case TLazy(f):
				to_string(dce, core.Type.lazy_type(f));
			case TDynamic(_.get()=>t):
				if (t == core.Type.t_dynamic) {
				}
				else {
					to_string(dce, t);
				}
			case TEnum(_), TFun(_), TAnon(_), TAbstract({a_impl:None},_):
				// if we to_string these it does not imply that we need all its sub-types
		}
	}

	public static function field (dce:Dce, c:TClass, n:String, stat:Bool) : Void {
		function find_field (n:String) : TClassField {
			return
			if (n == "new") {
				switch (c.cl_constructor) {
					case None: throw ocaml.Not_found.instance;
					case Some(cf): cf;
				}
			}
			else {
				PMap.find(n, (stat) ? c.cl_statics : c.cl_fields);
			}
		}
		try {
			var cf = find_field(n);
			mark_field(dce, c, cf, stat);
		}
		catch (_:ocaml.Not_found) {
			try {
				if (c.cl_interface) {
					function loop (cl:ImmutableList<{c:TClass, params:core.Type.TParams}>) {
						return
						switch (cl) {
							case []: throw ocaml.Not_found.instance;
							case {c:c} :: cl:
								try {
									field(dce, c, n, stat);
								}
								catch (_:ocaml.Not_found) {
									loop(cl);
								}
						}
					}
					loop(c.cl_implements);
				}
				else {
					switch (c.cl_super) {
						case Some({c:csup}):
							field(dce, csup, n, stat);
						case None: throw ocaml.Not_found.instance;
					}
				}
			}
			catch (_:ocaml.Not_found) {
				try {
					switch (c.cl_kind) {
						case KTypeParameter(tl):
							function loop (tl:core.Type.TParams) {
								switch (tl) {
									case []: throw ocaml.Not_found.instance;
									case TInst(c,_) :: cl:
										try {
											field(dce, c, n, stat);
										}
										catch (_:ocaml.Not_found) {
											loop(cl);
										}
									case t :: tl:
										loop(tl);
								}
							}
							loop(tl);
						case _:
							throw ocaml.Not_found.instance;
					}
				}
				catch (_:ocaml.Not_found) {
					if (dce.debug) {
						var stderr = std.Sys.stderr();
						stderr.writeString("[DCE] Field "+n+" not found on "+core.Globals.s_type_path(c.cl_path));
						stderr.flush();
						stderr.close();
					}
				}
			}
		}
	}

	public static function mark_directly_used_class (dce:Dce, c:TClass) : Void {
		if (c != dce.curclass && !core.Meta.has(DirectlyUsed, c.cl_meta)) {
			c.cl_meta = ({name:DirectlyUsed, params:Tl, pos:c.cl_pos} : core.Ast.MetadataEntry) :: c.cl_meta;
		}
	}

	public static function mark_directly_used_enum (e:TEnum) : Void {
		if (!core.Meta.has(DirectlyUsed, e.e_meta)) {
			e.e_meta = ({name:DirectlyUsed, params:Tl, pos:e.e_pos} : core.Ast.MetadataEntry) :: e.e_meta;
		}
	}

	public static function mark_directly_used_mt (dce:Dce, mt:ModuleType) : Void {
		switch (mt) {
			case TClassDecl(c):
				mark_directly_used_class(dce, c);
			case TEnumDecl(e):
				mark_directly_used_enum(e);
			case _:
		}
	}

	public static function mark_directly_used_t (dce:Dce, p:core.Globals.Pos, t:core.Type.T) : Void {
		trace("TODO: mark_directly_used_t");
		throw false;
	}

	public static function check_dynamic_write (dce:Dce, fa:core.Type.TFieldAccess) : Void {
		var n = core.Type.field_name(fa);
		check_and_add_feature(dce, "dynamic_write");
		check_and_add_feature(dce, "dynamic_write."+n);
	}

	public static function check_anon_optional_write (dce:Dce, fa:core.Type.TFieldAccess) : Void {
		var n = core.Type.field_name(fa);
		check_and_add_feature(dce, "anon_optional_write");
		check_and_add_feature(dce, "anon_optional_write."+n);
	}

	public static function check_anon_write (dce:Dce, fa:core.Type.TFieldAccess) : Void {
		var n = core.Type.field_name(fa);
		check_and_add_feature(dce, "anon_write");
		check_and_add_feature(dce, "anon_write."+n);
	}

	public static function is_array (t:core.Type.T) : Bool {
		return switch (core.Type.follow(t)) {
			case TAbstract(a, tl) if (!core.Meta.has(CoreType, a.a_meta)):
				is_array(core.Abstract.get_underlying_type(a, tl));
			case TInst({cl_path:{a:[], b:"Array"}}, _): true;
			case _: false;
		}
	}

	public static function is_dynamic (t:core.Type.T) : Bool {
		return switch (core.Type.follow(t)) {
			case TAbstract(a, tl) if (!core.Meta.has(CoreType, a.a_meta)):
				is_dynamic(core.Abstract.get_underlying_type(a,tl));
			case TDynamic(_): true;
			case _: false;
		}
	}

	public static function is_string (t:core.Type.T) : Bool {
		return switch (core.Type.follow(t)) {
			case TAbstract(a, tl) if (!core.Meta.has(CoreType, a.a_meta)):
				is_string(core.Abstract.get_underlying_type(a, tl));
			case TInst({cl_path:{a:[], b:"String"}}, _): true;
			case _: false;
		}
	}

	public static function is_const_string (e:TExpr) : Bool {
		return switch (e.eexpr) {
			case TConst(TString(_)): true;
			case _: false;
		}
	}

	public static function expr_field (dce:Dce, e:TExpr, fa:core.Type.TFieldAccess, is_call_expr:Bool) : Void {
		function do_default () {
			var n = core.Type.field_name(fa);
			switch (fa) {
				case FAnon(cf):
					if (core.Meta.has(Optional, cf.cf_meta)) {
						check_and_add_feature(dce, "anon_optional_read");
						check_and_add_feature(dce, "anon_optional_read."+n);
					}
					else {
						check_and_add_feature(dce, "anon_read");
						check_and_add_feature(dce, "anon_read."+n);
					}
				case FDynamic(_):
					check_and_add_feature(dce, "dynamic_read");
					check_and_add_feature(dce, "dynamic_read."+n);
				case _:
			}
			switch (core.Type.follow(e.etype)) {
				case TInst(c, _):
					mark_class(dce, c);
					field(dce, c, n, false);
				case TAnon(a):
					switch (a.a_status.get()) {
						case Statics(c):
							mark_class(dce, c);
							field(dce, c, n, true);
						case _:
					}
				case _:
			}
		}

		function mark_instance_field_access (c:TClass, cf:TClassField) : Void {
			if (!is_call_expr && dce.com.platform == Python) {
				if (c.cl_path.a == Tl && c.cl_path.b == "Array") {
					check_and_add_feature(dce, "closure_Array");
					check_and_add_feature(dce, "python.internal.ArrayImpl."+cf.cf_name);
					check_and_add_feature(dce, "python.internal.ArrayImpl");
				}
				else if (c.cl_path.a == Tl && c.cl_path.b == "String") {
					check_and_add_feature(dce, "closure_String");
					check_and_add_feature(dce, "python.internal.StringImpl."+cf.cf_name);
					check_and_add_feature(dce, "python.internal.StringImpl");
				}
			}
		}
		switch (fa) {
			case FStatic(c, cf):
				mark_class(dce, c);
				mark_field(dce, c, cf, true);
			case FInstance(c, _, cf):
				// mark_instance_field_access(c, cf);
				mark_class(dce, c);
				mark_field(dce, c, cf, false);
			case FClosure(Some({c:c}), cf):
				mark_instance_field_access(c, cf);
				do_default();
			case FClosure(_):
				do_default();
			case _:
				do_default();
		}
		expr(dce, e);
	}

	public static function expr (dce:Dce, e:TExpr) : Void {
		mark_t(dce, e.epos, e.etype);
		switch (e.eexpr) {
			case TNew(c, pl, el):
				mark_class(dce, c);
				mark_directly_used_class(dce, c);
				field(dce, c, "new", false);
				List.iter(expr.bind(dce), el);
				List.iter(mark_t.bind(dce, e.epos), pl);
			case TVar(v, e1):
				opt(expr.bind(dce), e1);
				mark_t(dce, e.epos, v.v_type);
			case TCast(e, Some(mt)):
				check_feature(dce, "typed_cast");
				mark_mt(dce, mt);
				mark_directly_used_mt(dce, mt);
				expr(dce, e);
			case TObjectDecl(vl):
				check_and_add_feature(dce, "has_anon");
				List.iter(function (tmp) { var e = tmp.expr; expr(dce, e); }, vl);
			case TTypeExpr (mt):
				mark_mt(dce, mt);
				mark_directly_used_mt(dce, mt);
			case TTry(e, vl):
				expr(dce, e);
				List.iter(function (tmp) {
					var v = tmp.v; var e = tmp.e;
					if (v.v_type != core.Type.t_dynamic) {
						check_feature(dce, "typed_catch");
						mark_directly_used_t(dce, v.v_pos, v.v_type);
					}
					expr(dce, e);
					mark_t(dce, e.epos, v.v_type);
				}, vl);
			case TCall({eexpr:TIdent("`trace")}, [p, {eexpr:TObjectDecl(v)}]):
				check_and_add_feature(dce, "has_anon_trace");
				List.iter(function (tmp) { var e = tmp.expr; expr(dce, e); }, v);
				expr(dce, p);
			case TCall({eexpr:TIdent("__define_feature__")}, [{eexpr:TConst(TString(ft))}, e]):
				Hashtbl.replace(dce.curclass.cl_module.m_extra.m_features, ft, true);
				check_feature(dce, ft);
				expr(dce, e);
			// keep toString method when the class is argument to Std.string or haxe.Log.trace
			case TCall(ef={eexpr:TField({eexpr:TTypeExpr(TClassDecl(c={cl_path:{a:["haxe"], b:"Log"}}))}, FStatic(_, {cf_name:"trace"}))}, (args=(e2::el))),
					TCall(ef={eexpr:TField({eexpr:TTypeExpr(TClassDecl(c={cl_path:{a:[], b:"Std"}}))}, FStatic(_, {cf_name:"string"}))}, (args=(e2::el))):
				mark_class(dce, c);
				to_string(dce, e2.etype);
				switch (el) {
					case [{eexpr:TObjectDecl(fl)}]:
						try {
							switch (core.ast.Expr.field_assoc("customParams", fl)) {
								case {eexpr:TArrayDecl(el)}:
									List.iter(function (e) { to_string(dce, e.etype); }, el);
								case _:
							}
						}
						catch (_:ocaml.Not_found) {

						}
					case _:
				}
				expr(dce, ef);
				List.iter(expr.bind(dce), args);
			case TCall(e={eexpr:TConst(TSuper)}, el):
				mark_t(dce, e.epos, e.etype);
				List.iter(expr.bind(dce), el);
			case TUnop((OpIncrement|OpDecrement),_, e1={eexpr:TArray(_)}):
				check_and_add_feature(dce, "array_write");
				check_and_add_feature(dce, "array_read");
				expr(dce, e1);
			case TBinop((OpAdd|OpAssignOp(OpAdd)), e1, e2) if (is_dynamic(e1.etype) || is_dynamic(e2.etype)):
				check_and_add_feature(dce, "add_dynamic");
				expr(dce, e1);
				expr(dce, e2);
			case TBinop(op=(OpAdd|OpAssignOp(OpAdd)), e1, e2) if ((is_string(e1.etype) || is_string(e2.etype)) && !(is_const_string(e1) || is_const_string(e2))):
				// add array_write if we're doing += to an array element, since this pattern comes before the array_write one
				switch [op, e1] {
					case [OpAssignOp(_), {eexpr:TArray({etype:t},_)}] if (is_array(t)):
						check_and_add_feature(dce, "array_write");
					case _:
				}
				check_and_add_feature(dce, "unsafe_string_concat");
				expr(dce, e1);
				expr(dce, e2);
			case TArray(e1={etype:TDynamic(_.get()=>t)}, e2) if (t == core.Type.t_dynamic):
				check_and_add_feature(dce, "dynamic_array_read");
				expr(dce, e1);
				expr(dce, e2);
			case TBinop((OpAssign|OpAssignOp(_)),e1={eexpr:TArray({etype:TDynamic(_.get()=>t)}, _)}, e2) if (t == core.Type.t_dynamic):
				check_and_add_feature(dce, "dynamic_array_write");
				expr(dce, e1);
				expr(dce, e2);
			case TArray(e1={etype:t}, e2) if (is_array(t)):
				check_and_add_feature(dce, "array_read");
				expr(dce, e1);
				expr(dce, e2);
			case TBinop((OpAssign|OpAssignOp(_)),e1={eexpr:TArray({etype:t}, _)}, e2) if (is_array(t)):
				check_and_add_feature(dce, "array_write");
				expr(dce, e1);
				expr(dce, e2);
			case TBinop(OpAssign, e1={eexpr:TField(_, fa=FDynamic(_))}, e2):
				check_dynamic_write(dce, fa);
				expr(dce, e1);
				expr(dce, e2);
			case TBinop(OpAssign, e1={eexpr:TField(_, fa=FAnon(cf))}, e2):
				if (core.Meta.has(Optional, cf.cf_meta)) {
					check_anon_optional_write(dce, fa);
				}
				else {
					check_anon_write(dce, fa);
				}
				expr(dce, e1);
				expr(dce, e2);
			case TBinop(OpAssignOp(op), e1={eexpr:TField(_, fa=FDynamic(_))}, e2):
				check_dynamic_write(dce, fa);
				expr(dce, e1);
				expr(dce, e2);
			case TBinop(OpAssignOp(op), e1={eexpr:TField(_, fa=FAnon(cf))}, e2):
				if (core.Meta.has(Optional, cf.cf_meta)) {
					check_anon_optional_write(dce, fa);
				}
				else {
					check_anon_write(dce, fa);
				}
				expr(dce, e1);
				expr(dce, e2);
			case TBinop(OpEq, e1={etype:t1}, e2={etype:t2}) if (is_dynamic(t1) || is_dynamic(t2)):
				check_and_add_feature(dce, "dynamic_binop_==");
				expr(dce, e1);
				expr(dce, e2);
			case TBinop(OpNotEq, e1={etype:t1}, e2={etype:t2}) if (is_dynamic(t1) || is_dynamic(t2)):
				check_and_add_feature(dce, "dynamic_binop_!=");
				expr(dce, e1);
				expr(dce, e2);
			case TBinop(OpMod, e1, e2):
				check_and_add_feature(dce, "binop_%");
				expr(dce, e1);
				expr(dce, e2);
			case TBinop((OpUShr|OpAssignOp(OpUShr)), e1, e2):
				check_and_add_feature(dce, "binop_>>>");
				expr(dce, e1);
				expr(dce, e2);
			case TCall(e2={eexpr:TField(ef, fa)}, el):
				mark_t(dce, e2.epos, e2.etype);
				expr_field(dce, ef, fa, true);
				List.iter(expr.bind(dce), el);
			case TField(e, fa):
				expr_field(dce, e, fa, false);
			case TThrow(e):
				check_and_add_feature(dce, "has_throw");
				expr(dce, e);
				function loop(e:TExpr) {
					to_string(dce, e.etype);
					core.Type.iter(loop, e);
				}
				loop(e);
			case _:
				core.Type.iter(expr.bind(dce), e);
		}
	}

	public static function fix_accessors (com:context.Common.Context) {
		List.iter(function (mt:ModuleType) {
			switch (mt) {
				// filter empty abstract implementation classes (issue #1885).
				case TClassDecl(c={cl_kind:KAbstractImpl(_)}) if (c.cl_ordered_statics == Tl && c.cl_ordered_fields == Tl && !core.Meta.has(Used, c.cl_meta)):
					c.cl_extern = true;
				case TClassDecl(c={cl_kind:KAbstractImpl(a)}) if (core.Meta.has(Enum, a.a_meta)):
					function is_runtime_field(cf:TClassField) : Bool {
						return !core.Meta.has(Enum, cf.cf_meta);
					}
					// also filter abstract implementation classes that have only @:enum fields (issue #2858)
					if (!List.exists(is_runtime_field,c.cl_ordered_statics)) {
						c.cl_extern = true;
					}
				case TClassDecl(c):
					function has_accessor (c:TClass, n:String, stat:Bool) {
						return PMap.mem(n, (stat) ? c.cl_statics : c.cl_fields) || (
							switch (c.cl_super) {
								case Some({c:csup}): has_accessor(csup, n, stat);
								case None: false;
							}
						);
					}
					function check_prop (stat:Bool, cf:TClassField) {
						switch (cf.cf_kind) {
							case Var({v_read:AccCall, v_write:a}):
								var s = "get_" + cf.cf_name;
								cf.cf_kind = Var({v_read:(has_accessor(c, s, stat)) ? AccCall : AccNever, v_write:a});
							case _:
						}
						switch (cf.cf_kind) {
							case Var({v_write:AccCall, v_read:a}):
								var s = "set_" + cf.cf_name;
								cf.cf_kind = Var({v_write:(has_accessor(c, s, stat)) ? AccCall : AccNever, v_read:a});
							case _:
						}
					}
					List.iter(check_prop.bind(true), c.cl_ordered_statics);
					List.iter(check_prop.bind(false), c.cl_ordered_fields);
				case _:
			}
		}, com.types);
	}
	public static function run (com:context.Common.Context, main:Option<TExpr>, full:Bool) {
		var dce:Dce = {
			com:com, full:full, dependent_types:Hashtbl.create(0),
			std_dirs: (full) ? [] : List.map(core.Path.unique_full_path, com.std_path),
			debug: context.Common.defined(com, DceDebug),
			added_fields: [], follow_expr: expr,
			marked_fields: [], marked_maybe_fields: [],
			t_stack: [], ts_stack: [], features: Hashtbl.create(0), curclass: core.Type.null_class
		};
		switch (main) {
			case Some({eexpr:TCall({eexpr:TField(e, FStatic(c, cf))}, _)}),
					Some({eexpr:TBlock({eexpr:TCall({eexpr:TField(e,  FStatic(c, cf))}, _)}::_)}):
				cf.cf_meta = ({name:Keep, params:Tl, pos:cf.cf_pos} : core.Ast.MetadataEntry) :: cf.cf_meta;
			case _:
		}
		List.iter( function (m:ModuleDef) {
			List.iter ( function (tmp) {
				var s = tmp.s; var v = tmp.v;
				if (Hashtbl.mem(dce.features, s)) {
					Hashtbl.replace(dce.features, s, v :: Hashtbl.find(dce.features, s));
				}
				else {
					Hashtbl.add(dce.features, s, [v]);
				}
			}, m.m_extra.m_if_feature);
		}, com.modules);
		// first step: get all entry points, which is the main method and all class methods which are marked with @:keep
		List.iter( function (t:ModuleType) {
			switch (t) {
				case TClassDecl(c):
					var keep_class = keep_whole_class(dce, c) && (!c.cl_extern || c.cl_interface);
					function loop (stat:Bool, cf:TClassField) {
						if (keep_class || keep_field(dce, cf, c, stat)) {
							mark_field(dce, c, cf, stat);
						}
					}
					List.iter(loop.bind(true), c.cl_ordered_statics);
					List.iter(loop.bind(false), c.cl_ordered_fields);
					switch (c.cl_constructor) {
						case Some(cf): loop(false, cf);
						case None:
					}
					switch (c.cl_init) {
						case Some(e) if (keep_class || core.Meta.has(KeepInit, c.cl_meta)):
							// create a fake field to deal with our internal logic (issue #3286)
							var cf = core.Type.mk_field("__init__", e.etype, e.epos, core.Globals.null_pos);
							cf.cf_expr = Some(e);
							loop(true, cf);
						case _:
					}
				case TEnumDecl(en) if (keep_whole_enum(dce, en)):
					mark_enum(dce, en);
				case _:
			}
		}, com.types);
		if (dce.debug) {
			List.iter(function (tmp) {
				var c = tmp.c; var cf = tmp.cf;
				switch (cf.cf_expr) {
					case None:
					case Some(_):
						std.Sys.println("[DCE] Entry point: "+core.Globals.s_type_path(c.cl_path) + "." + cf.cf_name);
				}
			}, dce.added_fields);
		}
		// second step: initiate DCE passes and keep going until no new fields were added
		function loop1 () : Void {
			switch (dce.added_fields) {
				case []:
				case cfl:
					dce.added_fields = Tl;
					// extend to dependent (= overriding/implementing) class fields
					List.iter( function (tmp) {
						var c = tmp.c; var cf = tmp.cf; var stat = tmp.stat;
						mark_dependent_fields(dce, c, cf.cf_name, stat);
					}, cfl);
					// mark fields as used
					List.iter( function(tmp) {
						var c = tmp.c; var cf = tmp.cf; var stat = tmp.stat;
						if (core.Type.is_physical_field(cf)) {
							mark_class(dce, c);
						}
						mark_field(dce, c, cf, stat);
						mark_t(dce, cf.cf_pos, cf.cf_type);
					}, cfl);
					// follow expressions to new types/fields
					List.iter (function (tmp) {
						var c = tmp.c; var cf = tmp.cf;
						dce.curclass = c;
						opt(expr.bind(dce), cf.cf_expr);
						List.iter( function (cf) { if (cf.cf_expr != None) { opt(expr.bind(dce), cf.cf_expr); } }, cf.cf_overloads);
						dce.curclass = core.Type.null_class;
					}, cfl);
					loop1();
			}
		}
		loop1();
		// third step: filter types
		function loop2 (acc:ImmutableList<ModuleType>, types:ImmutableList<ModuleType>) : ImmutableList<ModuleType> {
			return switch (types) {
				case (mt=TClassDecl(c)) :: l if (keep_whole_class(dce, c)):
					loop2(mt::acc, l);
				case (mt=TClassDecl(c)) :: l:
					function check_property (cf:TClassField, stat:Bool) {
						function add_accessor_metadata (cf:TClassField) {
							if (!core.Meta.has(Accessor, cf.cf_meta)) {
								cf.cf_meta = ({name:Accessor, params:Tl, pos:c.cl_pos} : core.Ast.MetadataEntry) :: cf.cf_meta;
							}
						}
						switch (cf.cf_kind) {
							case Var({v_read:AccCall}):
								try {
									add_accessor_metadata(PMap.find("get_"+cf.cf_meta, (stat) ? c.cl_statics : c.cl_fields));
								}
								catch (_:ocaml.Not_found) {}
							case _:
						}
						switch (cf.cf_kind) {
							case Var({v_write:AccCall}):
								try {
									add_accessor_metadata(PMap.find("set_"+cf.cf_meta, (stat) ? c.cl_statics : c.cl_fields));
								}
								catch (_:ocaml.Not_found) {}
							case _:
						}
					}
					// add :keep so subsequent filter calls do not process class fields again
					c.cl_meta= ({name:Keep, params:Tl, pos:c.cl_pos} : core.Ast.MetadataEntry) :: c.cl_meta;
					c.cl_ordered_statics = List.filter( function (cf:TClassField) {
						var b = keep_field(dce, cf, c, true);
						if (!b) {
							if (dce.debug) { std.Sys.println("[DCE] Removed field "+core.Globals.s_type_path(c.cl_path)+"."+cf.cf_name); }
							check_property(cf, true);
							c.cl_statics = PMap.remove(cf.cf_name, c.cl_statics);
						}
						return b;
					}, c.cl_ordered_statics);
					c.cl_ordered_fields = List.filter( function (cf:TClassField) {
						var b = keep_field(dce, cf, c, false);
						if (!b) {
							if (dce.debug) { std.Sys.println("[DCE] Removed field "+core.Globals.s_type_path(c.cl_path)+"."+cf.cf_name); }
							check_property(cf, false);
							c.cl_statics = PMap.remove(cf.cf_name, c.cl_fields);
						}
						return b;
					}, c.cl_ordered_fields);
					switch (c.cl_constructor) {
						case Some(cf) if (!keep_field(dce, cf, c, false)):
							c.cl_constructor = None;
						case _:
					}
					function inef (cf:TClassField) { return core.Type.is_physical_field(cf); }
					var has_non_extern_fields = List.exists(inef, c.cl_ordered_fields) || List.exists(inef, c.cl_ordered_statics);
					// we keep a class if it was used or has a used field
					if (core.Meta.has(Used, c.cl_meta) || has_non_extern_fields) {
						loop2(mt::acc, l);
					}
					else {
						switch (c.cl_init) {
							case Some(f) if (core.Meta.has(KeepInit, c.cl_meta)):
								// it means that we only need the __init__ block
								c.cl_extern = true;
								loop2(mt::acc, l);
							case _:
								if (dce.debug) { std.Sys.println("[DCE] Removed class "+core.Globals.s_type_path(c.cl_path)); }
								loop2(acc, l);
						}
					}
				case (mt=TEnumDecl(en)) :: l if (core.Meta.has(Used, en.e_meta) || en.e_extern || keep_whole_enum(dce, en)):
					loop2(mt::acc, l);
				case TEnumDecl(e) :: l:
					if (dce.debug) { std.Sys.println("[DCE] Removed enum "+core.Globals.s_type_path(e.e_path)); }
					loop2(acc, l);
				case mt :: l:
					loop2(mt::acc, l);
				case []:
					acc;
			}
		}
		com.types = loop2([], List.rev(com.types));

		// extra step to adjust properties that had accessors removed (required for Php and Cpp)
		fix_accessors(com);

		// remove "override" from fields that do not override anything anymore
		List.iter(function (mt) {
			switch (mt) {
				case TClassDecl(c):
					c.cl_overrides = List.filter(function (s:TClassField) {
						function loop (c:TClass) {
							return switch (c.cl_super) {
								case Some({c:csup}) if (PMap.mem(s.cf_name, csup.cl_fields)): true;
								case Some({c:csup}): loop(csup);
								case None: false;
							}
						}
						return loop(c);
					}, c.cl_overrides);
				case _:
			}
		}, com.types);

		// Mark extern classes as really used if they are extended by non-extern ones.
		List.iter (function (mt:ModuleType) {
			switch (mt) {
				case TClassDecl({cl_extern:false, cl_super:Some({c:(csup={cl_extern:true})})}):
					mark_directly_used_class(dce, csup);
				case TClassDecl(c={cl_extern:false}) if (c.cl_implements != Tl):
					List.iter( function (tmp) {
						var iface = tmp.c;
						if (iface.cl_extern) {
							mark_directly_used_class(dce, iface);
						}
					}, c.cl_implements);
				case _:
			}
		}, com.types);

		// cleanup added fields metadata - compatibility with compilation server
		function remove_meta (m:core.Meta.StrictMeta, l:core.Ast.Metadata) : core.Ast.Metadata {
			return switch (l) {
				case []: [];
				case ({name:m2}) :: l if (m.equals(m2)):
					l;
				case x :: l:
					x :: remove_meta(m, l);
			}
		}
		List.iter(function (cf:TClassField) { cf.cf_meta = remove_meta(Used, cf.cf_meta); }, dce.marked_fields);
		List.iter(function (cf:TClassField) { cf.cf_meta = remove_meta(MaybeUsed, cf.cf_meta); }, dce.marked_maybe_fields);
	}
}