package core;

import haxe.ds.Option;
import ocaml.PMap;

using equals.Equal;
using ocaml.Cloner;
using ocaml.Ref;

class Unify_error {
	public var l:Array<UnifyError>;
	public function new (l:Array<UnifyError>) {
		this.l = l;
	}
}

enum FieldKind {
	Var(vk:VarKind);
	Method(mk:MethodKind);
}
typedef VarKind = {
	v_read : VarAccess,
	v_write : VarAccess
}

enum VarAccess {
	AccNormal;
	AccNo; // can't be accessed outside of the class itself and its subclasses
	AccNever; // can't be accessed, even in subclasses
	AccCtor; // can only be accessed from the constructor
	AccResolve; // call resolve("field") when accessed
	AccCall; // perform a method call when accessed
	AccInline; // similar to Normal but inline when accessed
	// ocaml: string * string option
	AccRequire (s:String, t:Option<String>); // set when @:require(cond) fails
}

enum MethodKind {
	MethNormal;
	MethInline;
	MethDynamic;
	MethMacro;
}

enum ModuleCheckPolicy {
	NoCheckFileTimeModification;
	CheckFileContentModification;
	NoCheckDependencies;
	NoCheckShadowing;
}

enum T {
	TMono (t:ocaml.Ref<Option<T>>);
	TEnum (t:TEnum, params:Array<T>);
	TInst (t:TClass, params:Array<T>);
	TType (t:TDef, params:Array<T>);
	TFun (t:TSignature);
	TAnon (t:TAnon);
	// ocaml TDynamic(t:T);
	TDynamic(t:Ref<T>);
	TLazy (t:ocaml.Ref<TLazy>);
	TAbstract (t:TAbstract, params:Array<T>);
}

enum TLazy {
	LAvailable (t:T);
	LProcessing (f:Void->T);
	LWait (f:Void->T);
}


typedef TSignatureArg = {
	name:String,
	opt:Bool,
	t:T
}
typedef TSignature = {
	args : Array<TSignatureArg>,
	ret:T
}

typedef TParams = Array<T>;

typedef TypeParams = Array<{
	name : String, 
	t : T
}>;

enum TConstant {
	TInt (i:Int);
	TFloat (s:String);
	TString (s:String);
	TBool (b:Bool);
	TNull;
	TThis;
	TSuper;
}

//ocaml type: tvar_extra = (type_params * texpr option) option
typedef TVarExtra = Option<{
	params:TypeParams,
	expr:Option<TExpr>
}>;

typedef TVar = {
	v_id : Int,
	v_name : String,
	v_type : T,
	v_capture : Bool,
	v_extra : TVarExtra,
	v_meta : Ast.Metadata,
	v_pos : Globals.Pos,
}

typedef TFunc = {
	tf_args : Array<{v:TVar, c:Option<TConstant>}>,
	tf_type : T,
	tf_expr : TExpr,
}

enum AnonStatus {
	Closed;
	Opened;
	Const;
	Extend (l:Array<T>);
	Statics (t:TClass);
	EnumStatics (t:TEnum);
	AbstractStatics (t:TAbstract);
}

typedef TAnon = {
	a_fields : Map<String, TClassField>,
	a_status : ocaml.Ref<AnonStatus>
}

typedef TObjectField = {
	a:{
		name:String,
		pos:Globals.Pos,
		quotes:Ast.QuoteStatus
	},
	expr:TExpr
}
enum TExprExpr {
	TConst (c:TConstant);
	TLocal (v:TVar);
	TArray (e1:TExpr, e2:TExpr);
	TBinop (op:Ast.Binop, e1:TExpr, e2:TExpr);
	TField (e:TExpr, field:TFieldAccess);
	TTypeExpr (m:ModuleType);
	TParenthesis (e:TExpr);
	// ocaml type: of ((string * pos * quote_status) * texpr) list
	TObjectDecl (fields:Array<TObjectField>);
	TArrayDecl (values:Array<TExpr>);
	TCall (e:TExpr, params:Array<TExpr>);
	TNew (c:TClass, params:TParams, exprs:Array<TExpr>);
	TUnop (op:Ast.Unop, flag:Ast.UnopFlag, expr:TExpr);
	TFunction (f:TFunc);
	TVar (v:TVar, expr:Option<TExpr>);
	TBlock (exprs:Array<TExpr>);
	TFor (v:TVar, e1:TExpr, e2:TExpr);
	TIf (cond:TExpr, exprIf:TExpr, exprElse:Option<TExpr>);
	TWhile (econd:TExpr, e:TExpr, flag:Ast.WhileFlag);
	TSwitch (e:TExpr, cases:Array<{values:Array<TExpr>, e:TExpr}>, edef:Option<TExpr>);//of texpr * (texpr list * texpr) list * texpr option
	TTry (e:TExpr, catches:Array<{v:TVar, e:TExpr}>);
	TReturn (e:Option<TExpr>);
	TBreak;
	TContinue;
	TThrow (e:TExpr);
	TCast (e:TExpr, t:Option<ModuleType>);
	TMeta (s:Ast.MetadataEntry, e:TExpr);
	TEnumParameter (e:TExpr, f:TEnumField, o:Int);
	TEnumIndex (e:TExpr);
	TIdent (s:String);
}

enum TFieldAccess {
	FInstance (c:TClass, params:TParams, cf:TClassField);
	FStatic (c:TClass, cf:TClassField);
	FAnon (cf:TClassField);
	FDynamic (s:String);
	FClosure (c:Option<{c:TClass, params:TParams}>, cf:TClassField); // None class = TAnon
	FEnum (e:TEnum, ef:TEnumField);
}

typedef TExpr = {
	eexpr : TExprExpr,
	etype : T,
	epos : Globals.Pos
}

typedef TClassField = {
	cf_name : String,
	cf_type : T,
	cf_public : Bool,
	cf_pos : Globals.Pos,
	cf_name_pos : Globals.Pos,
	cf_doc : Ast.Documentation,
	cf_meta : Ast.Metadata,
	cf_kind : FieldKind,
	cf_params : TypeParams,
	cf_expr : Option<TExpr>,
	cf_expr_unoptimized : Option<TFunc>,
	cf_overloads : Array<TClassField>
}

enum TClassKind {
	KNormal;
	KTypeParameter (constraints:Array<T>);
	KExpr (expr:Ast.Expr);
	KGeneric;
	KGenericInstance (c:TClass, params:TParams);
	KMacroType;
	KGenericBuild (cf:Array<Ast.ClassField>);
	KAbstractImpl (ta:TAbstract);
}

typedef TInfos = {
	mt_path : Path,
	mt_module : ModuleDef,
	mt_pos : Globals.Pos,
	mt_name_pos : Globals.Pos,
	mt_private : Bool,
	mt_doc : Ast.Documentation,
	mt_meta : Ast.Metadata,
	mt_params : TypeParams
}

typedef TClass = {
	cl_path : Path,
	cl_module : ModuleDef,
	cl_pos : Globals.Pos,
	cl_name_pos : Globals.Pos,
	cl_private : Bool,
	cl_doc : Ast.Documentation,
	cl_meta : Ast.Metadata,
	cl_params : TypeParams,
	// do not insert any fields above
	cl_kind : TClassKind,
	cl_extern : Bool,
	cl_interface : Bool,
	cl_super : Option<{c:TClass, params:TParams}>,//(tclass * tparams) option;
	cl_implements : Array<{c:TClass, params:TParams}>,//(tclass * tparams) list;
	cl_fields : Map<String, TClassField>,
	cl_statics : Map<String, TClassField>,
	cl_ordered_statics : Array<TClassField>,
	cl_ordered_fields : Array<TClassField>,
	cl_dynamic : Option<T>,
	cl_array_access : Option<T>,
	cl_constructor : Option<TClassField>,
	cl_init : Option<TExpr>,
	cl_overrides : Array<TClassField>,

	cl_build : Void -> BuildState,
	cl_restore : Void -> Void,
	/*
		These are classes which directly extend or directly implement this class.
		Populated automatically in post-processing step (Filters.run)
	*/
	cl_descendants : Array<TClass>
}

typedef TEnumField = {
	ef_name : String,
	ef_type : T,
	ef_pos : Globals.Pos,
	ef_name_pos : Globals.Pos,
	ef_doc : Ast.Documentation,
	ef_index : Int,
	ef_params : TypeParams,
	ef_meta : Ast.Metadata
}

typedef TEnum = {
	e_path : Path,
	e_module : ModuleDef,
	e_pos : Globals.Pos,
	e_name_pos : Globals.Pos,
	e_private : Bool,
	e_doc : Ast.Documentation,
	e_meta : Ast.Metadata,
	e_params : TypeParams,
	// do not insert any fields above
	e_type : TDef,
	e_extern : Bool,
	e_constrs : Map<String, TEnumField>,
	e_names : Array<String>
}

typedef TDef = {
	t_path : Path,
	t_module : ModuleDef,
	t_pos : Globals.Pos,
	t_name_pos : Globals.Pos,
	t_private : Bool,
	t_doc : Ast.Documentation,
	t_meta : Ast.Metadata,
	t_params : TypeParams,
	// do not insert any fields above
	t_type : T
}

typedef TAbstract = {
	a_path : Path,
	a_module : ModuleDef,
	a_pos : Globals.Pos,
	a_name_pos : Globals.Pos,
	a_private : Bool,
	a_doc : Ast.Documentation,
	a_meta : Ast.Metadata,
	a_params : TypeParams,
	// do not insert any fields above
	a_ops : Array<{op:Ast.Binop, cf:TClassField}>,
	a_unops : Array<{op:Ast.Unop, flag:Ast.UnopFlag, cf:TClassField}>,
	a_impl : Option<TClass>,
	a_this : T,
	a_from : Array<T>,
	a_from_field : Array<{t:T, cf:TClassField}>,
	a_to : Array<T>,
	a_to_field : Array<{t:T, cf:TClassField}>,
	a_array : Array<TClassField>,
	a_resolve : Option<TClassField>
}

enum ModuleType {
	TClassDecl (c:TClass);
	TEnumDecl (e:TEnum);
	TTypeDecl (t:TDef);
	TAbstractDecl (a:TAbstract);
}

typedef ModuleDef = {
	m_id : Int,
	m_path : Path,
	m_types : Array<ModuleType>,
	m_extra : ModuleDefExtra
}

typedef ModuleDefExtra = {
	m_file : String,
	m_sign : String,
	m_check_policy : Array<ModuleCheckPolicy>,
	m_time : Float,
	m_dirty : Option<ModuleDef>,
	m_added : Int,
	m_mark : Int,
	m_deps : Map<Int, ModuleDef>,
	m_processed : Int,
	m_kind : ModuleKind,
	m_binded_res : Map<String,  String>,
	m_reuse_macro_calls : Array<String>,
	m_if_feature : Array<{
		s:String,
		c:TClass,
		cf:TClassField,
		b:Bool
	}>,
	m_features : Map<String, Bool>
}

enum ModuleKind {
	MCode;
	MMacro;
	MFake;
	MExtern;
	MImport;
}

enum BuildState {
	Built;
	Building (l:Array<TClass>);
	BuildMacro (l:Ref<Array<Void->Void>>);
}

typedef BasicTypes = {
	tvoid : T,
	tint : T,
	tfloat : T,
	tbool : T,
	tnull : T -> T,
	tstring : T,
	tarray : T -> T
}

enum UnifyError {
	Cannot_unify (t1:T, t2:T);
	Invalid_field_type (s:String);
	Has_no_field (t:T, s:String);
	Has_no_runtime_field (t:T, s:String);
	Has_extra_field (t:T, s:String);
	Invalid_kind (s:String, fk1:FieldKind, fk2:FieldKind);
	Invalid_visibility (s:String);
	Not_matching_optional (s:String);
	Cant_force_optional;
	Invariant_parameter (t1:T, t2:T);
	Constraint_failure (s:String);
	Missing_overload (tcf:TClassField, t:T);
	Unify_custom (s:String);
}

enum Eq_kind {
	EqStrict;
	EqCoreType;
	EqRightDynamic;
	EqBothDynamic;
	EqDoNotFollowNull; // like EqStrict, but does not follow Null<T>
}


class Type {
	static var mid:Int = 0;
	static var uid:Int = 0;

	public static function equalsModuleDef (a:ModuleDef, b:ModuleDef) : Bool {
		if (a == null || b == null) { return a ==b; }
		if ((a.m_id != b.m_id) || (a.m_path != b.m_path)) { return false; }
		return true;
	}

	public static function alloc_var (n:String, t:core.Type.T, p:core.Globals.Pos) : core.Type.TVar {
		uid++;
		return {
			v_name: n,
			v_type: t,
			v_id: uid,
			v_capture: false,
			v_extra: None,
			v_meta: [],
			v_pos: p 
		};
	}
	public static function alloc_mid () : Int {
		mid++;
		return mid;
	}

	public static function mk (e:TExprExpr, t:T, p:core.Globals.Pos) : TExpr {
		return { eexpr : e, etype : t, epos : p };
	}

	public static function mk_block (e:TExpr) : TExpr {
		return switch (e.eexpr) {
			case TBlock(_): e;
			case _: mk(TBlock([e]), e.etype, e.epos);
		}
	}

	public static function mk_cast(e:TExpr, t:T, p:core.Globals.Pos) : TExpr {
		return mk(TCast(e, None), t, p);
	}

	public static function null_ (t:T, p:core.Globals.Pos) : TExpr {
		return mk(TConst(TNull), t, p);
	}

	public static function mk_mono() : T {
		return TMono(new ocaml.Ref(None));
	}

	// ocaml: let rec t_dynamic = TDynamic t_dynamic
	static var dynamic_set:Bool = false;
	static var t_dynamic_internal:T = TDynamic(new Ref(null));
	public static var t_dynamic(get, never):T;
	static function get_t_dynamic(): T {
		if (!dynamic_set) {
			switch (t_dynamic_internal) {
				case TDynamic(ref):
					if (ref.get() == null) {
						ref.set(t_dynamic_internal);
					}
				case _:
			}
		}
		return t_dynamic_internal;
	}

	public static function mk_anon (fl:Map<String, TClassField>) : T {
		return TAnon({a_fields:fl, a_status:new Ref(Closed)});
	}

	/*
	 * We use this for display purposes because otherwise we never see the Dynamic type that
	 * is defined in StdTypes.hx. This is set each time a typer is created, but this is fine
	 * because Dynamic is the same in all contexts. If this ever changes we'll have to review
	 * how we handle this.
	 */
	public static var t_dynamic_def:Ref<T> = new Ref(t_dynamic);

	public static function tfun (pl:Array<T>, r:T) : T {
		return TFun({args:pl.map(function (t) { return {name:"", opt:false, t:t}; }), ret:r});
	}

	public static function fun_args (l:Array<{name:String, opt:Option<core.Ast.Expr>, t:T}>) : Array<TSignatureArg> {
		return l.map(function (sig) { return {name:sig.name, opt:sig.opt!=None, t:sig.t}; });
	}

	public static function mk_class (m:core.Type.ModuleDef, path:core.Path, pos:core.Globals.Pos, name_pos:core.Globals.Pos) : core.Type.TClass {
		return {
			cl_path: path,
			cl_module: m.clone(),
			cl_pos: pos,
			cl_name_pos: name_pos,
			cl_doc: None,
			cl_meta: [],
			cl_private: false,
			cl_kind: KNormal,
			cl_extern: false,
			cl_interface: false,
			cl_params: [],
			cl_super: None,
			cl_implements: [],
			cl_fields: new Map<String, TClassField>(),
			cl_ordered_statics: [],
			cl_ordered_fields: [],
			cl_statics: new Map<String, TClassField>(),
			cl_dynamic: None,
			cl_array_access: None,
			cl_constructor: None,
			cl_init: None,
			cl_overrides: [],
			cl_build: function () {return Built;},
			cl_restore: function () {},
			cl_descendants: []
		};
	}

	public static function module_extra (file:String,  sign:String, time:Float, kind:core.Type.ModuleKind, policy:Array<core.Type.ModuleCheckPolicy>) : core.Type.ModuleDefExtra {
		return {
			m_file: file,
			m_sign: sign,
			m_dirty: None,
			m_added: 0,
			m_mark: 0,
			m_time: time,
			m_processed: 0,
			m_deps: new Map<Int, ModuleDef>(),
			m_kind: kind,
			m_binded_res: new Map<String, String>(),
			m_reuse_macro_calls: [],
			m_if_feature: [],
			m_features: new Map<String, Bool>(),
			m_check_policy: policy,
		};
	}

	public static function mk_field (name:String, t:core.Type.T, p:core.Globals.Pos, name_pos:core.Globals.Pos) : core.Type.TClassField {
		return {
			cf_name: name,
			cf_type: t,
			cf_pos: p,
			cf_name_pos: name_pos,
			cf_doc: None,
			cf_meta: [],
			cf_public: true,
			cf_kind: Var({ v_read: AccNormal, v_write: AccNormal}),
			cf_expr: None,
			cf_expr_unoptimized: None,
			cf_params: [],
			cf_overloads: []
		};
	}

	public static var null_module = {
		m_id: alloc_mid(),
		m_path: new core.Path([], ""),
		m_types: [],
		m_extra: module_extra("", "", 0.0, MFake, [])
	};

	public static function null_class () {
		var c = mk_class(null_module, new core.Path([],""), core.Globals.null_pos, core.Globals.null_pos);
		c.cl_private = true;
		return c;
	}

	public static function null_field () : core.Type.TClassField {
		return mk_field("", t_dynamic, core.Globals.null_pos, core.Globals.null_pos);
	}

	public static function add_dependency(m:ModuleDef, mdep:ModuleDef) : Void {
		if (!m.equals(null_module) && !m.equals(mdep)) {
			m.m_extra.m_deps.set(mdep.m_id, mdep);
		}
	}

	public static function t_infos(mt:ModuleType) : TInfos {
		return switch (mt) {
			case TClassDecl(c):
				{
					mt_path: c.cl_path,
					mt_module: c.cl_module,
					mt_pos : c.cl_pos,
					mt_name_pos : c.cl_name_pos,
					mt_private : c.cl_private,
					mt_doc : c.cl_doc,
					mt_meta : c.cl_meta,
					mt_params : c.cl_params
				};
			case TEnumDecl(e): 
				{
					mt_path: e.e_path,
					mt_module: e.e_module,
					mt_pos : e.e_pos,
					mt_name_pos : e.e_name_pos,
					mt_private : e.e_private,
					mt_doc : e.e_doc,
					mt_meta : e.e_meta,
					mt_params : e.e_params
				};
			case TTypeDecl(t):
				{
					mt_path: t.t_path,
					mt_module: t.t_module,
					mt_pos : t.t_pos,
					mt_name_pos : t.t_name_pos,
					mt_private : t.t_private,
					mt_doc : t.t_doc,
					mt_meta : t.t_meta,
					mt_params : t.t_params
				};
			case TAbstractDecl(a):
				{
					mt_path: a.a_path,
					mt_module: a.a_module,
					mt_pos : a.a_pos,
					mt_name_pos : a.a_name_pos,
					mt_private : a.a_private,
					mt_doc : a.a_doc,
					mt_meta : a.a_meta,
					mt_params : a.a_params
				};
		}
	}


	public static function t_path(t:ModuleType) : core.Path {
		return t_infos(t).mt_path;
	}

	public static function is_parent (csup:TClass, c:TClass) : Bool {
		if(c.equals(csup) || ocaml.List.exists(function (imp:{c:TClass, params:TParams}) { return is_parent(csup, imp.c); }, c.cl_implements)) {
			return true;
		}
		else {
			return switch (c.cl_super) {
				case None: false;
				case Some({c:c}): is_parent(csup, c);
			}
		}
	}

	public static function lazy_type(f:ocaml.Ref<TLazy>) : core.Type.T {
		return switch (f.get()) {
			case LAvailable(t): t;
			case LProcessing(t), LWait(t): t();
		};
	}

	static function shallowCopyOfTClassField (f:TClassField) : TClassField {
		return {
			cf_name : f.cf_name,
			cf_type : f.cf_type,
			cf_public : f.cf_public,
			cf_pos : f.cf_pos,
			cf_name_pos : f.cf_name_pos,
			cf_doc : f.cf_doc,
			cf_meta : f.cf_meta,
			cf_kind : f.cf_kind,
			cf_params : f.cf_params,
			cf_expr : f.cf_expr,
			cf_expr_unoptimized : f.cf_expr_unoptimized,
			cf_overloads : f.cf_overloads
		}
	};

	public static function lazy_available(t:core.Type.T) : TLazy {
		return LAvailable(t);
	}
	public static function lazy_processing(f:Void->core.Type.T) : TLazy {
		return LProcessing(f);
	}
	public static function lazy_wait(f:Void->core.Type.T) : TLazy {
		return LWait(f);
	}

	public static function map (loop:T->T, t:T) : T {
		return switch (t) {
			case TMono(r):
				switch (r.get()) {
					case None: t;
					case Some(t): loop(t); // erase // ?
				}
			case TEnum(_,l), TInst(_, l), TType(_, l) if (l.length == 0):
				t;
			case TEnum(e, tl):
				TEnum(e, tl.map(loop));
			case TInst(c, tl):
				TInst(c, tl.map(loop));
			case TType(t2, tl):
				TType(t2, tl.map(loop));
			case TAbstract(a, tl):
				TAbstract(a, tl.map(loop));
			case TFun({args:tl, ret:r}):
				TFun({args:tl.map(function (arg) { return {name:arg.name, opt:arg.opt, t:loop(arg.t)}; }), ret:loop(r)});
			case TAnon(a):
				var fields = ocaml.PMap.map(function(f:TClassField) {
					var clone = f.clone();
					clone.cf_type = loop(f.cf_type);
					return clone;
				}, a.a_fields);
				switch (a.a_status.get()) {
					case Opened:
						a.a_fields = fields;
						t;
					case status:
						TAnon({a_fields:fields, a_status:new Ref(status)});
				}
			case TLazy(f):
				var ft = lazy_type(f);
				var ft2 = loop(ft);
				(ft.equals(ft2)) ? t : ft2;
			case TDynamic(_.get()=>t2):
				(t == t2)  ? t : TDynamic(new Ref(loop(t2)));
		}
	}

	/* substitute parameters with other types */
	public static function apply_params (cparams:TypeParams, params:Array<T>, t:T) : T {
		if (cparams.length == 0) { return t; }

		function rloop(l1:TypeParams, l2:Array<T>) : Array<{a:T, b:T}> {
			if (l1.length == 0 && l2.length == 0) {
				return [];
			}

			if (l1.length != l2.length) {
				throw false;
			}

			var e1 = l1[0];
			var e2 = l2[0];
			return switch (e1.t) {
				case TLazy(f):
					var ll1 = l1.slice(1);
					ll1.unshift({name:e1.name, t:lazy_type(f)});
					rloop(ll1, l2);
				default:
					var ll = rloop(l1.slice(1), l2.slice(2));
					ll.unshift({a:e1.t, b:e2});
					ll;
			}
		}
		var subst = rloop(cparams, params);

		function loop (t:T) {
			for (element in subst) {
				if (element.a == t) {
					return element.b;
				}
			}
			return switch (t) {
				case TMono(_.get()=>r):
					switch (r) {
						case None: t;
						case Some(v): loop(v);
					}
				case TEnum(e, tl):
					if (tl.length == 0) {
						t;
					}
					else {
						TEnum(e, tl.map(loop));
					}
				case TType(t2, tl):
					if (tl.length == 0) {
						t;
					}
					else {
						TType(t2, tl.map(loop));
					}
				case TAbstract(a, tl):
					if (tl.length == 0) {
						t;
					}
					else {
						TAbstract(a, tl.map(loop));
					}
				case TInst(c, tl):
					if (tl.length == 0) { return t; }
					if (tl.length == 1) {
						switch (tl[0]) {
							case TMono(r):
								switch (r.get()) {
									case Some(tt):
										if ( t == tt ) {
											// for dynamic
											var pt = mk_mono();
											var params = [pt];
											var t = TInst(c, params);
											switch (pt) {
												case TMono(r) :
													r.set(Some(t));
												case _:
													throw false; // never
											}
											return t;
										}
									default:
								}
							default:
						}
					}
					return TInst(c, tl.map(loop));
				case TFun({args:tl, ret:r}):
					TFun({args: tl.map(function(s:{name:String, opt:Bool, t:T}) {
							return {name:s.name,
									opt:s.opt,
									t: loop(s.t)};
						}),
						ret:loop(r)});
				case TAnon(a):
					function pmap_anon (f:TClassField) : TClassField {
						var clone = f.clone();
						clone.cf_type = loop(f.cf_type);
						return clone;
					}
					var fields = ocaml.PMap.map(pmap_anon, a.a_fields);
					switch (a.a_status.get()) {
						case Opened:
							a.a_fields = fields;
							t;
						default:
							TAnon({
								a_fields: fields,
								a_status: a.a_status
							});
					}
				case TLazy(f):
					var ft = lazy_type(f);
					var ft2 = loop(ft);
					if (ft == ft2) {
						t;
					}
					else {
						ft2;
					}
				case TDynamic(t2):
					if (t2.get() == t)
						t;
					else
						TDynamic(new Ref(loop(t2.get())));
			}
		}
		return loop(t);
	}

	public static function monomorphs (eparams:TypeParams, t:T) : T {
		return apply_params(eparams, eparams.map(function (_) { return mk_mono(); }), t);
	}

	public static function follow (t:core.Type.T) : core.Type.T {
		return switch(t) {
			case TMono(r):
				switch (r.get()) {
					case Some(tt): follow(tt);
					case None: t;
				}
			case TLazy(f):
				follow(lazy_type(f));
			case TType (tt,tl):
				follow(apply_params(tt.t_params, tl, tt.t_type));
			case TAbstract(a,[tt]):
				if (a.a_path == new core.Path([],"Null")) {
					follow(tt);
				}
				else {
					t;
				}
			default: t;
		};
	}

	public static function is_nullable (t:core.Type.T) : Bool {
		return switch (t) {
			case TMono(_.get()=>r):
				switch (r) {
					case None: false;
					case Some(t): is_nullable(t);
				}
			case TAbstract (a, tl):
				if (a.a_path == new core.Path([], "Null") && tl.length == 1) {
					true;
				}
				else if (core.Meta.has(CoreType, a.a_meta)) {
					!core.Meta.has(NotNull, a.a_meta);
				}
				else {
					!core.Meta.has(NotNull, a.a_meta) && is_nullable(apply_params(a.a_params, tl, a.a_this));
				}
			case TLazy(f):
				is_nullable(lazy_type(f));
			case TType (t,tl):
				is_nullable(apply_params(t.t_params, tl, t.t_type));
			case TFun(_):
				false;
			/*
				Type parameters will most of the time be nullable objects, so we don't want to make it hard for users
				to have to specify Null<T> all over the place, so while they could be a basic type, let's assume they will not.

				This will still cause issues with inlining and haxe.rtti.Generic. In that case proper explicit Null<T> is required to
				work correctly with basic types. This could still be fixed by redoing a nullability inference on the typed AST.

				| TInst ({ cl_kind = KTypeParameter },_) -> false
			*/
			case _:
				true;
		}
	}

	public static function is_null (?no_lazy:Bool=false, t:core.Type.T) : Bool {
		return switch (t) {
			case TMono(_.get()=>r):
				switch (r) {
					case None: false;
					case Some(t): is_null(t);
				}
			case TAbstract(a, tl):
				if (a.a_path == new core.Path([], "Null") && tl.length == 1) {
					!is_nullable(follow(tl[0]));
				}
				else {
					false;
				}
			case TLazy(f):
				if (no_lazy) {
					throw new ocaml.Exit();
				}
				else {
					is_null(lazy_type(f));
				}
			case TType(t, tl):
				is_null(apply_params(t.t_params, tl, t.t_type));
			case _:
				false;
		}
	}

	// Determines if we have a Null<T>. Unlike is_null, this returns true even if the wrapped type is nullable itself.
	public static function is_explicit_null (t:T) : Bool {
		return switch (t) {
			case TMono(r):
				switch (r.get()) {
					case None: false;
					case Some(_t): is_null(_t);
				}
			case TAbstract(a, ts) if (a.a_path.equals(new core.Path([], "Null")) && ts.length == 1):
				true;
			case TLazy(f):
				is_null(lazy_type(f));
			case TType(t, tl):
				is_null(apply_params(t.t_params, tl, t.t_type));
			case _: false;
		}
	}

	public static function has_mono (t:T) : Bool {
		return switch (t) {
			case TMono(r):
				switch (r.get()) {
					case None: true;
					case Some(_t): has_mono(_t);
				}
			case TInst(_, pl), TEnum(_, pl), TAbstract(_, pl), TType(_, pl):
				ocaml.List.exists(has_mono, pl);
			case TDynamic(_): false;
			case TFun(f):
				has_mono(f.ret) || ocaml.List.exists(function (arg) { return has_mono(arg.t); }, f.args);
			case TAnon(a):
				ocaml.PMap.fold(function (cf, b) { return b || has_mono(cf.cf_type); }, a.a_fields, false);
				// ocaml.PMap.fold(function (cf, b) { return has_mono(cf.cf_type) || b; }, a.a_fields, false);
			case TLazy(f):
				has_mono(lazy_type(f));
		}
	}

	public static function concat (e1:TExpr, e2:TExpr) : TExpr {
		var e = switch ({fst:e1.eexpr, snd:e2.eexpr}) {
			case {fst:TBlock(el1), snd:TBlock(el2)}: TBlock(el1.concat(el2));
			case {fst:TBlock(el1)}: TBlock(el1.concat([e2]));
			case {snd:TBlock(el2)}: TBlock([e1].concat(el2));
			case _: TBlock([e1, e2]);
		}
		return mk(e, e2.etype, core.Ast.punion(e1.epos, e2.epos));
	}

	public static function is_closed(a:TAnon) : Bool {
		return a.a_status.get() != Opened;
	}

	public static function type_of_module_type (mt:ModuleType) : T {
		function snd (e:{name : String, t : T}) : T {
			return e.t;
		}
		return switch (mt) {
			case TClassDecl(c): TInst(c, c.cl_params.map(snd));
			case TEnumDecl(e): TEnum(e, e.e_params.map(snd));
			case TTypeDecl(t): TType(t, t.t_params.map(snd));
			case TAbstractDecl(a): TAbstract(a,a.a_params.map(snd));
		}
	}

	public static function module_type_of_type (t:T) : ModuleType {
		return switch (t) {
			case TInst(c, _): TClassDecl(c);
			case TEnum(en, _): TEnumDecl(en);
			case TType(t, _): TTypeDecl(t);
			case TAbstract(a, _): TAbstractDecl(a);
			case TLazy(f): module_type_of_type(lazy_type(f));
			case TMono(r):
				switch (r.get()) {
					case Some(t): module_type_of_type(t);
					case None: throw new ocaml.Exit();
				}
			case _: throw new ocaml.Exit();
		}
	}

	public static function has_ctor_constraint (c:TClass) : Bool {
		return switch (c.cl_kind) {
			case KTypeParameter(tl):
				ocaml.List.exists(function (t) {
					return switch (core.Type.follow(t)) {
						case TAnon(a) if (ocaml.PMap.mem("new", a.a_fields)): true;
						case TAbstract(a, _) if (a.a_path.equals(new core.Path(["haxe"], "Constructible"))): true;
						case _: false;
					};
				}, tl);
			case _: false;
		};
	}

	// ======= Field utility =======

	public static function field_name (f:TFieldAccess) : String {
		return switch (f) {
			case FAnon(f), FInstance(_,_, f), FStatic(_, f), FClosure(_, f):
				f.cf_name;
			case FEnum(_, f) : f.ef_name;
			case FDynamic(n): n;
		}
	}

	public static function extract_field (f:TFieldAccess) : Option<TClassField> {
		return switch (f) {
			case FAnon(f), FInstance(_, _, f), FStatic(_, f), FClosure(_, f):Some(f);
			case _: None;
		}
	}

	public static function is_physical_field (f:TClassField) : Bool {
		return switch(f.cf_kind) {
			case Method(_): true;
			case Var({v_read:r, v_write:w}) if (r==AccNormal || r == AccInline || r == AccNo || w == AccNormal || w == AccNo):
				true;
			case _: core.Meta.has(IsVar, f.cf_meta);
		}
	}

	public static function field_type (f:TClassField) : T {
		return (f.cf_params.length == 0) ? f.cf_type : monomorphs(f.cf_params, f.cf_type);
	}

	public static function raw_class_field (build_type:TClassField->T, c:TClass, tl:TParams, i:String) : {fst:Option<{c:TClass, params:TParams}>, snd:T, trd:TClassField} {
		var apply = apply_params.bind(c.cl_params).bind(tl);
		return try {
			var f = PMap.find(i, c.cl_fields);
			{fst:Some({c:c, params:tl}), snd:build_type(f), trd:f};
		}
		catch (_:ocaml.Not_found) {
			try {
				switch (c.cl_constructor) {
					case Some(ctor) if (i == "new"): {fst:Some({c:c, params:tl}), snd:build_type(ctor), trd:ctor};
					case _: throw ocaml.Not_found.instance;
				}
			}
			catch (_:ocaml.Not_found) {
				try {
					switch (c.cl_super) {
						case None: throw ocaml.Not_found.instance;
						case Some({c:c, params:tl}):
							var _tmp = raw_class_field(build_type, c, tl.map(apply), i);
							{fst:_tmp.fst, snd:apply_params(c.cl_params, tl, _tmp.snd), trd:_tmp.trd};
					}
				}
				catch (_:ocaml.Not_found) {
					switch (c.cl_kind) {
						case KTypeParameter(tl):
							function loop(l:TParams) : {fst:Option<{c:TClass, params:TParams}>, snd:T, trd:TClassField} {
								if (l.length == 0) { throw ocaml.Not_found.instance; }
								else {
									var t = l[0];
									var ctl = l.slice(1);
									return switch (follow(t)) {
										case TAnon(a):
											try {
												var f = PMap.find(i, a.a_fields);
												{fst:None, snd:build_type(f), trd:f};
											}
											catch (_:ocaml.Not_found) {
												loop(ctl);
											}
										case TInst(c,tl):
											try {
												var _tmp = raw_class_field(build_type, c, tl.map(apply), i);
												{fst:_tmp.fst, snd:apply_params(c.cl_params, tl, _tmp.snd), trd:_tmp.trd};
											}
											catch (_:ocaml.Not_found) {
												loop(ctl);
											}
										case _: loop(ctl);
									}
								}
							}
							loop(tl);
						case _:
							if (!c.cl_interface) { throw ocaml.Not_found.instance; }
							/*
							 * an interface can implements other interfaces without
							 * having to redeclare its fields
							 */
							function loop (l:Array<{c:TClass, params:TParams}>) : {fst:Option<{c:TClass, params:TParams}>, snd:T, trd:TClassField}{
								if (l.length == 0) { throw ocaml.Not_found.instance; }
								else {
									return try {
										var c = l[0].c;
										var tl = l[0].params;
										var _tmp = raw_class_field(build_type, c, tl.map(apply), i);
										{fst:_tmp.fst, snd:apply_params(c.cl_params, tl, _tmp.snd), trd:_tmp.trd};
									}
									catch (_:ocaml.Not_found) {
										loop(l.slice(1));
									}
								}
							}
							loop(c.cl_implements);
					}
				}
			}
		}
	}

	public static var class_field = raw_class_field.bind(field_type);

	public static function quick_field (t:T, n:String) : TFieldAccess {
		return switch (follow(t)) {
			case TInst(c, tl):
				var _tmp = raw_class_field(function (f) { return f.cf_type; }, c, tl, n);
				switch (_tmp.fst) {
					case None: FAnon(_tmp.trd);
					case Some({c:c, params:tl}): FInstance(c, tl, _tmp.trd);
				}
			case TAnon(a):
				switch (a.a_status.get()) {
					case EnumStatics(e):
						var ef = PMap.find(n, e.e_constrs);
						FEnum(e, ef);
					case Statics(c): FStatic(c, PMap.find(n, c.cl_statics));
					case AbstractStatics(a):
						switch (a.a_impl) {
							case Some(c):
								var cf = PMap.find(n, c.cl_statics);
								FStatic(c, cf); // is that right?
							case _: throw ocaml.Not_found.instance;
						}
					case _: FAnon(PMap.find(n, a.a_fields));
				}
			case TDynamic(_): FDynamic(n);
			case TEnum(_), TMono(_), TAbstract(_), TFun(_): throw ocaml.Not_found.instance;
			case TLazy(_), TType(_): throw false;
		}
	}

	public static function quick_field_dynamic (t:T, s:String) : TFieldAccess {
		return try {
			quick_field(t, s);
		}
		catch (_:ocaml.Not_found) {
			FDynamic(s);
		}
	}

	public static function get_constructor (build_type:TClassField->T, c:TClass) : {fst:T, snd:TClassField} {
		return switch ({fst:c.cl_constructor, snd:c.cl_super}) {
			case {fst:Some(c)}: {fst:build_type(c), snd:c};
			case {fst:None, snd:None}: throw ocaml.Not_found.instance;
			case {snd:Some({c:csup, params:cparams})}:
				var _tmp = get_constructor(build_type, csup);
				var t = _tmp.fst; var c = _tmp.snd;
				{fst:apply_params(csup.cl_params, cparams, t), snd:c}
		}
	}

	// ======= Printing =======

	/** not sure if Array<String> */
	public static function print_context () : Array<String> {
		return [];
	}

	/** not sure if ctx is Array<String> */
	public static function s_type (ctx:Array<String>, t:T) : String {
		trace("TODO core.Type.s_type");
		return null;
	}

	public static function s_access (is_read:Bool, access:VarAccess) : String {
		return switch (access) {
			case AccNormal: "default";
			case AccNo: "null";
			case AccNever: "never";
			case AccResolve: "resolve";
			case AccCall: (is_read) ? "get" : "set";
			case AccInline: "inline";
			case AccRequire(n, _): "require "+n;
			case AccCtor: "ctor";
		}
	}

	public static function s_kind (fk:FieldKind) : String {
		return switch (fk) {
			case Var({v_read:AccNormal, v_write:AccNormal}): "var";
			case Var(v): "("+s_access(true, v.v_read)+","+s_access(false, v.v_write)+")";
			case Method(m):
				switch (m) {
					case MethNormal: "method";
					case MethDynamic: "dynamic method";
					case MethInline: "inline method";
					case MethMacro: "macro method";
				}
		}
	}

	// ======= Unification =======
	public static function link (e:ocaml.Ref<Option<T>>, a:T, b:T) : Bool {
		// tell if setting a == b will create a type-loop
		function loop (t:T) : Bool {
			if (t.equals(a)) {
				return true;
			}
			else {
				return switch (t) {
					case TMono(t):
						switch (t.get()) {
							case None: false;
							case Some(t): loop(t);
						}
					case TEnum(_, tl), TInst(_, tl), TType(_, tl), TAbstract(_, tl): ocaml.List.exists(loop, tl);
					case TFun(fun):
						// ocaml.List.exists(function (arg) { return loop(arg.t);}, f.args) || loop(f.ret);
						loop(fun.ret) || ocaml.List.exists(function (arg) { return loop(arg.t);}, fun.args);
					case TDynamic(t2):
						if (t == t2.get()) {
							false;
						}
						else {
							loop(t2.get());
						}
					case TLazy(f):
						loop(lazy_type(f));
					case TAnon(a):
						ocaml.PMap.fold(function (value, b) { return b || loop(value.cf_type); } , a.a_fields, false);
				}
			}
		}
		// tell is already a ~= b
		if (loop(b)) {
			return follow(b).equals(a);
		}
		else if (b.equals(t_dynamic)) {
			return true;
		}
		else {
			e.set(Some(b));
			return true;
		}
	}

	public static function link_dynamic (a:T, b:T) : Void {
		switch {fst:follow(a), snd:follow(b)} {
			case {fst:TMono(r), snd:TDynamic(_)}:
				r.set(Some(b));
			case {fst:TDynamic(_), snd:TMono(r)}:
				r.set(Some(a));
			case _:
		}
	}

	public static function fast_eq (a:T, b:T) : Bool {
		if (a.equals(b)) {
			return true;
		}
		else {
			return switch ({fst:a, snd:b}) {
				case {fst:TFun({args:l1, ret:r1}), snd:TFun({args:l2, ret:r2})} if (l1.length == l2.length):
					ocaml.List.for_all2 (function (a1, a2) { return fast_eq(a1.t, a2.t); }, l1, l2) && fast_eq(r1, r2);
				case {fst:TType(t1, l1), snd:TType(t2, l2)}:
					t1.equals(t2) && ocaml.List.for_all2(fast_eq, l1, l2);
				case {fst:TEnum(e1, l1), snd:TEnum(e2, l2)}:
					e1.equals(e2) && ocaml.List.for_all2(fast_eq, l1, l2);
				case {fst:TInst(c1, l1), snd:TInst(c2, l2)}:
					c1.equals(c2) && ocaml.List.for_all2(fast_eq, l1, l2);
				case {fst:TAbstract(a1, l1), snd:TAbstract(a2, l2)}:
					a1.equals(a2) && ocaml.List.for_all2(fast_eq, l1, l2);
				case _: false;
			}
		}
	}

	public static function fast_eq_mono (ml, a:T, b:T) : Bool {
		if (a.equals(b)) {
			return true;
		}
		else {
			return switch ({fst:a, snd:b}) {
				case {fst:TFun({args:l1, ret:r1}), snd:TFun({args:l2, ret:r2})} if (l1.length == l2.length):
					ocaml.List.for_all2 (function (a1, a2) { return fast_eq_mono(ml, a1.t, a2.t); }, l1, l2) && fast_eq_mono(ml, r1, r2);
				case {fst:TType(t1, l1), snd:TType(t2, l2)}:
					t1.equals(t2) && ocaml.List.for_all2(fast_eq_mono.bind(ml), l1, l2);
				case {fst:TEnum(e1, l1), snd:TEnum(e2, l2)}:
					e1.equals(e2) && ocaml.List.for_all2(fast_eq_mono.bind(ml), l1, l2);
				case {fst:TInst(c1, l1), snd:TInst(c2, l2)}:
					c1.equals(c2) && ocaml.List.for_all2(fast_eq_mono.bind(ml), l1, l2);
				case {fst:TAbstract(a1, l1), snd:TAbstract(a2, l2)}:
					a1.equals(a2) && ocaml.List.for_all2(fast_eq_mono.bind(ml), l1, l2);
				case {fst:TMono(_)}:
					ocaml.List.memq(a, ml);
				case _: false;
			}
		}
	}

	public static function cannot_unify (a:T, b:T) : UnifyError {
		return Cannot_unify(a, b);
	}
	public static function invalid_field (n:String) : UnifyError {
		return Invalid_field_type(n);
	}
	public static function invalid_kind (n:String, a:FieldKind, b:FieldKind) : UnifyError {
		return Invalid_kind(n, a, b);
	}
	public static function invalid_visibility (n:String) : UnifyError {
		return Invalid_visibility(n);
	}
	public static function has_no_field (t:T, n:String): UnifyError {
		return Has_no_field(t, n);
	}
	public static function has_extra_field (t:T, n:String) : UnifyError {
		return Has_extra_field(t, n);
	}

	public static function error (l:Array<UnifyError>) : Dynamic {
		throw new Unify_error(l);
	}

	public static function has_meta(m:core.Meta.StrictMeta, ml:core.Ast.Metadata) : Bool {
		return ocaml.List.exists(function (me:core.Ast.MetadataEntry) { return m.equals(me.name);}, ml);
	}
	public static function get_meta(m:core.Meta.StrictMeta, ml:core.Ast.Metadata) : core.Ast.MetadataEntry {
		return ocaml.List.find(function (me:core.Ast.MetadataEntry) { return m.equals(me.name);}, ml);
	}

	public static var no_meta:core.Ast.Metadata = [];

	/*
	 * we can restrict access as soon as both are runtime-compatible
	 */
	public static function unify_access (a1:core.Type.VarAccess, a2:core.Type.VarAccess) : Bool {
		if (a1.equals(a2)) { return true; }
		return switch ({fst:a1, snd:a2}) {
			case {snd:AccNo}, {snd:AccNever}: true;
			case {fst:AccInline, snd:AccNormal}: true;
			case _: false;
		}
	}

	public static function direct_access (a:core.Type.VarAccess) : Bool {
		return switch (a) {
			case AccNo, AccNever, AccNormal, AccInline, AccRequire(_), AccCtor: true;
			case AccResolve, AccCall: false;
		}
	}

	public static function unify_kind (k1:core.Type.FieldKind, k2:core.Type.FieldKind) : Bool {
		if (k1.equals(k2)) { return true; }
		return switch ({fst:k1, snd:k2}) {
			case {fst:Var(v1), snd:Var(v2)}:
				unify_access(v1.v_read, v2.v_read) && unify_access(v1.v_write, v2.v_write);
			case {fst:Var(v), snd:Method(m)}:
				switch ({fst:v.v_read, snd:v.v_write, trd:m}) {
					case {fst:AccNormal, trd:MethNormal}: true;
					case {fst:AccNormal, snd:AccNormal, trd:MethDynamic}: true;
					case _: false;
				}
			case {fst:Method(m), snd:Var(v)}:
				switch (m) {
					case MethDynamic: direct_access(v.v_read) && direct_access(v.v_write);
					case MethMacro: false;
					case MethNormal, MethInline:
						switch ({fst:v.v_read, snd:v.v_write}) {
							case {fst:AccNormal, snd:AccNo}, {fst:AccNormal, snd:AccNever}: true;
							case _: false;
						}
				}
			case {fst:Method(m1), snd:Method(m2)}:
				switch ({fst:m1, snd:m2}) {
					case {fst:MethInline, snd:MethNormal}, {fst:MethDynamic, snd:MethNormal}: true;
					case _: false;
				}
		}
	}

	public static var eq_stack = new Ref<Array<{fst:T, snd:T}>>([]);

	public static function rec_stack(stack:Ref<Array<{fst:T, snd:T}>>, value:{fst:T, snd:T}, fcheck:{fst:T, snd:T}->Bool, frun:Void->Void, ferror:Array<UnifyError>->Void) : Void {
		if (!ocaml.List.exists(fcheck, stack.get())) {
			try {
				stack.get().unshift(value);
				// var v = frun();
				frun();
				stack.get().shift();
				// v;
			}
			catch (err:Unify_error) {
				stack.get().shift();
				ferror(err.l);
			}
			catch (_:Bool) {
				throw false;
			}
			catch (e:Any) {
				stack.get().shift();
				throw e;
			}
		}
	}

	public static function rec_stack_bool (stack:Ref<Array<{fst:T, snd:T}>>, value:{fst:T, snd:T}, fcheck:{fst:T, snd:T}->Bool, frun:Void->Void) : Bool {
		if (ocaml.List.exists(fcheck, stack.get())) {
			return false;
		} 
		else {
			try {
				stack.get().unshift(value);
				frun();
				stack.get().shift();
				return true;
			}
			catch (err:Unify_error) {
				stack.get().shift();
				return false;
			}
			catch (_:Bool) {
				throw false;
			}
			catch (e:Any) {
				stack.get().shift();
				throw e;
			}
		}
	}

	public static function type_eq(param:Eq_kind, a:T, b:T) : Void {
		function can_follow(t:T) : Bool {
			return switch (param) {
				case EqCoreType: false;
				case EqDoNotFollowNull: !is_explicit_null(t);
				case _: true;
			}
		}
		if (a == b || a.equals(b)) {
		}
		else {
			switch ({fst:a, snd:b}) {
				case {fst:TLazy(f)}: type_eq(param, lazy_type(f), b);
				case {snd:TLazy(f)}: type_eq(param, a, lazy_type(f));
				case {fst:TMono(t)}:
					switch (t.get()) {
						case None:
							if (param == EqCoreType || !link(t, a, b)) {
								error([cannot_unify(a,b)]);
							}
						case Some(_t): type_eq(param, _t, b);
					}
				case {snd:TMono(t)}:
					switch (t.get()) {
						case None:
							if (param == EqCoreType || !link(t, b, a)) {
								error([cannot_unify(a,b)]);
							}
						case Some(_t): type_eq(param, a, _t);
					}
				case {fst:TType(t1, tl1), snd:TType(t2, tl2)} if (t1.equals(t2) || (param == EqCoreType && t1.t_path.equals(t2.t_path)) && tl1.length == tl2.length):
					ocaml.List.iter2(type_eq.bind(param), tl1, tl2);
				case {fst:TType(t, tl)} if (can_follow(a)):
					type_eq(param, apply_params(t.t_params, tl, t.t_type), b);
				case {snd:TType(t, tl)} if (can_follow(b)):
					rec_stack(eq_stack, {fst:a, snd:b},
						function (e) { return fast_eq(a, e.fst) && fast_eq(b, e.snd); },
						function () { type_eq(param, a, apply_params(t.t_params, tl, t.t_type)); },
						function (l) { 
							return error([cannot_unify(a,b)].concat(l));
						}
					);
				case {fst:TEnum(e1, tl1), snd:TEnum(e2, tl2)}:
					if (e1 != e2 && !(param == EqCoreType && e1.e_path.equals(e2.e_path))) {
						error([cannot_unify(a,b)]);
					}
					ocaml.List.iter2(type_eq.bind(param), tl1, tl2);
				case {fst:TInst(c1, tl1), snd:TInst(c2, tl2)}:
					if (c1 != c2 && !(param == EqCoreType && c1.cl_path.equals(c2.cl_path)) && switch ({fst:c1.cl_kind, snd:c2.cl_kind}) { case {fst:KExpr(_), snd:KExpr(_)}: false; case  _: true;}) {
						error([cannot_unify(a,b)]);
					}
					ocaml.List.iter2(type_eq.bind(param), tl1, tl2);
				case {fst:TFun({args:l1, ret:r1}), snd:TFun({args:l2, ret:r2})} if (l1.length == l2.length):
					try {
						type_eq(param, r1, r2);
						ocaml.List.iter2(function (a, b) {
							if (a.opt != b.opt) { error([Not_matching_optional(a.name)]); }
							type_eq(param, a.t, b.t);
						}, l1, l2);
					}
					catch (err:Unify_error) {
						error([cannot_unify(a, b)].concat(err.l));
					}
				case {fst:TDynamic(a), snd:TDynamic(b)}:
					type_eq(param, a.get(), b.get());
				case {fst:TAbstract(a1, tl1), snd:TAbstract(a2, tl2)} if (a1.a_path.equals(new core.Path([], "Null")) && a2.a_path.equals(new core.Path([], "Null")) && tl1.length == 1 && tl2 .length == 2):
					type_eq(param, tl1[0], tl2[0]);
				case {fst:TAbstract(a1, tl)} if (a1.a_path.equals(new core.Path([], "Null")) && tl.length == 1 && param != EqDoNotFollowNull):
					type_eq(param, tl[0], b);
				case {snd:TAbstract(a2, tl)} if (a2.a_path.equals(new core.Path([], "Null")) && tl.length == 1 && param != EqDoNotFollowNull):
					type_eq(param, a, tl[0]);
				case {fst:TAbstract(a1, tl1), snd:TAbstract(a2, tl2)}:
					if (a1 != a2 && !(param == EqCoreType && a1.a_path.equals(a2.a_path))) {
						error([cannot_unify(a, b)]);
					}
					ocaml.List.iter2(type_eq.bind(param), tl1, tl2);
				case {fst:TAnon(a1), snd:TAnon(a2)}:
					try {
						ocaml.PMap.iter(function (n, f1:TClassField) {
							try {
								var f2 = ocaml.PMap.find(n, a2.a_fields);
								if (!f1.cf_kind.equals(f2.cf_kind) && (param == EqStrict || param == EqCoreType || !unify_kind(f1.cf_kind, f2.cf_kind))) {
									error([invalid_kind(n, f1.cf_kind, f2.cf_kind)]);
								}
								var a = f1.cf_type;
								var b = f2.cf_type;
								rec_stack(eq_stack, {fst:a, snd:b}, 
									function (pair) { return fast_eq(a, pair.fst) && fast_eq(b, pair.snd); },
									function () { type_eq(param, a, b); },
									function (l) { error([invalid_field(n)].concat(l));}
								);
							}
							catch (_:ocaml.Not_found) {
								if (is_closed(a2)) {
									error([Has_no_field(b, n)]);
								}
								if (!link(new Ref(None), b, f1.cf_type)) {
									error([cannot_unify(a, b)]);
								}
								a2.a_fields.set(n, f1);
								
							}
						}, a1.a_fields);
						ocaml.PMap.iter(function (n, f2) {
							if (!ocaml.PMap.mem(n, a1.a_fields)) {
								if (is_closed(a1)) {
									error([has_no_field(a, n)]);
								}
								if (!link(new Ref(None), a, f2.cf_type)) {
									error([cannot_unify(a, b)]);
								}
								a1.a_fields.set(n, f2);
							}
						}, a2.a_fields);
					}
					catch (err:Unify_error) {
						error([cannot_unify(a, b)].concat(err.l));
					}
				case _:
					if (b.equals(t_dynamic) && (param == EqRightDynamic || param == EqBothDynamic)) {}
					else if (a.equals(t_dynamic) && param == EqBothDynamic) {}
					else {
						error([cannot_unify(a, b)]);
					}
			}
		}
	}

	public static function type_iseq(a:T, b:T) : Bool {
		return try {
			type_eq(EqStrict, a, b);
			true;
		}
		catch (_:Unify_error) {
			false;
		}
	}

	public static function type_iseq_strict(a:T, b:T) : Bool {
		return try {
			type_eq(EqDoNotFollowNull, a, b);
			true;
		}
		catch (_:Unify_error) {
			false;
		}
	}

	public static function unify (a:T, b:T) : Dynamic {
		trace("TODO: core.Type.unify");
		return null;
	}

	// ======= Mapping and iterating =======
	public static function iter (f:TExpr->Void, e:TExpr) : Void {
		switch(e.eexpr) {
			case TConst(_), TLocal(_), TBreak, TContinue, TTypeExpr(_), TIdent(_):
			case TArray(e1, e2), TBinop(_, e1, e2), TFor(_, e1, e2), TWhile(e1, e2, _):
				f(e1); f(e2);
			case TThrow(e), TField(e,_), TEnumParameter(e,_,_), TEnumIndex(e),
				 TParenthesis(e), TCast(e,_), TUnop(_,_,e), TMeta(_,e):
				f(e);
			case TArrayDecl(el), TNew(_,_,el), TBlock(el):
				for (e in el) {
					f(e);
				}
			case TObjectDecl(fl):
				for (of in fl) {
					f(of.expr);
				}
			case TCall(e1, el):
				f(e1);
				for (e in el) {
					f(e);
				}
			case TVar(_, eo):
				switch (eo) {
					case None:
					case Some(e): f(e);
				}
			case TFunction(fu):
				f(fu.tf_expr);
			case TIf(e, e1, e2):
				f(e); f(e1);
				switch (e2) {
					case Some(v): f(v);
					case None:
				}
			case TSwitch(e, cases, def):
				f(e);
				for (c in cases) {
					f(c.e);
					for (v in c.values) {
						f(v);
					}
				}
				switch (def) {
					case Some(v): f(v);
					case None:
				}
			case TTry(e, catches):
				f(e);
				for (c in catches) {
					f(c.e);
				}
			case TReturn(eo):
				switch (eo) {
					case Some(v): f(v);
					case None:
				}
		}
	}

	public static function map_expr (f:TExpr->TExpr, e:TExpr) : TExpr {
		var _e = e.clone();
		switch (e.eexpr) {
			case TConst(_), TLocal(_), TBreak, TContinue, TTypeExpr(_), TIdent(_):
				return e;
			case TArray(e1, e2):
				var e1 = f(e1);
				_e.eexpr = TArray(e1, f(e2));
			case TBinop(op, e1, e2):
				var e1 = f(e1);
				_e.eexpr = TBinop(op, e1, f(e2));
			case TFor(v, e1, e2):
				var e1 = f(e1);
				_e.eexpr = TFor(v, e1, f(e2));
			case TWhile(e1, e2, flags):
				var e1 = f(e1);
				_e.eexpr = TWhile(e1, f(e2), flags);
			case TThrow(e1):
				_e.eexpr = TThrow(f(e1));
			case TEnumParameter(e1, ef, i):
				_e.eexpr = TEnumParameter(f(e1), ef, i);
			case TEnumIndex(e1):
				_e.eexpr = TEnumIndex(f(e1));
			case TField(e1, v):
				_e.eexpr = TField(f(e1), v);
			case TParenthesis(e1):
				_e.eexpr = TParenthesis(f(e1));
			case TUnop(op, pre, e1):
				_e.eexpr = TUnop(op, pre, f(e1));
			case TArrayDecl(el):
				_e.eexpr = TArrayDecl(el.map(f));
			case TNew(t, pl, el):
				_e.eexpr = TNew(t, pl, el.map(f));
			case TBlock(el):
				_e.eexpr = TBlock(el.map(f));
			case TObjectDecl(el):
				_e.eexpr = TObjectDecl(el.map(function (a) {return {a:a.a, expr:f(a.expr)};}));
			case TCall(e1, el):
				var e1 = f(e1);
				_e.eexpr = TCall(e1, el.map(f));
			case TVar(v, eo):
				_e.eexpr = TVar(v, switch (eo) {
					case None:None;
					case Some(e): Some(f(e));
				});
			case TFunction (fu):
				var _fu = fu.clone();
				_fu.tf_expr = f(fu.tf_expr);
				_e.eexpr = TFunction(_fu);
			case TIf(ec, e1, e2):
				var ec = f(ec);
				var e1 = f(e1);
				_e.eexpr = TIf(ec, e1, switch (e2) {
					case None: None;
					case Some(e): Some(f(e));
				});
			case TSwitch (e1, cases, def):
				var e1 = f(e1);
				var cases = cases.map(function (c) { return {values:c.values.map(f), e:f(c.e)}});
				_e.eexpr = TSwitch(e1, cases, switch (def) {
					case None: None;
					case Some(e): Some(f(e));
				});
			case TTry (e1, catches):
				var e1 = f(e1);
				_e.eexpr = TTry(e1, catches.map(function (c) { return {v:c.v, e:f(c.e)};}));
			case TReturn (eo):
				_e.eexpr = TReturn (switch (eo) {
					case None: None;
					case Some(e): Some(f(e));
				});
			case TCast (e1, t):
				_e.eexpr = TCast(f(e1),t);
			case TMeta (m, e1):
				_e.eexpr = TMeta(m, f(e1));
		}
		return _e;
	}

	public static function map_expr_type (f:TExpr->TExpr, ft:T->T, fv:TVar->TVar, e:TExpr) : TExpr {
		var _e = e.clone();
		switch (e.eexpr) {
			case TConst(_), TBreak, TContinue, TTypeExpr(_), TIdent(_):
				_e.etype = ft(e.etype);
			case TLocal(v):
				_e.eexpr = TLocal(fv(v));
				_e.etype = ft(e.etype);
				_e;
			case TArray(e1, e2):
				var e1 = f(e1);
				_e.eexpr = TArray(e1, f(e2));
				_e.etype = ft(e.etype);
			case TBinop(op, e1, e2):
				var e1 = f(e1);
				_e.eexpr = TBinop(op, e1, f(e2));
				_e.etype = ft(e.etype);
			case TFor(v, e1, e2):
				var v = fv(v);
				var e1 = f(e1);
				_e.eexpr = TFor(v, e1, f(e2));
				_e.etype = ft(e.etype);
			case TWhile(e1, e2, flags):
				var e1 = f(e1);
				_e.eexpr = TWhile(e1, f(e2), flags);
				_e.etype = ft(e.etype);
			case TThrow(e1):
				_e.eexpr = TThrow(f(e1));
				_e.etype = ft(e.etype);
			case TEnumParameter(e1, ef, i):
				_e.eexpr = TEnumParameter(f(e1), ef, i);
				_e.etype = ft(e.etype);
			case TEnumIndex(e1):
				_e.eexpr = TEnumIndex(f(e1));
				_e.etype = ft(e.etype);
			case TField(e1, v):
				var e1 = f(e1);
				var v = try {
					var n = switch (v) {
						case FClosure(_): throw ocaml.Not_found.instance;
						case FAnon(f), FInstance(_,_,f), FStatic(_, f): f.cf_name;
						case FEnum(_,f): f.ef_name;
						case FDynamic(n): n;
					}
					quick_field(e1.etype, n);
				}
				catch (_:ocaml.Not_found) {
					v;
				}
				_e.eexpr = TField(f(e1), v);
				_e.etype = ft(e.etype);
			case TParenthesis(e1):
				_e.eexpr = TParenthesis(f(e1));
				_e.etype = ft(e.etype);
			case TUnop(op, pre, e1):
				_e.eexpr = TUnop(op, pre, f(e1));
				_e.etype = ft(e.etype);
			case TArrayDecl(el):
				_e.eexpr = TArrayDecl(el.map(f));
				_e.etype = ft(e.etype);
			case TNew(c, pl, el):
				var et = ft(e.etype);
				// make sure that we use the class corresponding to the replaced type
				var t = switch (c.cl_kind) {
					case KTypeParameter(_), KGeneric: et;
					case _: ft(TInst(c, pl));
				}
				var c = null; var pl = null;
				var _tmp = switch (follow(t)) {
					case TInst(_c, _pl):
						c = _c; pl = _pl;
					case TAbstract({a_impl:Some(_c)}, _pl):
						c = _c; pl = _pl;
					case t: error([has_no_field(t, "new")]);
				}
				_e.eexpr = TNew(c, pl, el.map(f));
				_e.etype = ft(e.etype);
			case TBlock(el):
				_e.eexpr = TBlock(el.map(f));
				_e.etype = ft(e.etype);
			case TObjectDecl(el):
				_e.eexpr = TObjectDecl(el.map(function (a) {return {a:a.a, expr:f(a.expr)};}));
				_e.etype = ft(e.etype);
			case TCall(e1, el):
				var e1 = f(e1);
				_e.eexpr = TCall(e1, el.map(f));
				_e.etype = ft(e.etype);
			case TVar(v, eo):
				_e.eexpr = TVar(fv(v), switch (eo) {
					case None:None;
					case Some(e): Some(f(e));
				});
				_e.etype = ft(e.etype);
			case TFunction (fu):
				var _fu = {
					tf_expr: f(fu.tf_expr),
					tf_args: fu.tf_args.map(function (a) { return {v:fv(a.v), c:a.c};}),
					tf_type: ft(fu.tf_type)
				}
				_e.eexpr = TFunction(_fu);
				_e.etype = ft(e.etype);
			case TIf(ec, e1, e2):
				var ec = f(ec);
				var e1 = f(e1);
				_e.eexpr = TIf(ec, e1, switch (e2) {
					case None: None;
					case Some(e): Some(f(e));
				});
				_e.etype = ft(e.etype);
			case TSwitch (e1, cases, def):
				var e1 = f(e1);
				var cases = cases.map(function (c) { return {values:c.values.map(f), e:f(c.e)}});
				_e.eexpr = TSwitch(e1, cases, switch (def) {
					case None: None;
					case Some(e): Some(f(e));
				});
				_e.etype = ft(e.etype);
			case TTry (e1, catches):
				var e1 = f(e1);
				_e.eexpr = TTry(e1, catches.map(function (c) { return {v:fv(c.v), e:f(c.e)};}));
				_e.etype = ft(e.etype);
			case TReturn (eo):
				_e.eexpr = TReturn (switch (eo) {
					case None: None;
					case Some(e): Some(f(e));
				});
				_e.etype = ft(e.etype);
			case TCast (e1, t):
				_e.eexpr = TCast(f(e1),t);
				_e.etype = ft(e.etype);
			case TMeta (m, e1):
				_e.eexpr = TMeta(m, f(e1));
				_e.etype = ft(e.etype);
		}
		return _e;
	}

	public static function resolve_typedef (t:core.Type.ModuleType) {
		return switch (t) {
			case TClassDecl(_), TEnumDecl(_), TAbstractDecl(_): t;
			case TTypeDecl(td):
				switch (follow(td.t_type)) {
					case TEnum(e,_): TEnumDecl(e);
					case TInst(c,_): TClassDecl(c);
					case TAbstract(a, _): TAbstractDecl(a);
					case _: t;
				}
		}
	}

	// module TExprToExpr = struct ...
	// module StringError = struct ...

	public static function enum_module_type (m:ModuleDef, path:core.Path, p:core.Globals.Pos) : TDef {
		return {
			t_path: new core.Path([], "Enum<" + core.Globals.s_type_path(path) + ">"),
			t_module: m,
			t_pos: p,
			t_name_pos: core.Globals.null_pos,
			t_private: true,
			t_doc: None,
			t_type: mk_mono(),
			t_params: [],
			t_meta: []
		}
	}
}