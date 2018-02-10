package core;

import haxe.ds.Option;

enum StrictMeta {
	Abi;
	Abstract;
	Access;
	Accessor;
	Allow;
	Analyzer;
	Annotation;
	ArrayAccess;
	Ast;
	AstSource;
	AutoBuild;
	Bind;
	Bitmap;
	BridgeProperties;
	Build;
	BuildXml;
	Callable;
	Class;
	ClassCode;
	Commutative;
	CompilerGenerated;
	Const;
	CoreApi;
	CoreType;
	CppFileCode;
	CppInclude;
	CppNamespaceCode;
	CsNative;
	Dce;
	Debug;
	Decl;
	DefParam;
	Delegate;
	Depend;
	Deprecated;
	DirectlyUsed;
	DynamicObject;
	Eager;
	Enum;
	EnumConstructorParam;
	Event;
	Exhaustive;
	Expose;
	Extern;
	FakeEnum;
	File;
	FileXml;
	Final;
	Fixed;
	FlatEnum;
	Font;
	ForLoopVariable;
	Forward;
	ForwardStatics;
	From;
	FunctionCode;
	FunctionTailCode;
	Generic;
	GenericBuild;
	GenericInstance;
	Getter;
	Hack;
	HasUntyped;
	HaxeGeneric;
	HeaderClassCode;
	HeaderCode;
	HeaderInclude;
	HeaderNamespaceCode;
	HxGen;
	IfFeature;
	Impl;
	PythonImport;
	ImplicitCast;
	Include;
	InitPackage;
	InlineConstructorArgument(a:Int, b:Int);
	InlineConstructorVariable;
	Internal;
	IsVar;
	JavaCanonical;
	JavaNative;
	JsRequire;
	Keep;
	KeepInit;
	KeepSub;
	LibType;
	LoopLabel;
	LuaRequire;
	LuaDotMethod;
	Meta;
	Macro;
	MaybeUsed;
	MergeBlock;
	MultiReturn;
	MultiType;
	Native;
	NativeChildren;
	NativeGen;
	NativeGeneric;
	NativeProperty;
	NativeStaticExtension;
	NoCompletion;
	NoDebug;
	NoDoc;
	NoExpr;
	NoImportGlobal;
	NonVirtual;
	NoPackageRestrict;
	NoPrivateAccess;
	NoStack;
	NotNull;
	NoUsing;
	Ns;
	Objc;
	ObjcProtocol;
	Op;
	Optional;
	Overload;
	PhpGlobal;
	PhpClassConst;
	PhpMagic;
	PhpNoConstructor;
	Pos;
	PrivateAccess;
	Property;
	Protected;
	Public;
	PublicFields;
	Pure;
	ReadOnly;
	RealPath;
	Remove;
	Require;
	RequiresAssign;
	Resolve;
	Rtti;
	Runtime;
	RuntimeValue;
	Scalar;
	SelfCall;
	Setter;
	SkipCtor;
	SkipReflection;
	Sound;
	SourceFile;
	StackOnly;
	StoredTypedExpr;
	Strict;
	Struct;
	StructAccess;
	StructInit;
	SuppressWarnings;
	This;
	Throws;
	To;
	ToString;
	Transient;
	TemplatedCall;
	ValueUsed;
	Volatile;
	UnifyMinDynamic;
	Unreflective;
	Unsafe;
	Usage;
	Used;
	UserVariable;
	Value;
	Void;
	Last;
	// do not put any custom metadata after Last
	Dollar (s:String);
	Custom (s:String);
}

enum MetaUsage {
	TClass;
	TClassField;
	TAbstract;
	TAbstractField;
	TEnum;
	TTypedef;
	TAnyField;
	TExpr;
	TTypeParameter;
}

enum MetaParameter {
	HasParam (s:String);
	Platform (pf:core.Globals.Platform);
	Platforms (l:Array<core.Globals.Platform>);
	UsedOn (mu:MetaUsage);
	UsedOnEither (l:Array<MetaUsage>);
	UsedInternally;
}


class Meta {

	public static function string_to_meta (s:String) : StrictMeta {
		return switch (s) {
			case ":abi": Abi;
			case ":abstract": Abstract;
			case ":access": Access;
			case ":accessor": Accessor;
			case ":allow": Allow;
			case ":analyzer": Analyzer;
			case ":annotation": Annotation;
			case ":arrayAccess": ArrayAccess;
			case ":ast": Ast;
			case ":astSource": AstSource;
			case ":autoBuild": AutoBuild;
			case ":bind": Bind;
			case ":bitmap": Bitmap;
			case ":bridgeProperties": BridgeProperties;
			case ":build": Build;
			case ":buildXml": BuildXml;
			case ":callable": Callable;
			case ":class": Class;
			case ":classCode": ClassCode;
			case ":commutative": Commutative;
			case ":compilerGenerated": CompilerGenerated;
			case ":const": Const;
			case ":coreApi": CoreApi;
			case ":coreType": CoreType;
			case ":cppFileCode": CppFileCode;
			case ":CppInclude": CppInclude;
			case ":cppNamespaceCode": CppNamespaceCode;
			case ":csNative": CsNative;
			case ":dce": Dce;
			case ":debug": Debug;
			case ":decl": Decl;
			case ":defParam": DefParam;
			case ":delegate": Delegate;
			case ":depend": Depend;
			case ":deprecated": Deprecated;
			case ":directlyUsed": DirectlyUsed;
			case ":dynamicObject": DynamicObject;
			case ":eager": Eager;
			case ":enum": Enum;
			case ":enumConstructorParam": EnumConstructorParam;
			case ":event": Event;
			case ":exhaustive": Exhaustive;
			case ":expose": Expose;
			case ":extern": Extern;
			case ":fakeEnum": FakeEnum;
			case ":file": File;
			case ":fileXml": FileXml;
			case ":final" : Final;
			case ":fixed": Fixed;
			case ":flatEnum": FlatEnum;
			case ":font": Font;
			case ":forLoopVariable": ForLoopVariable;
			case ":forward": Forward;
			case ":forwardStatics": ForwardStatics;
			case ":from": From;
			case ":functionCode": FunctionCode;
			case ":functionTrailCode": FunctionTailCode;
			case ":generic": Generic;
			case ":genericBuild": GenericBuild;
			case ":genericInstance": GenericInstance;
			case ":getter": Getter;
			case ":hack": Hack;
			case ":hasUntyped": HasUntyped;
			case ":haxeGeneric": HaxeGeneric;
			case ":headerClassCode": HeaderClassCode;
			case ":headerCode": HeaderCode;
			case ":headerInclude": HeaderInclude;
			case ":headerNamespaceCode": HeaderNamespaceCode;
			case ":hxGen": HxGen;
			case ":ifFeature": IfFeature;
			case ":impl": Impl;
			case ":pythonImport": PythonImport;
			case ":implicitCast": ImplicitCast;
			case ":include": Include;
			case ":initPackage": InitPackage;
			case ":inlineConstructorArgument": InlineConstructorArgument(0, 0);
			case ":inlineConstructorVariable": InlineConstructorVariable;
			case ":internal": Internal;
			case ":isVar": IsVar;
			case ":javaCanonical": JavaCanonical;
			case ":javaNative": JavaNative;
			case ":jsRequire": JsRequire;
			case ":luaRequire": LuaRequire;
			case ":luaDotMethod": LuaDotMethod;
			case ":keep": Keep;
			case ":keepInit": KeepInit;
			case ":keepSub": KeepSub;
			case ":libType": LibType;
			case ":loopLabel": LoopLabel;
			case ":meta": Meta;
			case ":macro": Macro;
			case ":maybeUsed": MaybeUsed;
			case ":mergeBlock": MergeBlock;
			case ":multiReturn": MultiReturn;
			case ":multiType": MultiType;
			case ":native": Native;
			case ":nativeChildren": NativeChildren;
			case ":nativeGen": NativeGen;
			case ":nativeProperty": NativeProperty;
			case ":nativeStaticExtension": NativeStaticExtension;
			case ":noCompletion": NoCompletion;
			case ":noDebug": NoDebug;
			case ":noDoc": NoDoc;
			case ":noExpr": NoExpr;
			case ":noImportGlobal": NoImportGlobal;
			case ":nonVirtual": NonVirtual;
			case ":noPackageRestrict": NoPackageRestrict;
			case ":noPrivateAccess": NoPrivateAccess;
			case ":noStack": NoStack;
			case ":notNull": NotNull;
			case ":noUsing": NoUsing;
			case ":ns": Ns;
			case ":objc": Objc;
			case ":objcProtocol": ObjcProtocol;
			case ":op": Op;
			case ":optional": Optional;
			case ":overload": Overload;
			case ":phpGlobal": PhpGlobal;
			case ":phpClassConst": PhpClassConst;
			case ":phpMagic": PhpMagic;
			case ":phpNoConstructor": PhpNoConstructor;
			case ":pos": Pos;
			case ":public": Public;
			case ":publicFields": PublicFields;
			case ":privateAccess": PrivateAccess;
			case ":protected": Protected;
			case ":property": Property;
			case ":pure": Pure;
			case ":readOnly": ReadOnly;
			case ":realPath": RealPath;
			case ":remove": Remove;
			case ":require": Require;
			case ":requiresAssign": RequiresAssign;
			case ":resolve": Resolve;
			case ":rtti": Rtti;
			case ":runtime": Runtime;
			case ":runtimeValue": RuntimeValue;
			case ":scalar": Scalar;
			case ":selfCall": SelfCall;
			case ":setter": Setter;
			case ":stackOnly": StackOnly;
			case ":storedTypedExpr": StoredTypedExpr;
			case ":skipCtor": SkipCtor;
			case ":skipReflection": SkipReflection;
			case ":sound": Sound;
			case ":sourceFile": SourceFile;
			case ":strict": Strict;
			case ":struct": Struct;
			case ":structAccess": StructAccess;
			case ":structInit": StructInit;
			case ":suppressWarnings": SuppressWarnings;
			case ":templatedCall": TemplatedCall;
			case ":throws": Throws;
			case ":this": This;
			case ":to": To;
			case ":toString": ToString;
			case ":transient": Transient;
			case ":valueUsed": ValueUsed;
			case ":volatile": Volatile;
			case ":unifyMinDynamic": UnifyMinDynamic;
			case ":unreflective": Unreflective;
			case ":unsafe": Unsafe;
			case ":usage": Usage;
			case ":used": Used;
			case ":userVariable": UserVariable;
			case ":value": Value;
			case ":void": Void;
			default:
				if (s.length > 0 && s.charAt(0) == "$") {
					Dollar(s.substr(1));
				}
				else {
					Custom(s);
				}
		};
	}

	public static function to_string (m:StrictMeta) : String {
		return get_info(m).a;
	}

	public static function has(m:StrictMeta, ml:core.Ast.Metadata) : Bool {
		for (entry in ml) {
			if (entry.name == m) {
				return true;
			}
		}
		return false;
	}
	
	public static function get(m:StrictMeta, ml:core.Ast.Metadata) : core.Ast.MetadataEntry {
		for (entry in ml) {
			if (entry.name == m) {
				return entry;
			}
		}
		throw ocaml.Not_found.instance;
	}

	public static function get_info(m:StrictMeta) : {a:String, b:{a:String, b:Array<MetaParameter>}} {
		return switch (m) {
			case Abi:
				{a:":abi", b:{a:"Function ABI/calling convention",b: [Platforms([Cpp])]}};
			case Abstract:
				{a:":abstract", b:{a:"Sets the underlying class implementation as 'abstract'", b:[Platforms([Java,Cs])]}};
			case Access:
				{a:":access", b:{a:"Forces private access to package, type or field", b:[HasParam("Target path"), UsedOnEither([TClass,TClassField])]}};
			case Accessor:
				{a:":accessor", b:{a:"Used internally by DCE to mark property accessors", b:[UsedOn(TClassField), UsedInternally]}};
			case Allow:
				{a:":allow", b:{a:"Allows private access from package, type or field", b:[HasParam("Target path"), UsedOnEither([TClass,TClassField])]}};
			case Analyzer:
				{a:":analyzer", b:{a:"Used to configure the static analyzer", b:[]}};
			case Annotation:
				{a:":annotation", b:{a:"Annotation (@interface) definitions on -java-lib imports will be annotated with this metadata. Has no effect on types compiled by Haxe", b:[Platform(Java), UsedOn(TClass)]}};
			case ArrayAccess:
				{a:":arrayAccess", b:{a:"Allows [] access on an abstract", b:[UsedOnEither([TAbstract,TAbstractField])]}};
			case Ast:
				{a:":ast", b:{a:"Internally used to pass the AST source into the typed AST", b:[UsedInternally]}};
			case AstSource:
				{a:":astSource", b:{a:"Filled by the compiler with the parsed expression of the field", b:[UsedOn(TClassField)]}};
			case AutoBuild:
				{a:":autoBuild", b:{a:"Extends @:build metadata to all extending and implementing classes", b:[HasParam("Build macro call"), UsedOn(TClass)]}};
			case Bind:
				{a:":bind", b:{a:"Override Swf class declaration", b:[Platform(Flash), UsedOn(TClass)]}};
			case Bitmap:
				{a:":bitmap", b:{a:"Embeds given bitmap data into the class (must extend flash.display.BitmapData)", b:[HasParam("Bitmap file path"), UsedOn(TClass), Platform(Flash)]}};
			case BridgeProperties:
				{a:":bridgeProperties", b:{a:"Creates native property bridges for all Haxe properties in this class", b:[UsedOn(TClass),Platform(Cs)]}};
			case Build:
				{a:":build", b:{a:"Builds a class or enum from a macro", b:[HasParam("Build macro call"), UsedOnEither([TClass,TEnum])]}};
			case BuildXml:
				{a:":buildXml", b:{a:"Specify xml data to be injected into Build.xml", b:[Platform(Cpp)]}};
			case Callable:
				{a:":callable", b:{a:"Abstract forwards call to its underlying type", b:[UsedOn(TAbstract)]}};
			case Class:
				{a:":class", b:{a:"Used internally to annotate an enum that will be generated as a class", b:[Platforms([Java,Cs]), UsedOn(TEnum), UsedInternally]}};
			case ClassCode:
				{a:":classCode", b:{a:"Used to inject platform-native code into a class", b:[Platforms([Java,Cs]), UsedOn(TClass)]}};
			case Commutative:
				{a:":commutative", b:{a:"Declares an abstract operator as commutative", b:[UsedOn(TAbstractField)]}};
			case CompilerGenerated:
				{a:":compilerGenerated", b:{a:"Marks a field as generated by the compiler. Shouldn't be used by the end user", b:[Platforms([Java,Cs])]}};
			case Const:
				{a:":const", b:{a:"Allows a type parameter to accept expression values", b:[UsedOn(TTypeParameter)]}};
			case CoreApi:
				{a:":coreApi", b:{a:"Identifies this class as a core api class (forces Api check)", b:[UsedOnEither([TClass,TEnum,TTypedef,TAbstract])]}};
			case CoreType:
				{a:":coreType", b:{a:"Identifies an abstract as core type so that it requires no implementation", b:[UsedOn(TAbstract)]}};
			case CppFileCode:
				{a:":cppFileCode", b:{a:"Code to be injected into generated cpp file", b:[Platform(Cpp)]}};
			case CppInclude:
				{a:":cppInclude", b:{a:"File to be included in generated cpp file", b:[Platform(Cpp)]}};
			case CppNamespaceCode:
				{a:":cppNamespaceCode", b:{a:"", b:[Platform(Cpp)]}};
			case CsNative:
				{a:":csNative", b:{a:"Automatically added by -net-lib on classes generated from .NET DLL files", b:[Platform(Cs), UsedOnEither([TClass,TEnum]), UsedInternally]}};
			case Dce:
				{a:":dce", b:{a:"Forces dead code elimination even when -dce full is not specified", b:[UsedOnEither([TClass,TEnum])]}};
			case Debug:
				{a:":debug", b:{a:"Forces debug information to be generated into the Swf even without -debug", b:[UsedOnEither([TClass,TClassField]), Platform(Flash)]}};
			case Decl:
				{a:":decl", b:{a:"", b:[Platform(Cpp)]}};
			case DefParam:
				{a:":defParam", b:{a:"Default function argument value loaded from the SWF and used for documentation in Genxml", b:[Platform(Flash),UsedInternally]}};
			case Delegate:
				{a:":delegate", b:{a:"Automatically added by -net-lib on delegates", b:[Platform(Cs), UsedOn(TAbstract)]}};
			case Depend:
				{a:":depend", b:{a:"", b:[Platform(Cpp)]}};
			case Deprecated:
				{a:":deprecated", b:{a:"Mark a type or field as deprecated", b:[]}};
			case DirectlyUsed:
				{a:":directlyUsed", b:{a:"Marks types that are directly referenced by non-extern code", b:[UsedInternally]}};
			case DynamicObject:
				{a:":dynamicObject", b:{a:"Used internally to identify the Dynamic Object implementation", b:[Platforms([Java,Cs]), UsedOn(TClass), UsedInternally]}};
			case Eager:
				{a:":eager", b:{a:"Forces typedefs to be followed early", b:[UsedOn(TTypedef)]}};
			case Enum:
				{a:":enum", b:{a:"Defines finite value sets to abstract definitions", b:[UsedOn(TAbstract)]}};
			case EnumConstructorParam:
				{a:":enumConstructorParam", b:{a:"Used internally to annotate GADT type parameters", b:[UsedOn(TClass), UsedInternally]}};
			case Event:
				{a:":event", b:{a:"Automatically added by -net-lib on events. Has no effect on types compiled by Haxe", b:[Platform(Cs), UsedOn(TClassField)]}};
			case Exhaustive:
				{a:":exhaustive", b:{a:"", b:[UsedInternally]}};
			case Expose:
				{a:":expose", b:{a:"Includes the class or field in Haxe exports", b:[HasParam("?Name=Class path"),UsedOnEither([TClass,TClassField]),Platforms([Js,Lua])]}};
			case Extern:
				{a:":extern", b:{a:"Marks the field as extern so it is not generated", b:[UsedOn(TClassField)]}};
			case FakeEnum:
				{a:":fakeEnum", b:{a:"Treat enum as collection of values of the specified type", b:[HasParam("Type name"), UsedOn(TEnum)]}};
			case File:
				{a:":file", b:{a:"Includes a given binary file into the target Swf and associates it with the class (must extend flash.utils.ByteArray)", b:[HasParam("File path"), UsedOn(TClass), Platform(Flash)]}};
			case FileXml:
				{a:":fileXml", b:{a:"Include xml attribute snippet in Build.xml entry for file", b:[UsedOn(TClass), Platform(Cpp)]}};
			case Final:
				{a:":final", b:{a:"Prevents a class from being extended", b:[UsedOn(TClass)]}};
			case Fixed:
				{a:":fixed", b:{a:"Delcares an anonymous object to have fixed fields", b:[]}}; // {b:{b:[UsedOn(TObjectDecl(_)]}}
			case FlatEnum:
				{a:":flatEnum", b:{a:"Internally used to mark an enum as being flat, i.e. having no function constructors", b:[UsedOn(TEnum), UsedInternally]}};
			case Font:
				{a:":font", b:{a:"Embeds the given TrueType font into the class (must extend flash.text.Font)", b:[HasParam("TTF path"), HasParam("Range String"), UsedOn(TClass)]}};
			case ForLoopVariable:
				{a:":forLoopVariable", b:{a:"Internally used to mark for-loop variables", b:[UsedInternally]}};
			case Forward:
				{a:":forward", b:{a:"Forwards field access to underlying type", b:[HasParam("List of field names"), UsedOn(TAbstract)]}};
			case ForwardStatics:
				{a:":forwardStatics", b:{a:"Forwards static field access to underlying type", b:[HasParam("List of field names"), UsedOn(TAbstract)]}};
			case From:
				{a:":from", b:{a:"Specifies that the field of the abstract is a cast operation from the type identified in the function", b:[UsedOn(TAbstractField)]}};
			case FunctionCode:
				{a:":functionCode", b:{a:"Used to inject platform-native code into a function", b:[Platforms([Cpp,Java,Cs])]}};
			case FunctionTailCode:
				{a:":functionTailCode", b:{a:"", b:[Platform(Cpp)]}};
			case Generic:
				{a:":generic", b:{a:"Marks a class or class field as generic so each type parameter combination generates its own type/field", b:[UsedOnEither([TClass,TClassField])]}};
			case GenericBuild:
				{a:":genericBuild", b:{a:"Builds instances of a type using the specified macro", b:[UsedOn(TClass)]}};
			case GenericInstance:
				{a:":genericInstance", b:{a:"Internally used to mark instances of @:generic methods", b:[UsedOn(TClassField),UsedInternally]}};
			case Getter:
				{a:":getter", b:{a:"Generates a native getter function on the given field", b:[HasParam("Class field name"), UsedOn(TClassField), Platform(Flash)]}};
			case Hack:
				{a:":hack", b:{a:"Allows extending classes marked as @:final", b:[UsedOn(TClass)]}};
			case HasUntyped:
				{a:":has_untyped", b:{a:"Used by the typer to mark fields that have untyped expressions", b:[UsedInternally]}};
			case HaxeGeneric:
				{a:":haxeGeneric", b:{a:"Used internally to annotate non-native generic classes", b:[Platform(Cs), UsedOnEither([TClass,TEnum]), UsedInternally]}};
			case HeaderClassCode:
				{a:":headerClassCode", b:{a:"Code to be injected into the generated class, in the header", b:[Platform(Cpp)]}};
			case HeaderCode:
				{a:":headerCode", b:{a:"Code to be injected into the generated header file", b:[Platform(Cpp)]}};
			case HeaderInclude:
				{a:":headerInclude", b:{a:"File to be included in generated header file", b:[Platform(Cpp)]}};
			case HeaderNamespaceCode:
				{a:":headerNamespaceCode", b:{a:"", b:[Platform(Cpp)]}};
			case HxGen:
				{a:":hxGen", b:{a:"Annotates that an extern class was generated by Haxe", b:[Platforms([Java,Cs]), UsedOnEither([TClass,TEnum])]}};
			case IfFeature:
				{a:":ifFeature", b:{a:"Causes a field to be kept by DCE if the given feature is part of the compilation", b:[HasParam("Feature name"), UsedOn(TClassField)]}};
			case Impl:
				{a:":impl", b:{a:"Used internally to mark abstract implementation fields", b:[UsedOn(TAbstractField), UsedInternally]}};
			case PythonImport:
				{a:":pythonImport", b:{a:"Generates python import statement for extern classes", b:[Platforms([Python]), UsedOn(TClass)]}};
			case ImplicitCast:
				{a:":implicitCast", b:{a:"Generated automatically on the AST when an implicit abstract cast happens", b:[UsedInternally, UsedOn(TExpr)]}};
			case Include:
				{a:":include", b:{a:"", b:[Platform(Cpp)]}};
			case InitPackage:
				{a:":initPackage", b:{a:"Some weird thing for Genjs we want to remove someday", b:[UsedInternally, Platform(Js)]}};
			case InlineConstructorArgument(_):
				{a:":inlineConstructorArgument", b:{a:"Internally used to mark expressions that were passed as arguments of an inlined constructor", b:[UsedInternally]}};
			case InlineConstructorVariable:
				{a:":inlineConstructorVariable", b:{a:"Internally used to mark variables that come from inlined constructors", b:[UsedInternally]}};
			case Internal:
				{a:":internal", b:{a:"Generates the annotated field/class with 'internal' access", b:[Platforms([Java,Cs]), UsedOnEither([TClass,TEnum,TClassField])]}};
			case IsVar:
				{a:":isVar", b:{a:"Forces a physical field to be generated for properties that otherwise would not require one", b:[UsedOn(TClassField)]}};
			case JavaCanonical:
				{a:":javaCanonical", b:{a:"Used by the Java target to annotate the canonical path of the type", b:[HasParam("Output type package"), HasParam("Output type name"), UsedOnEither([TClass,TEnum]), Platform(Java)]}};
			case JavaNative:
				{a:":javaNative", b:{a:"Automatically added by -java-lib on classes generated from JAR/class files", b:[Platform(Java), UsedOnEither([TClass,TEnum]), UsedInternally]}};
			case JsRequire:
				{a:":jsRequire", b:{a:"Generate javascript module require expression for given extern", b:[Platform(Js), UsedOn(TClass)]}};
			case LuaRequire:
				{a:":luaRequire", b:{a:"Generate lua module require expression for given extern", b:[Platform(Lua), UsedOn(TClass)]}};
			case LuaDotMethod:
				{a:":luaDotMethod", b:{a:"Indicates that the given extern type instance should have dot-style invocation for methods instead of colon.", b:[Platform(Lua), UsedOnEither([TClass,TClassField])]}};
			case Keep:
				{a:":keep", b:{a:"Causes a field or type to be kept by DCE", b:[]}};
			case KeepInit:
				{a:":keepInit", b:{a:"Causes a class to be kept by DCE even if all its field are removed", b:[UsedOn(TClass)]}};
			case KeepSub:
				{a:":keepSub", b:{a:"Extends @:keep metadata to all implementing and extending classes", b:[UsedOn(TClass)]}};
			case LibType:
				{a:":libType", b:{a:"Used by -net-lib and -java-lib to mark a class that shouldn't be checked (overrides, interfaces, etc) by the type loader", b:[UsedInternally, UsedOn(TClass), Platforms([Java,Cs])]}};
			case LoopLabel:
				{a:":loopLabel", b:{a:"Mark loop and break expressions with a label to support breaking from within switch", b:[UsedInternally]}};
			case Meta:
				{a:":meta", b:{a:"Internally used to mark a class field as being the metadata field", b:[]}};
			case Macro:
				{a:":macro", b:{a:"(deprecated)", b:[]}};
			case MaybeUsed:
				{a:":maybeUsed", b:{a:"Internally used by DCE to mark fields that might be kept", b:[UsedInternally]}};
			case MergeBlock:
				{a:":mergeBlock", b:{a:"Merge the annotated block into the current scope", b:[UsedOn(TExpr)]}};
			case MultiReturn:
				{a:":multiReturn", b:{a:"Annotates an extern class as the result of multi-return function", b:[UsedOn(TClass), Platform(Lua)]}};
			case MultiType:
				{a:":multiType", b:{a:"Specifies that an abstract chooses its this-type from its @:to functions", b:[UsedOn(TAbstract), HasParam("Relevant type parameters")]}};
			case Native:
				{a:":native", b:{a:"Rewrites the path of a class or enum during generation", b:[HasParam("Output type path"), UsedOnEither([TClass,TEnum])]}};
			case NativeChildren:
				{a:":nativeChildren", b:{a:"Annotates that all children from a type should be treated as if it were an extern definition - platform native", b:[Platforms([Java,Cs]), UsedOn(TClass)]}};
			case NativeGen:
				{a:":nativeGen", b:{a:"Annotates that a type should be treated as if it were an extern definition - platform native", b:[Platforms([Java,Cs,Python]), UsedOnEither([TClass,TEnum])]}};
			case NativeGeneric:
				{a:":nativeGeneric", b:{a:"Used internally to annotate native generic classes", b:[Platform(Cs), UsedOnEither([TClass,TEnum]), UsedInternally]}};
			case NativeProperty:
				{a:":nativeProperty", b:{a:"Use native properties which will execute even with dynamic usage", b:[Platform(Cpp)]}};
			case NativeStaticExtension:
				{a:":nativeStaticExtension", b:{a:"Converts static function syntax into member call", b:[Platform(Cpp)]}};
			case NoCompletion:
				{a:":noCompletion", b:{a:"Prevents the compiler from suggesting completion on this field", b:[UsedOn(TClassField)]}};
			case NoDebug:
				{a:":noDebug", b:{a:"Does not generate debug information into the Swf even if -debug is set", b:[UsedOnEither([TClass,TClassField]),Platform(Flash)]}};
			case NoDoc:
				{a:":noDoc", b:{a:"Prevents a type from being included in documentation generation", b:[]}};
			case NoExpr:
				{a:":noExpr", b:{a:"Internally used to mark abstract fields which have no expression by design", b:[UsedInternally]}};
			case NoImportGlobal:
				{a:":noImportGlobal", b:{a:"Prevents a static field from being imported with import Class.*", b:[UsedOn(TAnyField)]}};
			case NonVirtual:
				{a:":nonVirtual", b:{a:"Declares function to be non-virtual in cpp", b:[Platform(Cpp)]}};
			case NoPackageRestrict:
				{a:":noPackageRestrict", b:{a:"Allows a module to be accessed across all targets if found on its first type", b:[UsedInternally]}};
			case NoPrivateAccess:
				{a:":noPrivateAccess", b:{a:"Disallow private access to anything for the annotated expression", b:[UsedOn(TExpr)]}};
			case NoStack:
				{a:":noStack", b:{a:"", b:[Platform(Cpp)]}};
			case NotNull:
				{a:":notNull", b:{a:"Declares an abstract type as not accepting null values", b:[UsedOn(TAbstract)]}};
			case NoUsing:
				{a:":noUsing", b:{a:"Prevents a field from being used with 'using'", b:[UsedOn(TClassField)]}};
			case Ns:
				{a:":ns", b:{a:"Internally used by the Swf generator to handle namespaces", b:[Platform(Flash)]}};
			case Objc:
				{a:":objc", b:{a:"Declares a class or interface that is used to interoperate with Objective-C code", b:[Platform(Cpp), UsedOn(TClass)]}};
			case ObjcProtocol:
				{a:":objcProtocol", b:{a:"Associates an interface with, or describes a function in, a native Objective-C protocol.", b:[Platform(Cpp), UsedOnEither([TClass,TClassField]) ]}};
			case Op:
				{a:":op", b:{a:"Declares an abstract field as being an operator overload", b:[HasParam("The operation"), UsedOn(TAbstractField)]}};
			case Optional:
				{a:":optional", b:{a:"Marks the field of a structure as optional", b:[UsedOn(TClassField)]}};
			case Overload:
				{a:":overload", b:{a:"Allows the field to be called with different argument types", b:[HasParam("Function specification (no expression)"), UsedOn(TClassField)]}};
			case PhpGlobal:
				{a:":phpGlobal", b:{a:"Indicates that static fields of an extern class actually are located in the global PHP namespace", b:[Platform(Php), UsedOn(TClass)]}};
			case PhpClassConst:
				{a:":phpClassConst", b:{a:"Indicates that a static var of an extern class is a PHP class constant", b:[Platform(Php), UsedOn(TClassField)]}};
			case PhpMagic:
				{a:":phpMagic", b:{a:"Treat annotated field as special PHP magic field. This meta makes compiler avoid renaming such fields on generating PHP code.", b:[Platform(Php), UsedOn(TClassField)]}};
			case PhpNoConstructor:
				{a:":phpNoConstructor", b:{a:"Special meta for extern classes which do not have native constructor in PHP, but need a constructor in Haxe extern", b:[Platform(Php), UsedOn(TClass)]}};
			case Pos:
				{a:":pos", b:{a:"Sets the position of a reified expression", b:[HasParam("Position"), UsedOn(TExpr)]}};
			case Public:
				{a:":public", b:{a:"Marks a class field as being public", b:[UsedOn(TClassField), UsedInternally]}};
			case PublicFields:
				{a:":publicFields", b:{a:"Forces all class fields of inheriting classes to be public", b:[UsedOn(TClass)]}};
			case PrivateAccess:
				{a:":privateAccess", b:{a:"Allow private access to anything for the annotated expression", b:[UsedOn(TExpr)]}};
			case Protected:
				{a:":protected", b:{a:"Marks a class field as being protected", b:[UsedOn(TClassField), Platforms([Cs,Java,Flash])]}};
			case Property:
				{a:":property", b:{a:"Marks a property field to be compiled as a native C# property", b:[UsedOn(TClassField), Platform(Cs)]}};
			case Pure:
				{a:":pure", b:{a:"Marks a class field, class or expression as pure (side-effect free)", b:[UsedOnEither([TClass,TClassField,TExpr])]}};
			case ReadOnly:
				{a:":readOnly", b:{a:"Generates a field with the 'readonly' native keyword", b:[Platform(Cs), UsedOn(TClassField)]}};
			case RealPath:
				{a:":realPath", b:{a:"Internally used on @:native types to retain original path information", b:[UsedInternally]}};
			case Remove:
				{a:":remove", b:{a:"Causes an interface to be removed from all implementing classes before generation", b:[UsedOn(TClass)]}};
			case Require:
				{a:":require", b:{a:"Allows access to a field only if the specified compiler flag is set", b:[HasParam("Compiler flag to check"), UsedOn(TClassField)]}};
			case RequiresAssign:
				{a:":requiresAssign", b:{a:"Used internally to mark certain abstract operator overloads", b:[UsedInternally]}};
			case Resolve:
				{a:":resolve", b:{a:"Abstract fields marked with this metadata can be used to resolve unknown fields", b:[UsedOn(TClassField)]}};
			case Rtti:
				{a:":rtti", b:{a:"Adds runtime type information", b:[UsedOn(TClass)]}};
			case Runtime:
				{a:":runtime", b:{a:"?", b:[]}};
			case RuntimeValue:
				{a:":runtimeValue", b:{a:"Marks an abstract as being a runtime value", b:[UsedOn(TAbstract)]}};
			case Scalar:
				{a:":scalar", b:{a:"Used by hxcpp to mark a custom coreType abstract", b:[UsedOn(TAbstract), Platform(Cpp)]}};
			case SelfCall:
				{a:":selfCall", b:{a:"Translates method calls into calling object directly", b:[UsedOn(TClassField), Platforms([Js,Lua])]}};
			case Setter:
				{a:":setter", b:{a:"Generates a native setter function on the given field", b:[HasParam("Class field name"), UsedOn(TClassField),Platform(Flash)]}};
			case StackOnly:
				{a:":stackOnly", b:{a:"Instances of this type can only appear on the stack", b:[Platform(Cpp)]}};
			case StoredTypedExpr:
				{a:":storedTypedExpr", b:{a:"Used internally to reference a typed expression returned from a macro", b:[UsedInternally]}};
			case SkipCtor:
				{a:":skipCtor", b:{a:"Used internally to generate a constructor as if it were a native type (no __hx_ctor)", b:[Platforms([Java,Cs]), UsedInternally]}};
			case SkipReflection:
				{a:":skipReflection", b:{a:"Used internally to annotate a field that shouldn't have its reflection data generated", b:[Platforms([Java,Cs]), UsedOn(TClassField), UsedInternally]}};
			case Sound:
				{a:":sound", b:{a: "Includes a given .wav or .mp3 file into the target Swf and associates it with the class (must extend flash.media.Sound)", b:[HasParam("File path"), UsedOn(TClass), Platform(Flash)]}};
			case SourceFile:
				{a:":sourceFile", b:{a:"Source code filename for external class", b:[Platform(Cpp)]}};
			case Strict:
				{a:":strict", b:{a:"Used to declare a native C# attribute or a native Java metadata. Is type checked", b:[Platforms([Java,Cs])]}};
			case Struct:
				{a:":struct", b:{a:"Marks a class definition as a struct", b:[Platform(Cs), UsedOn(TClass)]}};
			case StructAccess:
				{a:":structAccess", b:{a:"Marks an extern class as using struct access('.') not pointer('->')", b:[Platform(Cpp), UsedOn(TClass)]}};
			case StructInit:
				{a:":structInit", b:{a:"Allows one to initialize the class with a structure that matches constructor parameters", b:[UsedOn(TClass)]}};
			case SuppressWarnings:
				{a:":suppressWarnings", b:{a:"Adds a SuppressWarnings annotation for the generated Java class", b:[Platform(Java), UsedOn(TClass)]}};
			case TemplatedCall:
				{a:":templatedCall", b:{a:"Indicates that the first parameter of static call should be treated as a template argument", b:[Platform(Cpp), UsedOn(TClassField)]}};
			case Throws:
				{a:":throws", b:{a:"Adds a 'throws' declaration to the generated function", b:[HasParam("Type as String"), Platform(Java), UsedOn(TClassField)]}};
			case This:
				{a:":this", b:{a:"Internally used to pass a 'this' expression to macros", b:[UsedInternally, UsedOn(TExpr)]}};
			case To:
				{a:":to", b:{a:"Specifies that the field of the abstract is a cast operation to the type identified in the function", b:[UsedOn(TAbstractField)]}};
			case ToString:
				{a:":toString", b:{a:"Internally used", b:[UsedInternally]}};
			case Transient:
				{a:":transient", b:{a:"Adds the 'transient' flag to the class field", b:[Platform(Java), UsedOn(TClassField)]}};
			case ValueUsed:
				{a:":valueUsed", b:{a:"Internally used by DCE to mark an abstract value as used", b:[UsedInternally]}};
			case Volatile:
				{a:":volatile", b:{a:"", b:[Platforms([Java,Cs])]}};
			case UnifyMinDynamic:
				{a:":unifyMinDynamic", b:{a:"Allows a collection of types to unify to Dynamic", b:[UsedOn(TClassField)]}};
			case Unreflective:
				{a:":unreflective", b:{a:"", b:[Platform(Cpp)]}};
			case Unsafe:
				{a:":unsafe", b:{a:"Declares a class, or a method with the C#'s 'unsafe' flag", b:[Platform(Cs), UsedOnEither([TClass,TClassField])]}};
			case Usage:
				{a:":usage", b:{a:"Internal metadata used to mark a symbol for which usage request was invoked", b:[UsedInternally]}};
			case Used:
				{a:":used", b:{a:"Internally used by DCE to mark a class or field as used", b:[UsedInternally]}};
			case UserVariable:
				{a:":userVariable", b:{a:"Internally used to mark variables that come from user code", b:[UsedInternally]}};
			case Value:
				{a:":value", b:{a:"Used to store default values for fields and function arguments", b:[UsedOn(TClassField)]}};
			case Void:
				{a:":void", b:{a:"Use Cpp native 'void' return type", b:[Platform(Cpp)]}};
			case Last:
				throw false;
			// do not put any custom metadata after Last
			case Dollar(s):
				{a:"$" + s, b:{a:"", b:[]}};
			case Custom(s):
				{a:s, b:{a:"", b:[]}};
		}
	}

	public static function parse (s:String) {
		return string_to_meta(":"+s);
	}
	
	public static function get_documentation (d:StrictMeta) : Option<{a:String, b:String}> {
		var meta_info = get_info(d);
		var flags = meta_info.b.b;
		if (flags.indexOf(UsedInternally) != -1) {
			var params = [];
			var used = [];
			var pfs = [];
			for (value in flags) {
				switch (value) {
					case HasParam(s): params.unshift(s);
					case Platform(f): pfs.unshift(f);
					case Platforms(fl): pfs = fl.concat(pfs);
					case UsedOn(u): used.unshift(u);
					case UsedOnEither(ul): used = ul.concat(used);
					case UsedInternally: throw false;
				}
			}
			var sfp = pfs.copy();
			sfp.reverse();
			var string_params = (sfp.length == 0) ? "" : "(" + sfp.join(",") + ")";
			var string_pfs = core.Globals.platform_list_help(sfp);
			var str = "@" + meta_info.a;
			var doc = meta_info.b.a;
			return Some({a:str,b: string_params + doc + string_pfs});
		}
		return None;
	}

	public static function get_documentation_list () : {a:Array<{a:String,b:String}>, b:Int} {
		var m = 0;
		var acc = [];

		for (d in std.Type.allEnums(StrictMeta)) {
			switch (get_documentation(d)) {
				case None:
				case Some(smt):
					var str = smt.a;
					var desc = smt.b;
					if (str.length > m) {
						m = str.length;
					}
					acc.push({a:str, b:desc});
			}
		}

		haxe.ds.ArraySort.sort(acc, function (x:{a:String, b:String}, y:{a:String, b:String}) {
			if (x.a == y.a) { return 0;}
			if (x.a > y.a) { return 1;}
			return -1;
		});
		return {a:acc, b:m};
	}
}
