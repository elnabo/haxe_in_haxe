package context.common.identifiertype;

enum T {
	ITLocal (v:core.Type.TVar);
	ITMember (c:core.Type.TClass, field:core.Type.TClassField);
	ITStatic (c:core.Type.TClass, field:core.Type.TClassField);
	ITEnum (e:core.Type.TEnum, field:core.Type.TEnumField);
	ITEnumAbstract (a:core.Type.TAbstract, field:core.Type.TClassField);
	ITGlobal (mt:core.Type.ModuleType, s:String, t:core.Type.T);
	ITType (mt:core.Type.ModuleType);
	ITPackage (s:String);
	ITLiteral (s:String);
	ITTimer (s:String);
}