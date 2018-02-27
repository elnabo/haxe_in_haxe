package typing.matcher.constructor;

import haxe.ds.ImmutableList;

enum T {
	ConConst(c:core.Type.TConstant);
	ConEnum(e:core.Type.TEnum, ef:core.Type.TEnumField);
	ConStatic(c:core.Type.TClass, cf:core.Type.TClassField);
	ConTypeExpr(mt:core.Type.ModuleType);
	ConFields(l:ImmutableList<String>);
	ConArray(i:Int);
}