package typing.matcher.pattern;

import haxe.ds.ImmutableList;
import typing.matcher.Pattern;

enum T {
	PatConstructor(t:typing.matcher.Constructor, l:ImmutableList<Pattern>);
	PatVariable(v:core.Type.TVar);
	PatAny;
	PatBind(v:core.Type.TVar, p:Pattern);
	PatOr(p1:Pattern, p2:Pattern);
	PatTuple(l:ImmutableList<Pattern>);
	PatExtractor(v:core.Type.TVar, e:core.Type.TExpr, p:Pattern);
}