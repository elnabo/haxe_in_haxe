package typing.matcher.decisiontree;

import haxe.ds.ImmutableList;

enum T {
	Leaf (t:typing.matcher.Case.T);
	Switch(subject:core.Type.TExpr, l:ImmutableList<{fst:typing.matcher.constructor.T, snd:Bool, trd:Dt}>, d:Dt);
	Bind(l:ImmutableList<{fst:core.Type.TVar, snd:core.Globals.Pos, trd:core.Type.TExpr}>, d:Dt);
	Guard(e:core.Type.TExpr, d1:Dt, d2:Dt);
	GuardNull(e:core.Type.TExpr, d1:Dt, d2:Dt);
	Fail;
}
