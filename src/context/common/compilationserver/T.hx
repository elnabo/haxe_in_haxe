package context.common.compilationserver;

import haxe.ds.ImmutableList;

typedef T = {
	cache : context.common.CompilationServer.Cache,
	signs : ImmutableList<{s1:String, s2:String}>
}