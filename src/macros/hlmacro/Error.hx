package macros.hlmacro;

import haxe.ds.ImmutableList;

class Error {
	public var s:String;
	public var p:ImmutableList<core.Globals.Pos>;
	public function new (s:String, p:ImmutableList<core.Globals.Pos>) {
		this.s = s;
		this.p = p;
	}
}