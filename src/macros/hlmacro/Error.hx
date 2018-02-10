package macros.hlmacro;

class Error {
	public var s:String;
	public var p:Array<core.Globals.Pos>;
	public function new (s:String, p:Array<core.Globals.Pos>) {
		this.s = s;
		this.p = p;
	}
}