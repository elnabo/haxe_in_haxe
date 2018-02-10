package syntax.parser;

class Error {
	public var error_msg:ErrorMsg;
	public var pos:core.Globals.Pos;
	public function new (error_msg:ErrorMsg, pos:core.Globals.Pos) {
		this.error_msg = error_msg;
		this.pos = pos;
	}
	public static function of(msg:ErrorMsg, pos:{file:String, min:Int, max:Int}) {
		return new Error(msg, new core.Globals.Pos(pos.file, pos.min, pos.max));
	}
}