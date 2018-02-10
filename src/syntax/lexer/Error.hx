package syntax.lexer;

class Error {
	public var error_msg:ErrorMsg;
	public var pos:core.Globals.Pos;
	public function new (error_msg:ErrorMsg, pos:core.Globals.Pos) {
		this.error_msg = error_msg;
		this.pos = pos;
	}

	public static function of(error_msg:ErrorMsg, pos:hxparse.Position) : Error {
		return new Error(error_msg, new core.Globals.Pos(pos.psource, pos.pmin, pos.pmax));
	}
}