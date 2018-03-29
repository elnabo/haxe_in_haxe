package neko;

import neko.Nast.Expr;
import neko.Nast.Pos;

class Error {
	public final msg:String;
	public final pos:Pos;

	public function new (msg:String, pos:Pos) {
		this.msg = msg; this.pos = pos;
	}
}

class Ncompile {

	public static function compile (version:Int, ast:Expr) : Dynamic {
		trace("TODO: neko.Ncompile.compile");
		throw false;
	}

}