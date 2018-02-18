package core.ast;

class Expr {
	public static function ensure_block (e:core.Ast.Expr) : core.Ast.Expr {
		return switch (e.expr) {
			case EBlock(_): e;
			case _: {expr:EBlock([e]), pos:e.pos};
		}
	}
}