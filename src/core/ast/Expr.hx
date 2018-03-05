package core.ast;

import haxe.ds.ImmutableList;

class Expr {
	public static function ensure_block (e:core.Ast.Expr) : core.Ast.Expr {
		return switch (e.expr) {
			case EBlock(_): e;
			case _: {expr:EBlock([e]), pos:e.pos};
		}
	}

	public static function field_assoc<T> (name:String, fl:ImmutableList<{name:String, pos:core.Globals.Pos, quotes:core.Ast.QuoteStatus, expr:T}>) : T {
		// function loop (fl:ImmutableList<core.Ast.ObjectField>) {
		function loop (fl:ImmutableList<{name:String, pos:core.Globals.Pos, quotes:core.Ast.QuoteStatus, expr:T}>) {
			return switch (fl) {
				case {name:name_, expr:e}::fl:
					if (name_==name) {
						e;
					}
					else {
						loop(fl);
					}
				case []: throw ocaml.Not_found.instance;
			}
		}
		return loop(fl);
	}

	public static function field_mem_assoc<T> (name:String, fl:ImmutableList<{name:String, pos:core.Globals.Pos, quotes:core.Ast.QuoteStatus, expr:T}>) : Bool {
		function loop (fl:ImmutableList<{name:String, pos:core.Globals.Pos, quotes:core.Ast.QuoteStatus, expr:T}>) {
			return switch (fl) {
				case {name:name_, expr:e}::fl:
					if (name_==name) {
						throw ocaml.Exit.instance;
					}
					else {
						loop(fl);
					}
				case []: false;
			}
		}
		return try {
			loop(fl);
		}
		catch (_:ocaml.Exit) {
			true;
		}
	}
}