package context.display;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
import ocaml.List;
import ocaml.Ref;

class ExprPreprocessing {
	public static function find_enclosing (com:context.Common.Context, e:core.Ast.Expr) : core.Ast.Expr {
		var display_pos = new Ref(syntax.Parser.resume_display.get());
		function mk_null (p:core.Globals.Pos) : core.Ast.Expr {
			return {expr:EDisplay({expr:EConst(CIdent("null")), pos:p}, false), pos:p};
		}
		function encloses_display_pos(p:core.Globals.Pos) : Option<core.Globals.Pos> {
			if (context.Display.encloses_position(display_pos.get(), p)) {
				var p = display_pos.get();
				display_pos.set(new core.Globals.Pos("", -2, -2));
				return Some(p);
			}
			else {
				return None;
			}
		}
		function loop(e:core.Ast.Expr) : core.Ast.Expr {
			switch (e.expr) {
				case EBlock(el):
					var p = e.pos;
					// We want to find the innermost block which contains the display position.
					var el = List.map(loop, el);
					var el = switch (encloses_display_pos(p)) {
						case None: el;
						case Some(p2):
							var tmp = ocaml.List.fold_left( function (bel:{fst:Bool, snd:ImmutableList<core.Ast.Expr>}, e:core.Ast.Expr) : {fst:Bool, snd:ImmutableList<core.Ast.Expr>} {
								var b = bel.fst; var el = bel.snd;
								var p = e.pos;
								if (b || p.pmax <= p2.pmin) {
									return {fst:b, snd:e::el};
								}
								else {
									var e_d:core.Ast.Expr = {expr:EDisplay(mk_null(p), false), pos:p};
									return {fst:true, snd:e::(e_d::el)};
								}
							}, {fst:false, snd:[]}, el);
							var b = tmp.fst; var el = tmp.snd;
							el = if (b) {
								el;
							}
							else {
								mk_null(p) :: el;
							};
							List.rev(el);
					}
					return {expr:EBlock(el), pos:e.pos};
				case _:
					return core.Ast.map_expr(loop, e);
			}
		}
		return loop(e);
	}

	public static function find_before_pos (com:context.Common.Context, e:core.Ast.Expr) : core.Ast.Expr {
		var display_pos = new Ref(syntax.Parser.resume_display.get());
		function is_annotated(p:core.Globals.Pos) : Bool {
			if (p.pmin <= display_pos.get().pmin && p.pmax >= display_pos.get().pmax) {
				display_pos.set(new core.Globals.Pos("", -2, -2));
				return true;
			}
			else {
				return false;
			}
		}
		function loop(e:core.Ast.Expr) : core.Ast.Expr {
			if (is_annotated(e.pos)) {
				return {expr:EDisplay(e, false), pos:e.pos};
			}
			else {
				return e;
			}
		}
		function map(e:core.Ast.Expr) {
			return loop(core.Ast.map_expr(map, e));
		}
		return map(e);
	}

	public static function find_display_call (e:core.Ast.Expr) : core.Ast.Expr {
		var found = new Ref(false);
		function loop (e:core.Ast.Expr) {
			if (found.get()) {
				return e;
			}
			else {
				switch (e.expr) {
					case ECall(_), ENew(_):
						if (context.Display.is_display_position(e.pos)) {
							found.set(true);
							return {expr:EDisplay(e, true), pos:e.pos};
						}
						else {
							return e;
						}
					case _: return e;
				}
			}
		}
		function map (e:core.Ast.Expr) : core.Ast.Expr {
			return switch (e.expr) {
				case EDisplay(_, true):
					found.set(true);
					e;
				case EDisplay(e1, false):
					map(e1);
				case _: loop(core.Ast.map_expr(map, e));
			}
		}
		return map(e);

	}

	public static function process_expr (com:context.Common.Context, e:core.Ast.Expr) : core.Ast.Expr {
		return switch(com.display.dms_kind) {
			case DMToplevel: find_enclosing(com, e);
			case DMPosition, DMUsage(_), DMType: find_before_pos(com, e);
			case DMSignature: find_display_call(e);
			case _: e;
		}
	}

}