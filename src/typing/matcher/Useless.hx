package typing.matcher;

import haxe.ds.ImmutableList;
import ocaml.List;

enum EUseless {
	False;
	Pos(p:core.Globals.Pos);
	True;
}

class Useless {

	// U part

	// U' part

	public static function copy (p:ImmutableList<Pattern>) {
	}

	// Sane part

	public static function check_case (com:context.Common.Context, p:ImmutableList<Case>, c:Case) {
		trace("TODO: Useless.check_case");
		var p = List.map(function (c:Case) { var patterns = c.trd; return patterns; }, p);
		// switch (_)
		// throw false;
	}

	public static function check (com:context.Common.Context, cases) : Void {
		List.fold_left(function (acc:ImmutableList<Case>, c:Case) {
			var case_ = c.fst; var bindings = c.snd; var patterns = c.trd;
			check_case(com, acc, (c));
			return
			if (case_.case_guard == None) {
				List.append(acc, [c]);
			}
			else {
				acc;
			}
		}, [], cases);
	}
}