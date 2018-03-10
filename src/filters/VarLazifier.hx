package filters;

import haxe.ds.Option;
import ocaml.List;
import ocaml.PMap;

import core.Type;

using ocaml.Cloner;

class VarLazifier {
	public static function apply (com:context.Common.Context, e:core.Type.TExpr) : core.Type.TExpr {
		function loop (var_inits:PMap<Int, core.Type.TExpr>, e:core.Type.TExpr) : {fst:PMap<Int, core.Type.TExpr>, snd:core.Type.TExpr} {
			return switch (e.eexpr) {
				case TVar(v, Some(e1)) if (core.Meta.has(Custom(":extract"), v.v_meta)):
					var _tmp = loop(var_inits, e1);
					var var_inits = _tmp.fst; var e1 = _tmp.snd;
					var_inits = PMap.add(v.v_id, e1, var_inits);
					{fst:var_inits, snd:e.with({eexpr:TVar(v, None)})};
				case TLocal(v):
					try {
						var e_init = PMap.find(v.v_id, var_inits);
						var e = e.with({eexpr:TBinop(OpAssign, e, e_init)});
						e = e.with({eexpr:TParenthesis(e)});
						var var_inits = PMap.remove(v.v_id, var_inits);
						{fst:var_inits, snd:e};
					}
					catch (_:ocaml.Not_found) {
						{fst:var_inits, snd:e};
					}
				case TIf(e1, e2, eo):
					var _tmp = loop(var_inits, e1);
					var var_inits = _tmp.fst; var e1 = _tmp.snd;
					var e2 = loop(var_inits, e2).snd;
					var eo = switch (eo) { case None: None; case Some(e): Some(loop(var_inits, e).snd); }
					{fst:var_inits, snd:e.with({eexpr:TIf(e1, e2, eo)})};
				case TSwitch (e1, cases, edef):
					var _tmp = loop(var_inits, e1);
					var var_inits = _tmp.fst; var e1 = _tmp.snd;
					var cases = List.map(function (c) {
						var el = c.values; var e = c.e;
						var e = loop(var_inits, e).snd;
						return {values:el, e:e};
					}, cases);
					var edef = switch (edef) { case None: None; case Some(e): Some(loop(var_inits, e).snd); }
					{fst:var_inits, snd:e.with({eexpr: TSwitch(e1, cases, edef)})};
				case _:
					core.Texpr.foldmap(loop, var_inits, e);
			}
		}
		return loop(PMap.empty(), e).snd;
	}
}