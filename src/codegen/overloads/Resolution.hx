package codegen.overloads;

import haxe.ds.ImmutableList;

import core.Globals.Pos;
import core.Type.T;
import core.Type.TExpr;


/**
 *  Overload resolution
 */
class Resolution {

	public static function reduce_compatible (compatible:ImmutableList<{fst:ImmutableList<{fst:TExpr, snd:Bool}>, snd:T, trd:TExpr->Pos->TExpr}>) : ImmutableList<{fst:ImmutableList<{fst:TExpr, snd:Bool}>, snd:T, trd:TExpr->Pos->TExpr}> {
		trace("TODO: codegen.overloads.Resolution");
		throw false;
	}
}