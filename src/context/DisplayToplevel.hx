package context;

import core.type.StringError;
import haxe.ds.ImmutableList;
import ocaml.List;

class DisplayToplevel {
	public static function collect(ctx:context.Typecore.Typer, only_types:Bool) : ImmutableList<context.common.identifiertype.T> {
		trace("TODO: context.DisplayToplevel.collect");
		throw false;
	}

	public static function handle_unresolved_identifier (ctx:context.Typecore.Typer, i:String, p, only_types:Bool) {
		var l = collect(ctx, only_types);
		var cl = List.map(function (it) {
			var s = context.common.IdentifierType.get_name(it);
			return {fst:{fst:s, snd:it}, snd:StringError.levenshtein(i, s)};
		}, l);
		cl = List.sort( function (a1:{fst:{fst:String, snd:context.common.identifiertype.T}, snd:Int}, a2:{fst:{fst:String, snd:context.common.identifiertype.T}, snd:Int}) {
			var c1 = a1.snd; var c2 = a2.snd;
			if (c1 == c2) { return 0; }
			return (c1 > c2) ? 1 : -1;
		}, cl);
		function filter (el:{fst:String, snd:context.common.identifiertype.T}, r:Int) : Bool {
			var s = el.fst;
			return r > 0 && r <= Math.min(s.length, (i.length / 3.0)) ;
		}
		var _cl = StringError.filter_similar(filter, cl);
		ctx.com.display_information.unresolved_identifiers = {s:i, pos:p, l:_cl} :: ctx.com.display_information.unresolved_identifiers;
	}
}