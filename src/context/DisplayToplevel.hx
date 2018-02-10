package context;

import core.type.StringError;

class DisplayToplevel {
	public static function collect(ctx:context.Typecore.Typer, only_types:Bool) : Array<context.common.identifiertype.T> {
		trace("TODO: context.DisplayToplevel.collect");
		return null;
	}

	public static function handle_unresolved_identifier (ctx:context.Typecore.Typer, i:String, p, only_types:Bool) {
		var l = collect(ctx, only_types);
		var cl = l.map(function (it) {
			var s = context.common.IdentifierType.get_name(it);
			return {fst:{fst:s, snd:it}, snd:StringError.levenshtein(i, s)};
		});
		cl.sort( function (a1, a2) {
			var c1 = a1.snd;
			var c2 = a2.snd;
			if (c1 == c2) { return 0; }
			return (c1 > c2) ? 1 : -1;
		});
		function filter (el:{fst:{fst:String, snd:context.common.identifiertype.T}, snd:Int}) : Bool {
			return el.snd > 0 && el.snd <= Math.min(el.fst.fst.length, (i.length / 3.0)) ;
		}
		var _cl = StringError.filter_similar(filter, cl);
		ctx.com.display_information.unresolved_identifiers.unshift({s:i, pos:p, l:_cl});
	}
}