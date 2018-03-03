package core.type;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
import ocaml.List;
import ocaml.PMap;

class Printer {
	public static inline function s_type (t:core.Type.T) : String {
		return core.Type.s_type(core.Type.print_context(), t);
	}

	public static inline function s_pair (s1:String, s2:String) : String {
		return '($s1,$s2)';
	}

	public static inline function s_record_field (name:String, value:String) : String {
		return '$name = $value';
	}

	public static inline function s_pos (p:core.Globals.Pos) : String {
		return '${p.pfile}: ${p.pmin}-${p.pmax}';
	}

	public static function s_record_fields (tabs:String, fields:ImmutableList<{fst:String, snd:String}>) : String {
		var sl = List.map(function (f) { return s_record_field(f.fst, f.snd); }, fields);
		return '{\n${tabs}\t${List.join("\n\t"+tabs, sl)}\n${tabs}}';
	}

	public static inline function s_list<T> (sep:String, f:T->String, l:ImmutableList<T>) : String {
		return '[${List.join(sep, List.map(f, l))}]';
	}

	public static inline function s_opt<T> (f:T->String, o:Option<T>) : String {
		return switch (o) {
			case None: "None";
			case Some(v): f(v);
		}
	}

	public static inline function s_pmap<K,V> (fk:K->String, fv:V->String, pm:PMap<K, V>) : String {
		return '{${List.join(", ", PMap.foldi(function (k, v, acc:ImmutableList<String>) : ImmutableList<String> {
			return ('${fk(k)} = ${fv(v)}') :: acc; }, pm, []))
		}';
	}

	public static var s_doc = s_opt.bind(function (s:String) { return s; });

	public static inline function s_metadata_entry (entry:core.Ast.MetadataEntry) : String {
		return "@"+core.Meta.to_string(entry.name) + (switch (entry.params) { case []: ""; case el: "("+List.join(", ",List.map(core.Ast.s_expr, el))+")";});
	}

	public static inline function s_metadata (metadata:core.Ast.Metadata) : String {
		return s_list(" ", s_metadata_entry, metadata);
	}

	public static function s_type_param (tp:{name:String, t:core.Type.T}) : String {
		return switch (core.Type.follow(tp.t)) {
			case TInst({cl_kind:KTypeParameter(tl1)}, tl2):
				switch (tl1) {
					case []: tp.name;
					case _: '${tp.name}:${List.join(", ", List.map(s_type, tl1))}';
				}
			case _: trace("Shall not be seen"); throw false;
		}
	}

	public static inline function s_type_params (tl:core.Type.TypeParams) : String {
		return s_list(", ", s_type_param, tl);
	}

	public static function s_tclass_field (tabs:String, cf:core.Type.TClassField) : String {
		trace("TODO: core.type.Printer.s_tclass");
		throw false;
	}

	public static function s_tclass (tabs:String, c:core.Type.TClass) : String {
		trace("TODO: core.type.Printer.s_tclass");
		throw false;
	}
	public static function s_tabstract (tabs:String, c:core.Type.TAbstract) : String {
		trace("TODO: core.type.Printer.s_tabstract");
		throw false;
	}
}