package ocaml;

import haxe.Constraints.IMap;
import haxe.EnumTools;
import haxe.ds.ObjectMap;
import haxe.io.Bytes;
// import haxe.rtti.Meta;

class Cloner {

	static function _clone<T> (v:T, existing:ObjectMap<Dynamic,Dynamic>) : T {
		var base:Dynamic = null;
		switch (Type.typeof(v)) {
			case TNull, TInt, TFloat, TBool, TFunction: // immutable
				return v;
			case TUnknown: // ??
				return v;
			case TEnum(e):
				var index = EnumValueTools.getIndex(cast v);
				var params = [ for (p in EnumValueTools.getParameters(cast v)) _clone(p, existing) ];
				return EnumTools.createByIndex(e, index, params);
			case TClass(_):
				if (Std.is(v, String)) { return v; }
				if (Std.is(v, Array)) { return cast [ for (e in cast(v, Array<Dynamic>)) _clone(e, existing) ]; }
				if (Std.is(v, Date)) { return cast Date.fromTime(cast(v, Date).getTime()); }
				if (Std.is(v, Bytes)) { return cast Bytes.ofData(cast clone(cast (v, Bytes).getData())); }
				if (Std.is(v, IMap)) {
					var map = cast(v, IMap<Dynamic, Dynamic>);
					var res:IMap<Dynamic, Dynamic> = map.copy();
					for (key in map.keys()) {
						res.set(key, map.get(key));
					}
					return cast res;
				}
				if (Std.is(v, context.Typecore.TyperGlobals) || Std.is(v, context.Common.Context)) {
					return v;
					// trace(Meta.getType(v));
					// trace(Meta.getType(v).clone);
				}
				base = Type.createEmptyInstance(Type.getClass(v)); // class type
			case TObject:
				base = {} // anonymous type
		}
		if (existing.exists(v)) { return cast existing.get(v); }
		existing.set(v, base);
		for (field in Reflect.fields(v)) {
			Reflect.setField(base, field, _clone(Reflect.field(v, field), existing));
		}
		return cast base;
	}

	public static function clone<T> (v:T) : T {
		return _clone(v, new ObjectMap<Dynamic,Dynamic>());
	}
}