package ocaml;

import haxe.Constraints.IMap;
import haxe.EnumTools;
import haxe.ds.ObjectMap;
import haxe.io.Bytes;
// import haxe.rtti.Meta;

class Cloner {

	public static function with<T> (o:T, change:Dynamic): T {
		var res = Reflect.copy(o);
		for (field in  Reflect.fields(o)) {
			if (Reflect.hasField(change, field)) {
				Reflect.setField(res, field, Reflect.field(change, field));
			}
		}
		return res;
	}

	static function _clone<T> (v:T, existing:ObjectMap<Dynamic,Dynamic>, ?deep:Bool=true) : T {
		// return v; // ?
		var base:Dynamic = null;
		switch (Type.typeof(v)) {
			case TNull, TInt, TFloat, TBool, TFunction: // immutable
				return v;
			case TUnknown: // ??
				return v;
			case TEnum(e):
				// return v;
				if (Std.is(v, haxe.ds.ImmutableList.ListRepr)) {
					return v;
				}
				var index = EnumValueTools.getIndex(cast v);
				var params = [ for (p in EnumValueTools.getParameters(cast v))  ((deep) ? _clone(p, existing) : p) ];
				return EnumTools.createByIndex(e, index, params);
			case TClass(_):
				if (Std.is(v, String)) { return v; }
				if (Std.is(v, haxe.ds.ImmutableList)) { return v; }
				if (Std.is(v, Array)) { return cast [ for (e in cast(v, Array<Dynamic>)) (deep) ? _clone(e, existing) : e]; }
				if (Std.is(v, Date)) { return cast Date.fromTime(cast(v, Date).getTime()); }
				if (Std.is(v, Bytes)) { return cast Bytes.ofData(cast clone(cast (v, Bytes).getData())); }
				if (Std.is(v, IMap)) {
					var map = cast(v, IMap<Dynamic, Dynamic>);
					var res:IMap<Dynamic, Dynamic> = map.copy();
					if (deep) {
						for (key in map.keys()) {
							res.set(key, map.get(key));
						}
					}
					return cast res;
				}
				if (Std.is(v, context.Typecore.TyperGlobals) || Std.is(v, context.Common.Context)) {
					return v;
					// trace(Meta.getType(v));
					// trace(Meta.getType(v).clone);
				}
				if (Std.is(v, ocaml.PMap)) {
					return v;
				}
				base = Type.createEmptyInstance(Type.getClass(v)); // class type
			case TObject:
				base = {} // anonymous type
		}
		if (existing.exists(v)) { return cast existing.get(v); }
		existing.set(v, base);
		for (field in Reflect.fields(v)) {
			if (deep) {
				Reflect.setField(base, field, _clone(Reflect.field(v, field), existing));
			}
			else {
				Reflect.setField(base, field, Reflect.field(v, field));
			}
		}
		return cast base;
	}

	public static function clone<T> (v:T, ?deep:Bool=false) : T {
		return _clone(v, new ObjectMap<Dynamic,Dynamic>(), deep);
	}
}