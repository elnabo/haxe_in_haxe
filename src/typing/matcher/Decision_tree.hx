package typing.matcher;

enum Type_finiteness {
	Infinite; // type has inifite constructors (e.g. Int, String)
	CompileTimeFinite; //type is considered finite only at compile-time but has inifite possible run-time values (enum abstracts)
	RunTimeFinite; // type is truly finite (Bool, enums)
}

class Decision_tree {
	public static function to_string (tabs:String, dt:typing.matcher.decisiontree.Dt) : String {
		trace("TODO: Decision_tree.to_string");
		throw false;
	}
}