package syntax.parser;

import haxe.ds.Option;

// in import
class TypePath {
	public var p:Array<String>;
	public var c:Option<{c:String, cur_package:Bool}>;
	public var is_import:Bool;
	public function new (p:Array<String>, c:Option<{c:String, cur_package:Bool}>, is_import:Bool) {
		this.p = p;
		this.c = c;
		this.is_import = is_import;
	}
}