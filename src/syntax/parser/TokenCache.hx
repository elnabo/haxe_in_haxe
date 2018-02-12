package syntax.parser;

class TokenCache {
	public static var cache:ocaml.DynArray<syntax.Lexer.Token> = [];

	public static function add(token:syntax.Lexer.Token) {
		cache.push(token);
	}

	public static function get(i:Int) {
		return cache[i];
	}

	public static function clear () : Void->Void {
		var old_cache = cache.copy();
		cache = [];
		return function () {
			cache = old_cache;
		}
	}
}