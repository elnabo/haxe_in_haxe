package ocaml;

class OcamlString {
	public static function index_from (str:String, id:Int, c:String) : Int {
		if (id < 0 || id > str.length) {
			throw new ocaml.Invalid_argument("ocaml.String.index_from");
		}
		var id = str.indexOf(c, id);
		if (id == -1) {
			throw ocaml.Not_found.instance;
		}
		return id;

	}
}