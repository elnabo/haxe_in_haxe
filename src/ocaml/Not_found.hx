package ocaml;

class Not_found extends Exception {
	public static final instance = new Not_found();
	function new () {}
}