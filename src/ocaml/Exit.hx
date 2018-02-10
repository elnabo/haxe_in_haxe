package ocaml;

class Exit extends Exception {
	public static final instance = new Exit();
	public function new () {}
}