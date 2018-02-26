package ocaml;

class Failure {
	public static final instance = new Failure("");
	public var msg:String;
	public function new (msg:String) {
		this.msg = msg;
	}
}