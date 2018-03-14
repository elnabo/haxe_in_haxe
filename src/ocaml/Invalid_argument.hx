package ocaml;

class Invalid_argument {
	public static final instance = new Invalid_argument("");
	public final msg:String;
	public function new(msg) { this.msg = msg; }
}