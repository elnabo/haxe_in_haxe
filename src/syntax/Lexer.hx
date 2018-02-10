package syntax;

// import haxeparser.Data;
import core.Ast;
import core.Type;

typedef LexerFile = {
	lfile : String,
	lline : Int,
	lmaxline : Int,
	llines : Array<{a:Int, b:Int}>, // ocaml list
	lalines : Array<{a:Int, b:Int}>, // ocaml array
	lstrings : Array<Int>,
	llast : Int,
	llastindex : Int
}

// typedef Token = {td:core.Ast.Token, pos:{file:String, min:Int, max:Int}};
//@:structInit
class Token {
	public var td:core.Ast.Token;
	// public var pos:{file:String, min:Int, max:Int};
	public var pos:core.Globals.Pos;
	public function new (td:core.Ast.Token, pos:core.Globals.Pos) {
		this.td = td;
		this.pos = pos;
	}
}

class Lexer extends hxparse.Lexer implements hxparse.RuleBuilder {

	static final max_int = 2147483647; // max int

	// from haxeparser.HaxeLexer
	static function mkPos(p:hxparse.Position) {
		return new core.Globals.Pos(p.psource, p.pmin, p.pmax);
		// {
		// 	file: p.psource,
		// 	min: p.pmin,
		// 	max: p.pmax
		// };
	}

	// @:mapping generates a map with lowercase enum constructor names as keys
	// and the constructor itself as value
	// static var keywords = @:mapping(3) haxeparser.Data.Keyword;

	static var buf = new StringBuf();

	static var ident = "_*[a-z][a-zA-Z0-9_]*|_+|_+[0-9][_a-zA-Z0-9]*";
	static var idtype = "_*[A-Z][a-zA-Z0-9_]*";

	static var integer = "([1-9][0-9]*)|0";
	// end from 
	static function mk(lexer:hxparse.Lexer, td) : Token {
			// return new Token(td, mkPos(lexer.curPos()));
			return new Token(td, mkPos(lexer.curPos()));
	}

	public static var skip_header = @:rule [
		// 0xfeff  => function (lexer) { skip_header lexbuff}
		"#![^\n\r]" => {
			true || lexer.token(skip_header);
		},
		"" => {
			return false;
		}
	];

	public static var tok = @:rule [
		"" => mk(lexer, Eof),
		"[\t ]" => lexer.token(tok),
		"\r\n" => {
			newline(lexer);
			lexer.token(tok);
		},
		"[\r\n]" => {
			newline(lexer);
			lexer.token(tok);
		},
		"0x[0-9a-fA-F]+" => mk(lexer, Const(CInt(lexer.current))),
		integer => mk(lexer, Const(CInt(lexer.current))),
		integer + "\\.[0-9]+" => mk(lexer, Const(CFloat(lexer.current))),
		"\\.[0-9]+" => mk(lexer, Const(CFloat(lexer.current))),
		integer + "[eE][\\+\\-]?[0-9]+" => mk(lexer,Const(CFloat(lexer.current))),
		integer + "\\.[0-9]*[eE][\\+\\-]?[0-9]+" => mk(lexer,Const(CFloat(lexer.current))),
		integer + "\\.\\.\\." => mk(lexer,IntInterval(lexer.current.substr(0,-3))),
		"//[^\n\r]*" => mk(lexer, CommentLine(lexer.current.substr(2))),
		"+\\+" => mk(lexer,Unop(OpIncrement)),
		"--" => mk(lexer,Unop(OpDecrement)),
		"~" => mk(lexer,Unop(OpNegBits)),
		"%=" => mk(lexer,Binop(OpAssignOp(OpMod))),
		"&=" => mk(lexer,Binop(OpAssignOp(OpAnd))),
		"|=" => mk(lexer,Binop(OpAssignOp(OpOr))),
		"^=" => mk(lexer,Binop(OpAssignOp(OpXor))),
		"+=" => mk(lexer,Binop(OpAssignOp(OpAdd))),
		"-=" => mk(lexer,Binop(OpAssignOp(OpSub))),
		"*=" => mk(lexer,Binop(OpAssignOp(OpMult))),
		"/=" => mk(lexer,Binop(OpAssignOp(OpDiv))),
		"<<=" => mk(lexer,Binop(OpAssignOp(OpShl))),
		"|\\|=" => mk(lexer,Binop(OpAssignOp(OpBoolOr))),
		"&&=" => mk(lexer,Binop(OpAssignOp(OpBoolAnd))),
		"==" => mk(lexer,Binop(OpEq)),
		"!=" => mk(lexer,Binop(OpNotEq)),
		"<=" => mk(lexer,Binop(OpLte)),
		"&&" => mk(lexer,Binop(OpBoolAnd)),
		"|\\|" => mk(lexer,Binop(OpBoolOr)),
		"<<" => mk(lexer,Binop(OpShl)),
		"->" => mk(lexer,Arrow),
		"\\.\\.\\." => mk(lexer,Binop(OpInterval)),
		"=>" => mk(lexer,Binop(OpArrow)),
		"!" => mk(lexer,Unop(OpNot)),
		"<" => mk(lexer,Binop(OpLt)),
		">" => mk(lexer,Binop(OpGt)),
		";" => mk(lexer, Semicolon),
		":" => mk(lexer, DblDot),
		"," => mk(lexer, Comma),
		"\\." => mk(lexer, Dot),
		"%" => mk(lexer,Binop(OpMod)),
		"&" => mk(lexer,Binop(OpAnd)),
		"|" => mk(lexer,Binop(OpOr)),
		"^" => mk(lexer,Binop(OpXor)),
		"+" => mk(lexer,Binop(OpAdd)),
		"*" => mk(lexer,Binop(OpMult)),
		"/" => mk(lexer,Binop(OpDiv)),
		"-" => mk(lexer,Binop(OpSub)),
		"=" => mk(lexer,Binop(OpAssign)),
		#if (haxe_ver >= 4)
		"in" => mk(lexer,Binop(OpIn)),
		#end
		"[" => mk(lexer, BkOpen),
		"]" => mk(lexer, BkClose),
		"{" => mk(lexer, BrOpen),
		"}" => mk(lexer, BrClose),
		"\\(" => mk(lexer, POpen),
		"\\)" => mk(lexer, PClose),
		"?" => mk(lexer, Question),
		"@" => mk(lexer, At),
		'/\\*' => {
			reset();
			var pmin = lexer.curPos();
			var pmax = try lexer.token(comment) catch (e:haxe.io.Eof) throw syntax.lexer.Error.of(Unclosed_comment, pmin) ;
			var token = mk(lexer, Comment(contents()));
			// token.pos.pmin = pmin.pmin;
			// token.pos.pmax = pmax;
			token.pos = new core.Globals.Pos(token.pos.pfile, pmin.pmin, pmax);
			token;
		},
		'"' => {
			reset();
			var pmin = lexer.curPos();
			var pmax = try lexer.token(string) catch (e:haxe.io.Eof) throw syntax.lexer.Error.of(Unterminated_string, pmin);
			try {
				var token = mk(lexer, Const(CString(core.Ast.unescape(contents()))));
				// token.pos.pmin = pmin.pmin;
				// token.pos.pmax = pmax;
				token.pos = new core.Globals.Pos(token.pos.pfile, pmin.pmin, pmax);
				token;
			}
			catch (e:core.Ast.Invalid_escape_sequence) {
				throw syntax.lexer.Error.of(Invalid_escape(e.char),
					new hxparse.Position(pmin.psource, pmin.pmax+e.i, pmin.pmax+e.i)
				);
			}
		},
		"'" => {
			reset();
			var pmin = lexer.curPos();
			var pmax = try lexer.token(string2) catch (e:haxe.io.Eof) throw syntax.lexer.Error.of(Unterminated_string, pmin);
			try {
				var token = mk(lexer, Const(CString(core.Ast.unescape(contents()))));
				// token.pos.pmin = pmin.pmin;
				// token.pos.pmax = pmax;
				token.pos = new core.Globals.Pos(token.pos.pfile, pmin.pmin, pmax);
				fast_add_fmt_string(new hxparse.Position(cur.lfile, pmin.pmin, pmax));
				token;
			}
			catch (e:core.Ast.Invalid_escape_sequence) {
				throw syntax.lexer.Error.of(Invalid_escape(e.char),
					new hxparse.Position(pmin.psource, pmin.pmax+e.i, pmin.pmax+e.i)
				);
			}
		},
		'~/' => {
			reset();
			var pmin = lexer.curPos();
			var info = try lexer.token(regexp) catch (e:haxe.io.Eof) throw syntax.lexer.Error.of(Unterminated_regexp, pmin);
			var token = mk(lexer, Const(CRegexp(contents(), info.opt)));
			// token.pos.pmin = pmin.pmin; 
			token.pos = new core.Globals.Pos(token.pos.pfile, pmin.pmin, token.pos.pmax);
			token;
		},
		"#" + ident => mk(lexer, Sharp(lexer.current.substr(1))),
		"$[_a-zA-Z0-9]*" => mk(lexer, Dollar(lexer.current.substr(1))),
		ident => {
			var kwd = core.Ast.keyword_s(lexer.current);
			if (kwd != null) {
				mk(lexer, Kwd(kwd));
			}
			else {
				mk(lexer, Const(CIdent(lexer.current)));
			}
		},
		idtype => mk(lexer, Const(CIdent(lexer.current))),
	];

	public static var string = @:rule [
		"\n|\r|(\r\n)" => { newline(lexer); store(lexer); lexer.token(string); },
		"\\\\\\\\" => {
			buf.add("\\\\");
			lexer.token(string);
		},
		"\\\\" => {
			buf.add("\\");
			lexer.token(string);
		},
		"\\\\\"" => {
			buf.add('"');
			lexer.token(string);
		},
		'"' => lexer.curPos().pmax,
		"[^\\\\\"\r\n]+" => {
			store(lexer);
			lexer.token(string);
		}
	];

	public static var string2 = @:rule [
		"\n|\r|(\r\n)" => { newline(lexer); store(lexer); lexer.token(string2); },
		"\\\\\\\\" => {
			buf.add("\\\\");
			lexer.token(string2);
		},
		"\\\\" => {
			buf.add("\\");
			lexer.token(string2);
		},
		'\\\\\'' => {
			buf.add("'");
			lexer.token(string2);
		},
		"'" => lexer.curPos().pmax,
		"($$)|(\\$)|$" => {
			buf.add("$");
			lexer.token(string2);
		},
		"${" => {
			var pmin = lexer.curPos();
			store(lexer);
			open_braces = 0;
			try lexer.token(codeString) catch(e:haxe.io.Eof) throw syntax.lexer.Error.of(Unclosed_code, pmin);
			lexer.token(string2);
		},
		"[^$\\\\'\r\n]+" => {
			store(lexer);
			lexer.token(string2);
		}
	];

	static var open_braces = 0;
	public static var codeString = @:rule [
		"\n|\r|(\r\n)" => {
			newline(lexer);
			store(lexer);
			lexer.token(codeString);
		},
		"{" => {
			store(lexer);
			open_braces++;
			lexer.token(codeString);
		},
		"/" => {
			store(lexer);
			lexer.token(codeString);
		},
		"}" => {
			store(lexer);
			if (open_braces > 0) {
				lexer.token(codeString);
			}
		},
		'"' => {
			buf.addChar('"'.code);
			var pmin = lexer.curPos();
			try lexer.token(string) catch (e:haxe.io.Eof) throw syntax.lexer.Error.of(Unterminated_string, pmin);
			buf.addChar('"'.code);
			lexer.token(codeString);
		},
		"'" => {
			buf.addChar("'".code);
			var pmin = lexer.curPos();
			var pmax = try lexer.token(string2) catch (e:haxe.io.Eof) throw syntax.lexer.Error.of(Unterminated_string, pmin);
			buf.addChar("'".code);
			fast_add_fmt_string(new hxparse.Position(cur.lfile, pmin.pmin, pmax));
			lexer.token(codeString);
		},
		'/\\*' => {
			var pmin = lexer.curPos();
			try lexer.token(comment) catch (e:haxe.io.Eof) throw syntax.lexer.Error.of(Unclosed_comment, pmin);
			lexer.token(codeString);
		},
		"//[^\n\r]*" => {
			store(lexer);
			lexer.token(codeString);
		},
		"[^/\"'{}\n\r]+" => {
			store(lexer);
			lexer.token(codeString);
		}
	];

	public static var regexp = @:rule [
		"\n|\r" => { 
			throw new haxe.io.Eof();
		},
		"\\\\/" => {
			buf.add("/");
			lexer.token(regexp);
		},
		"\\\\r" => {
			buf.add("\r");
			lexer.token(regexp);
		},
		"\\\\n" => {
			buf.add("\n");
			lexer.token(regexp);
		},
		"\\\\t" => {
			buf.add("\t");
			lexer.token(regexp);
		},
		"\\\\[\\\\$\\.*+\\^|{}\\[\\]()?\\-0-9]" => {
			store(lexer);
			lexer.token(regexp);
		},
		"\\\\[wWbBsSdDx]" => {
			store(lexer);
			lexer.token(regexp);
		},
		"\\\\(u|U)(0-9|a-f|A-F)(0-9|a-f|A-F)(0-9|a-f|A-F)(0-9|a-f|A-F)" => {
			store(lexer);
			lexer.token(regexp);
		},
		"\\\\[^(\\)]" => {
			throw syntax.lexer.Error.of(Invalid_character(lexer.current.charCodeAt(0)), lexer.curPos());
		},
		"/" => {
			lexer.token(regexp_options);
		},
		"[^\\\\/\r\n]+" => {
			store(lexer);
			lexer.token(regexp);
		}
	];

	public static var regexp_options = @:rule [
		"[gimsu]*" => {
			{ pmax:lexer.curPos().pmax, opt:lexer.current };
		},
		"[abcdefhjklnopqrtvwxyz]" => {
			throw syntax.lexer.Error.of(Invalid_option, lexer.curPos());
		}
	];

	public static var comment = @:rule [
		"\n" => { 
			newline(lexer);
			store(lexer);
			lexer.token(comment);
		},
		"\r" => { 
			newline(lexer);
			store(lexer);
			lexer.token(comment);
		},
		"\r\n" => { 
			newline(lexer);
			store(lexer);
			lexer.token(comment);
		},
		"*/" => lexer.curPos().pmax,
		"*" => {
			store(lexer);
			lexer.token(comment);
		},
		"[^\\*\n\r]+" => {
			store(lexer);
			lexer.token(comment);
		}
	];

	public static function make_file (file:String) : LexerFile {
		return {
			lfile:file,
			lline:1,
			lmaxline:1,
			llines: [{a:0, b:1}],
			lalines: [{a:0, b:1}],
			lstrings : [],
			llast: max_int, // max int
			llastindex: 0
		};
	}
	public static var cur = make_file("");
	public static var all_files = new Map<String, LexerFile>();

	public static function init (file:String, do_add:Bool) {
		var f = make_file(file);
		cur = f;
		if (do_add) {
			all_files.set(file, f);
		}
	}

	public static inline function reset () {
		buf = new StringBuf();
	}

	public static inline function contents () : String {
		return buf.toString();
	}

	public static inline function store (lexbuf:hxparse.Lexer) {
		buf.add(lexbuf.current);
	}
	
	public static inline function save () : LexerFile {
		return {
			lfile : cur.lfile,
			lline : cur.lline,
			lmaxline : cur.lmaxline,
			llines : cur.llines.copy(), // ocaml list
			lalines : cur.lalines.copy(), // ocaml array
			lstrings : cur.lstrings.copy(),
			llast : cur.llast,
			llastindex : cur.llastindex
		};
	}

	public static inline function restore (r:LexerFile) {
		cur = r;
	}

	public static inline function newline (lexbuf:hxparse.Lexer) {
		cur.lline ++;
		cur.llines.unshift({a:lexbuf.curPos().pmax, b:cur.lline});
	}

	public static function fmt_pos (p:hxparse.Position) : Int {
		return p.pmin + (p.pmax - p.pmin) * 1000000;
	}

	public static function fast_add_fmt_string(p:hxparse.Position) {
		cur.lstrings.unshift(fmt_pos(p));
	}

	public static function error_msg (m:syntax.lexer.ErrorMsg) : String {
		trace("TODO: syntax.Lexer.error_msg");
		return null;
	}

	public static function find_line (p:Int, f:LexerFile) : {fst:Int, snd:Int} {
		// rebuild cache if we have a new line
		if (f.lmaxline != f.lline) {
			f.lmaxline = f.lline;
			f.lalines = ocaml.List.rev(f.llines);
			f.llast = max_int; // max int
			f.llastindex = 0;
		}
		function loop (min:Int, max:Int) : {fst:Int, snd:Int} {
			var med = (min + max) >> 1;
			var _tmp = f.lalines[med];
			var lp = _tmp.a; var line = _tmp.b;
			if (med == min) {
				f.llast = p;
				f.llastindex = med;
				return {fst:line, snd:p - lp};
			}
			else if (lp > p) {
				return loop(min, med);
			}
			else {
				return loop(med, max);
			}
		}
		if (p >= f.llast) {
			var _tmp = f.lalines[f.llastindex];
			var lp = _tmp.a; var line = _tmp.b;
			var lp2 = (f.llastindex == (f.lalines.length - 1)) ? max_int : f.lalines[f.llastindex+1].a;
			return (p >= lp && p < lp2) ? {fst:line, snd:p-lp} : loop(0, f.lalines.length);
		}
		else {
			return loop(0, f.lalines.length);
		}
	}
	
	// resolve a position within a non-haxe file by counting newlines 
	public static function resolve_pos (file:String) : LexerFile {
		if (!sys.FileSystem.exists(file)) { throw ocaml.Sys_error.instance; }
		// var ch = sys.io.File.getBytes(file);
		var ch = sys.io.File.getContent(file);
		var _pos = 0;
		var f = make_file(file);
		function loop (p:Int) {
			if (p >= ch.length) { return; }
			function inc (i:Int) : Void->Int {
				return function () {
					f.lline++;
					f.llines.unshift({a:p+i, b:f.lline});
					return i;
				}
			}
			// var i = switch ch.getString(_pos++, 1) {
			var i = switch ch.charAt(_pos++) {
				case "\n": inc(1);
				case "\r":
					_pos++;
					inc(2);
				case _: function () { return 1; };
			}
			loop (p + i());
		}
		loop(0);
		return f;
	}

	public static function find_file (file:String) : LexerFile {
		return try {
			ocaml.Hashtbl.find(all_files, file);
		}
		catch (_:ocaml.Not_found) {
			try {
				resolve_pos(file);
			}
			catch (_:ocaml.Sys_error) {
				make_file(file);
			}
		}
	}

	public static function find_pos (p:core.Globals.Pos) : {fst:Int, snd:Int} {
		return find_line(p.pmin, find_file(p.pfile));
	}

	public static function get_error_line (p:core.Globals.Pos) {
		return find_pos(p).fst;
	}

	public static var old_format = new ocaml.Ref(false);

	public static function get_pos_coords (p:core.Globals.Pos) : {l1:Int, p1:Int, l2:Int, p2:Int} {
		var file = find_file(p.pfile);
		var _tmp1 = find_line(p.pmin, file);
		var l1 = _tmp1.fst; var p1 = _tmp1.snd;
		var _tmp2 = find_line(p.pmax, file);
		var l2 = _tmp2.fst; var p2 = _tmp2.snd;
		if (old_format.get()) {
			return {l1:l1, p1:p1, l2:l2, p2:p2};
		}
		else {
			return {l1:l1, p1:p1+1, l2:l2, p2:p2+1};
		}
	}

	public static function get_error_pos (printer:String->Int->String,  p:core.Globals.Pos) : String {
		if (p.pmin == -1) {
			return "(unknown)";
		}
		else {
			var _tmp = get_pos_coords(p);
			var l1 = _tmp.l1; var l2 = _tmp.l2;
			var p1 = _tmp.p1; var p2 = _tmp.p2;
			if (l1 == l2) {
				var s = (p1 == p2) ? ' ${p1}' : 's ${p1}-${p2}';
				return '${printer(p.pfile, l1)} character${s}';
			}
			else {
				return '${printer(p.pfile, l1)} lines ${l1}-${l2}';
			}
		}
	}
}