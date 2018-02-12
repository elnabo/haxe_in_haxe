package syntax;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
import ocaml.List;

enum DeclFlag {
	DPrivate;
	DExtern;
}

class Parser {

	public static var special_identifier_files = new Map<String, String>();

	public static function decl_flag_to_class_flag (f:DeclFlag) : core.Ast.ClassFlag {
		return switch (f) {
			case DPrivate: HPrivate;
			case DExtern : HExtern;
		}
	}
	public static function decl_flag_to_enum_flag (f:DeclFlag) : core.Ast.EnumFlag {
		return switch (f) {
			case DPrivate: EPrivate;
			case DExtern : EExtern;
		}
	}
	public static function decl_flag_to_abstract_flag (f:DeclFlag) : core.Ast.AbstractFlag {
		return switch (f) {
			case DPrivate: APrivAbstract;
			case DExtern : AExtern;
		}
	}
	
	public static function error_msg (msg:syntax.parser.ErrorMsg) : String {
		return switch (msg) {
			case Unexpected(token): "Unexpected " + core.Ast.s_token(token);
			case Duplicate_default: "Duplicate default";
			case Missing_semicolon: "Missing ;";
			case Unclosed_macro: "Unclosed macro";
			case Unimplemented: "Not implemented for current platform";
			case Missing_type: "Missing type declaration";
			case Custom(s): s;
		};
	}

	public static function last_token(s:haxeparser.HaxeParser) {
		// TODO ("Ensure correct behaviour of syntax.Parser.last_token");
		// var n = s.count;
		// return syntax.parser.TokenCache.get((n==0) ? 0 : n-1);
		return s.last;
	}


	public static function get_doc (parser:haxeparser.HaxeParser) : Option<String> {
		// do the peek first to make sure we fetch the doc
		var peeked = parser.npeek(1)[0];
		if (peeked == null) {
			return None;
		}

		return switch (last_doc) {
			case None : None;
			case Some(ld):
				last_doc = None;
				(ld.pos == peeked.pos.pmin) ? Some(ld.doc) : None;
		}
	}

	public static inline function serror() : Dynamic {
		throw ocaml.Error.instance;
	}

	public static inline function error(m:syntax.parser.ErrorMsg, p:core.Globals.Pos) : Dynamic {
		throw new syntax.parser.Error(m,p);
	}

	public static var display_error:syntax.parser.ErrorMsg->core.Globals.Pos->Void = function(error_msg:Dynamic, pos:core.Globals.Pos) : Void { throw false; }

	public static var last_doc : Option<{doc:String, pos:Int}> = None;
	public static var use_doc: Bool = false;
	public static var resume_display : core.Globals.Pos = core.Globals.null_pos;
	public static var in_macro : Bool = false;

	public static inline function do_resume() : Bool {	
		return resume_display != core.Globals.null_pos;
	}

	public static inline function display(e:core.Ast.Expr) : Dynamic {
		throw new syntax.parser.Display(e);
	}


	public static function type_path(sl:ImmutableList<String>, in_import:Bool) : Dynamic {
		return switch (sl) {
			case n::l if (n.charCodeAt(0) >= "A".code && n.charCodeAt(0)<="Z".code):
				throw new syntax.parser.TypePath(List.rev(l), Some({c:n, cur_package:false}), in_import);
			case _:
				throw new syntax.parser.TypePath(List.rev(sl),None,in_import);
		}
	}

	public static inline function is_resuming_file (file:String) {
		return core.Path.unique_full_path(file) == resume_display.pfile;
	}

	public static inline function is_resuming(p:core.Globals.Pos) {
		var p2 = resume_display;
		return p.pmax == p2.pmin && is_resuming_file(p.pfile);
	}

	public static inline function set_resume(p:core.Globals.Pos) {
		resume_display = new core.Globals.Pos(core.Path.unique_full_path(p.pfile), p.pmin, p.pmax);
	}
	
	public static inline function encloses_resume(p:core.Globals.Pos) {
		return p.pmin <= resume_display.pmin && p.pmax >= resume_display.pmax;
	}

	public static function would_skip_resume (p1:core.Globals.Pos, s:haxeparser.HaxeParser) {
		return switch (s.npeek(1)) {
			case [{pos:p2}]: is_resuming_file(p2.pfile) && encloses_resume(core.Ast.punion(p1, p2));
			case _: false;
		}
	}
}