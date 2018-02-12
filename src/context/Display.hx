package context;

import haxe.ds.ImmutableList;
import haxe.ds.Option;

enum DisplayFieldKind {
	FKVar (t:core.Type.T);
	FKMethod (t:core.Type.T);
	FKType (t:core.Type.T);
	FKModule;
	FKPackage;
	FKMetadata;
	FKTimer (s:String);
}

enum DisplayException {
	Diagnostics (s:String);
	Statistics (s:String);
	ModuleSymbols (s:String);
	Metadata (s:String);
	DisplaySignatures (l:ImmutableList<{sig:core.Type.TSignature, doc:core.Ast.Documentation}>, i:Int);
	DisplayType (t:core.Type.T, pos:core.Globals.Pos, s:Option<String>);
	DisplayPosition (l:ImmutableList<core.Globals.Pos>);
	DisplayFields (l:ImmutableList<{name:String, kind:DisplayFieldKind, doc:core.Ast.Documentation}>);
	DisplayToplevel (l:ImmutableList<context.common.identifiertype.T>);
	DisplayPackage (l:ImmutableList<String>);
}

class Display {
	public static function is_display_file (file:String) : Bool {
		return (file != "?") && 
			(core.Path.unique_full_path(file) == syntax.Parser.resume_display.pfile);
	}

	public static function encloses_position (p_target:core.Globals.Pos, p:core.Globals.Pos) : Bool {
		return p.pmin <= p_target.pmin && p.pmax >= p_target.pmax;
	}

	public static function is_display_position (p:core.Globals.Pos) : Bool {
		return encloses_position(syntax.Parser.resume_display,p);
	}

	public static function display_field_kind_index (dfk:DisplayFieldKind) : Int {
		return switch (dfk) {
			case FKVar(_): 0;
			case FKMethod(_): 1;
			case FKType(_): 2;
			case FKModule: 3;
			case FKPackage: 4;
			case FKMetadata: 5;
			case FKTimer(_): 6;
		}
	}
}