package context.common;

import haxe.ds.Option;
import ocaml.Ref;

enum ErrorPolicy {
	EPIgnore;
	EPCollect;
	EPShow;
}

enum DisplayFilePolicy {
	DFPOnly;
	DFPAlso;
	DFPNo;
}

typedef Settings = {
	dms_kind : context.common.displaymode.T,
	dms_display : Bool,
	dms_full_typing : Bool,
	dms_force_macro_typing : Bool,
	dms_error_policy : ErrorPolicy,
	dms_collect_data : Bool,
	dms_check_core_api : Bool,
	dms_inline : Bool,
	dms_display_file_policy : DisplayFilePolicy,
	dms_exit_during_typing : Bool
}

class DisplayMode {

	public static function get_default_display_settings() : Settings {
		return {
			dms_kind : DMField,
			dms_display : true,
			dms_full_typing : false,
			dms_force_macro_typing : false,
			dms_error_policy : EPIgnore,
			dms_collect_data : false,
			dms_check_core_api : false,
			dms_inline : false,
			dms_display_file_policy : DFPOnly,
			dms_exit_during_typing : true
		};
	}

	public static function get_default_compilation_settings() : Settings {
		return {
			dms_kind : DMNone,
			dms_display : false,
			dms_full_typing : true,
			dms_force_macro_typing : true,
			dms_error_policy : EPShow,
			dms_collect_data : false,
			dms_check_core_api : true,
			dms_inline : true,
			dms_display_file_policy : DFPNo,
			dms_exit_during_typing : false
		};
	}

	public static function create (dm:context.common.displaymode.T) : Settings {
		var settings = get_default_display_settings();
		settings.dms_kind = dm;
		return switch (dm) {
			case DMNone:
				get_default_compilation_settings();
			case DMField, DMPosition, DMResolve(_), DMPackage, DMType, DMSignature:
				settings;
			case DMUsage(_):
				settings.dms_full_typing = true;
				settings.dms_collect_data = true;
				settings.dms_display_file_policy = DFPAlso;
				settings.dms_exit_during_typing = false;
				settings;
			case DMToplevel:
				settings.dms_full_typing = true;
				settings;
			case DMModuleSymbols(filter):
				settings.dms_display_file_policy = (filter == None) ? DFPOnly : DFPNo;
				settings.dms_exit_during_typing = false;
				settings.dms_force_macro_typing = false;
				settings;
			case DMDiagnostics(global):
				settings.dms_full_typing = true;
				settings.dms_error_policy = EPCollect;
				settings.dms_collect_data = true;
				settings.dms_inline = true;
				settings.dms_display_file_policy = (global) ? DFPNo : DFPAlso;
				settings.dms_exit_during_typing = false;
				settings;
			case DMStatistics:
				settings.dms_full_typing = true;
				settings.dms_collect_data = true;
				settings.dms_inline = false;
				settings.dms_display_file_policy = DFPAlso;
				settings.dms_exit_during_typing = false;
				settings;
		};
	}

	public static function toString (kind:context.common.displaymode.T) : String {
		return switch (kind) {
			case DMNone: "none";
			case DMField: "field";
			case DMPosition: "position";
			case DMResolve(s): "resolve " + s;
			case DMPackage: "package";
			case DMType: "type";
			case DMUsage(b): (b) ? "rename" : "references";
			case DMToplevel: "toplevel";
			case DMModuleSymbols(s):
				switch (s) {
					case None: "module-symbols";
					case Some(t): "workspace-symbols " + t;
				}
			case DMDiagnostics(b) : ((b) ? "global " : "") + "diagnostics";
			case DMStatistics : "statistics";
			case DMSignature : "signature";
		}
	}
}