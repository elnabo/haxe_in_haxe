package context.common.displaymode;

import haxe.ds.Option;

enum T {
	DMNone;
	DMField;
	DMUsage (b:Bool); // true = also report definition
	DMPosition;
	DMToplevel;
	DMResolve (s:String);
	DMPackage;
	DMType;
	DMModuleSymbols (s:Option<String>);
	DMDiagnostics (b:Bool); /* true = global, false = only in display file */
	DMStatistics;
	DMSignature;
}