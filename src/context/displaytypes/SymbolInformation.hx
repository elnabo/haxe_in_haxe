package context.displaytypes;

class SymbolInformation {

	public static function make (name : String,	kind : context.displaytypes.symbolkind.T,	pos : core.Globals.Pos,	container_name : haxe.ds.Option<String>) : context.common.displaytypes.symbolinformation.T {
		return {
			name : name,
			kind : kind,
			pos : pos,
			container_name : container_name
		};
	}
}
