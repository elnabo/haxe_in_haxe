package context.displaytypes.symbolinformation;

typedef T = {
	name : String,
	kind : context.displaytypes.symbolkind.T,
	pos : core.Globals.Pos,
	container_name : haxe.ds.Option<String>
}
