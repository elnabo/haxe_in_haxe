package filters;

enum Usage {
	Block(f:(Usage->Void)->Void);
	Loop(f:(Usage->Void)->Void);
	Function(f:(Usage->Void)->Void);
	Declare(v:core.Type.TVar);
	Use(v:core.Type.TVar);
	Assign(v:core.Type.TVar);
}

class LocalUsage {
	public static function local_usage(f:Usage->Void, e:core.Type.TExpr) : Void {
		trace("TODO: LocalUsage.local_usage");
		throw false;
	}
}