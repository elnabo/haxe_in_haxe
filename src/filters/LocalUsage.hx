package filters;

import ocaml.List;

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
		switch (e.eexpr) {
			case TBinop((OpAssign|OpAssignOp(_)), {eexpr:TLocal(v)}, e2):
				local_usage(f, e2);
				f(Assign(v));
			case TUnop((OpIncrement|OpDecrement), _, {eexpr:TLocal(v)}):
				f(Assign(v));
			case TLocal(v):
				f(Use(v));
			case TVar(v, eo):
				switch (eo) { case None: case Some(e): local_usage(f, e); }
				f(Declare(v));
			case TFunction(tf):
				function cc (f) {
					List.iter(function (arg) { var v = arg.v; f(Declare(v)); }, tf.tf_args);
					local_usage(f, tf.tf_expr);
				}
				f(Function(cc));
			case TBlock(l):
				f(Block(function (f) { List.iter(local_usage.bind(f), l); }));
			case TFor(v, it, e):
				local_usage(f, it);
				f(Loop(function (f) {
					f(Declare(v));
					local_usage(f, e);
				}));
			case TWhile(_):
				f(Loop(function (f) {
					core.Type.iter(local_usage.bind(f), e);
				}));
			case TTry(e, catchs):
				local_usage(f, e);
				List.iter(function (_tmp) {
					var v = _tmp.v; var e = _tmp.e;
					f(Block(function (f) {
						f(Declare(v));
						local_usage(f, e);
					}));
				}, catchs);
			case _:
				core.Type.iter(local_usage.bind(f), e);
		}
	}
}