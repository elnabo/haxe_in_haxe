package context.display;

// import haxe.ds.Option;

class DisplayEmitter {
	public static function display_module_type (dm:context.common.DisplayMode.Settings, mt, p:core.Globals.Pos) : Void {
		switch (dm.dms_kind) {
			case DMPosition: throw context.Display.DisplayException.DisplayPosition([core.Type.t_infos(mt).mt_pos]);
			case DMUsage(_):
				var ti = core.Type.t_infos(mt);
				ti.mt_meta.unshift({name:Usage, params:[], pos:ti.mt_pos});
			case DMType: throw context.Display.DisplayException.DisplayType(core.Type.type_of_module_type(mt), p, None);
			case _:
		}
	}

	public static function display_type (dm:context.common.DisplayMode.Settings, t:core.Type.T, p:core.Globals.Pos) : Void {
		switch (dm.dms_kind) {
			case DMType: throw context.Display.DisplayException.DisplayType(t, p, None);
			case _:
		}
		try {
			display_module_type(dm, core.Type.module_type_of_type(t), p);
		}
		catch (_:ocaml.Exit) {
			var f1 = core.Type.follow(t);
			var f2 = core.Type.follow(core.Type.t_dynamic_def.get());
			switch (f2) {
				case TDynamic(_): // sanity check in case it's still t_dynamic
				case _:
			}
			switch (f1) {
				case TDynamic(_): display_type(dm, core.Type.t_dynamic_def.get(), p);
				case _:
			}
		}
	}

	public static function check_display_type (ctx:context.Typecore.Typer, t:core.Type.T, p:core.Globals.Pos) {
		function add_type_hint () {
			ctx.com.shared.shared_display_information.type_hints.set(p, t);
		}
		function maybe_display_type () {
			if (ctx.is_display_file && context.Display.is_display_position(p)) {
				display_type(ctx.com.display, t, p);
			}
		}
		switch (ctx.com.display.dms_kind) {
			case DMStatistics: add_type_hint();
			case DMUsage(_): add_type_hint(); maybe_display_type();
			case _: maybe_display_type();
		}
	}

	public static function display_variable (dm:context.common.DisplayMode.Settings, v:core.Type.TVar, p:core.Globals.Pos) : Void {
		switch (dm.dms_kind) {
			case DMPosition: throw context.Display.DisplayException.DisplayPosition([v.v_pos]);
			case DMUsage(_):
				v.v_meta.unshift({name:Usage, params:[], pos:v.v_pos});
			case DMType: throw context.Display.DisplayException.DisplayType(v.v_type,p,None);
			case _:
		}
	}

	public static function display_field (dm:context.common.DisplayMode.Settings, cf:core.Type.TClassField, p:core.Globals.Pos) {
		switch (dm.dms_kind) {
			case DMPosition: throw context.Display.DisplayException.DisplayPosition([cf.cf_pos]);
			case DMUsage(_): cf.cf_meta.unshift({name:Usage, params:[], pos:cf.cf_pos});
			case DMType: throw context.Display.DisplayException.DisplayType(cf.cf_type, p, cf.cf_doc);
			case _:
		}
	}

	public static function maybe_display_field (ctx:context.Typecore.Typer, p:core.Globals.Pos, cf:core.Type.TClassField) {
		if (context.Display.is_display_position(p)) {
			display_field(ctx.com.display, cf, p);
		}
	}

	public static function display_enum_field (dm:context.common.DisplayMode.Settings, ef:core.Type.TEnumField, p:core.Globals.Pos) : Void {
		switch (dm.dms_kind) {
			case DMPosition: throw context.Display.DisplayException.DisplayPosition([p]);
			case DMUsage(_): ef.ef_meta.unshift({name:Usage, params:[], pos:p});
			case DMType: throw context.Display.DisplayException.DisplayType(ef.ef_type, p, ef.ef_doc);
			case _:
		}
	}

	public static function display_meta (dm:context.common.DisplayMode.Settings, meta:core.Meta.StrictMeta) : Void {
		switch (dm.dms_kind) {
			case DMType:
				switch (meta) {
					case Custom(_), Dollar(_):
					case _:
						switch (core.Meta.get_documentation(meta)) {
							case None:
							case Some(s):
								// TODO: hack until we support proper output for hover display mode
								throw context.Display.DisplayException.Metadata("<metadata>"+s.b+"</metadata>");
						}
				}
			case DMField:
				var all = core.Meta.get_documentation_list().a;
				var all = all.map(function (e) { return {name:e.a, kind:context.Display.DisplayFieldKind.FKMetadata, doc:e.b}; });
				throw context.Display.DisplayException.DisplayFields(all);
			case _:
		}
	}

	public static function check_display_metadata (ctx:context.Typecore.Typer, meta:core.Ast.Metadata) : Void {
		for (m in meta) {
			if (context.Display.is_display_position(m.pos)) {
				display_meta(ctx.com.display, m.name);
			}
			for (e in m.params) {
				if (context.Display.is_display_position(e.pos)) {
					var e = context.display.ExprPreprocessing.process_expr(ctx.com, e);
					context.Typecore.delay(ctx, PTypeField, function () { context.Typecore.type_expr(ctx, e, Value); });
				}
			}
		}
	}

}