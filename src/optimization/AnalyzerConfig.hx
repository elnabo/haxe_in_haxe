package optimization;

import haxe.ds.ImmutableList;
import ocaml.List;
using ocaml.Cloner;

enum Debug_kind {
	DebugNone;
	DebugDot;
	DebugFull;
}

@:structInit
class AnalyzerConfig {
	public function new (optimize:Bool, const_propagation:Bool, copy_propagation:Bool, local_dce:Bool, fusion:Bool, purity_inference:Bool, debug_kind:Debug_kind, detail_times:Bool, user_var_fusion:Bool, fusion_debug:Bool) {
		this.optimize = optimize;
		this.const_propagation = const_propagation;
		this.copy_propagation = copy_propagation;
		this.local_dce = local_dce;
		this.fusion = fusion;
		this.purity_inference = purity_inference;
		this.debug_kind = debug_kind;
		this.detail_times = detail_times;
		this.user_var_fusion = user_var_fusion;
		this.fusion_debug = fusion_debug;
	}
	public final optimize : Bool;
	public final const_propagation : Bool;
	public final copy_propagation : Bool;
	public final local_dce : Bool;
	public final fusion : Bool;
	public final purity_inference : Bool;
	public final debug_kind : Debug_kind;
	public final detail_times : Bool;
	public final user_var_fusion : Bool;
	public final fusion_debug : Bool;


	public static final flag_optimize = "optimize";
	public static final flag_const_propagation = "const_propagation";
	public static final flag_copy_propagation = "copy_propagation";
	public static final flag_local_dce = "local_dce";
	public static final flag_fusion = "fusion";
	public static final flag_ignore = "ignore";
	public static final flag_dot_debug = "dot_debug";
	public static final flag_full_debug = "full_debug";
	public static final flag_user_var_fusion = "user_var_fusion";
	public static final flag_fusion_debug = "fusion_debug";

	public static final all_flags = List.fold_left(function (acc:ImmutableList<String>, flag:String) {
		return flag :: ("no_" + flag) :: acc;
	}, [], [flag_optimize,flag_const_propagation,flag_copy_propagation,flag_local_dce,flag_fusion,flag_ignore,flag_dot_debug,flag_user_var_fusion]);

	public static function has_analyzer_option (meta:core.Ast.Metadata, s:String) : Bool {
		return
		try {
			function loop (ml:core.Ast.Metadata) {
				return switch (ml) {
					case {name:Analyzer, params:el} :: ml:
						if (List.exists(function (expr:core.Ast.Expr) {
								var e = expr.expr; var p = expr.pos;
								return
								switch (e) {
									case EConst(CIdent(s2)) if (s == s2): true;
									case _: false;
								}
							}, el)) {
							true;
						}
						else {
							loop(ml);
						}
					case _::ml: loop(ml);
					case []: false;
				}
			}
			loop(meta);
		}
		catch (_:ocaml.Not_found) {
			false;
		}
	}

	public static inline function is_ignored (meta:core.Ast.Metadata) : Bool {
		return has_analyzer_option(meta, flag_ignore);
	}

	public static function get_base_config (com:context.Common.Context) : AnalyzerConfig {
		return {
			optimize: context.Common.raw_defined(com, "analyzer-optimize"),
			const_propagation: !context.Common.raw_defined(com, "analyzer-no-const-propagation"),
			copy_propagation: !context.Common.raw_defined(com, "analyzer-no-copy-propagation"),
			local_dce: !context.Common.raw_defined(com, "analyzer-no-local-dce"),
			fusion: !context.Common.raw_defined(com, "analyzer-no-fusion"),
			purity_inference: !context.Common.raw_defined(com, "analyzer-no-purity-inference"),
			debug_kind: DebugNone,
			detail_times: context.Common.raw_defined(com, "analyzer-times"),
			user_var_fusion: com.platform.match(Flash|Java) && (context.Common.raw_defined(com, "analyzer-user-var-fusion") || (!com.debug && !context.Common.raw_defined(com, "analyzer-no-user-var-fusion"))),
			fusion_debug: false
		};
	}

	public static function update_config_from_meta (com:context.Common.Context, config:AnalyzerConfig, meta:core.Ast.Metadata) : AnalyzerConfig {
		return List.fold_left(function (config:AnalyzerConfig, meta:core.Ast.MetadataEntry) {
			return switch (meta) {
				case {name:Analyzer, params:el}:
					List.fold_left(function (config:AnalyzerConfig, e:core.Ast.Expr) {
						return switch (e.expr) {
							case EConst(CIdent(s)):
								switch (s) {
									case "optimize": config.with({optimize:true});
									case "no_optimize": config.with({optimize:false});
									case "const_propagation": config.with({const_propagation:true});
									case "no_const_propagation": config.with({const_propagation:false});
									case "copy_propagation": config.with({copy_propagation:true});
									case "no_copy_propagation": config.with({copy_propagation:false});
									case "local_dce": config.with({local_dce:true});
									case "no_local_dce": config.with({local_dce:false});
									case "fusion": config.with({fusion:true});
									case "no_fusion": config.with({fusion:false});
									case "user_var_fusion": config.with({use_var_fusion:true});
									case "no_user_var_fusion": config.with({use_var_fusion:false});
									case "dot_debug": config.with({debug_kind:DebugDot});
									case "full_debug": config.with({debug_kind:DebugFull});
									case "fusion_debug": config.with({fusion_debug:true});
									case "as_var": config;
									case _:
										com.warning(core.type.StringError.string_error(s, all_flags, "Unrecognized analyzer option: " + s), e.pos);
										config;
								}
							case _:
								var s = core.Ast.s_expr(e);
								com.warning(core.type.StringError.string_error(s, all_flags, "Unrecognized analyzer option: " +s), e.pos);
								config;
						}
					}, config, el);
				case {name:HasUntyped}:
					config.with({optimize:false});
				case _: config;
			}
		}, config, meta);
	}
}