package optimization;

import core.Type.TExprExpr;

import haxe.ds.ImmutableList;
import haxe.ds.Option;

import ocaml.DynArray;
import ocaml.Hashtbl;
import ocaml.List;
import ocaml.Ref;

using ocaml.Cloner;
using equals.Equal;

/*
	This module rewrites some expressions to reduce the amount of special cases for subsequent analysis. After analysis
	it restores some of these expressions back to their original form.

	The following expressions are removed from the AST after `apply` has run:
	- OpBoolAnd and OpBoolOr binary operations are rewritten to TIf
	- OpAssignOp on a variable is rewritten to OpAssign
	- Prefix increment/decrement operations are rewritten to OpAssign
	- Postfix increment/decrement operations are rewritten to a TBlock with OpAssign and OpAdd/OpSub
	- `do {} while(true)` is rewritten to `while(true) {}`
	- TWhile expressions are rewritten to `while (true)` with appropriate conditional TBreak
	- TFor is rewritten to TWhile
*/
class TexprFilter {
	public static function apply (com:context.Common.Context, e:core.Type.TExpr) : core.Type.TExpr {
		function loop (e:core.Type.TExpr) {
			return switch (e.eexpr) {
				case TBinop(op=(OpBoolAnd|OpBoolOr), e1, e2):
					var e_then = e2;
					var _tmp = if (op == OpBoolOr) {
						{
							fst:core.Type.mk(TUnop(OpNot, Prefix, e1), com.basic.tbool, e.epos),
							snd:core.Type.mk(TConst(TBool(true)), com.basic.tbool, e.epos)
						};
					}
					else {
						{fst: e1, snd:core.Type.mk(TConst(TBool(false)), com.basic.tbool, e.epos)};
					}
					var e_if = _tmp.fst; var e_else = _tmp.snd;
					loop(core.Type.mk(TIf(e_if, e_then, Some(e_else)), e.etype, e.epos));
				case TBinop(OpAssignOp(op), e1={eexpr:TLocal(_)}, e2):
					var e = e.with({eexpr:TBinop(op, e1, e2)});
					loop(e.with({eexpr:TBinop(OpAssign,e1, e2)}));
				case TUnop(op=(OpIncrement|OpDecrement), flag, e1={eexpr:TLocal(_)}):
					var e_one = core.Type.mk(TConst(TInt(1)), com.basic.tint, e1.epos);
					var e = e.with({eexpr:TBinop(OpAssignOp((op == OpIncrement) ? OpAdd : OpSub), e1, e_one)});
					var e = if (flag == Prefix) {
						e;
					}
					else {
						core.Type.mk(TBlock([
							e.with({eexpr:TBinop(OpAssignOp((op == OpIncrement) ? OpAdd : OpSub), e1, e_one)}),
							e.with({eexpr:TBinop(((op == OpIncrement) ? OpSub : OpAdd), e1, e_one)})
						]), e.etype, e.epos);
					}
					loop(e);
				case TWhile(e1, e2, DoWhile) if (AnalyzerTexpr.is_true_expr(e1)):
					loop(e.with({eexpr:TWhile(e1, e2, NormalWhile)}));
				case TWhile(e1, e2, flag) if (!AnalyzerTexpr.is_true_expr(e1)):
					var p = e.epos;
					var e_break = core.Type.mk(TBreak, core.Type.t_dynamic, p);
					var e_not = core.Type.mk(TUnop(OpNot, Prefix, core.Texpr.Builder.mk_parent(e1)), e1.etype, e1.epos);
					function e_if (eo) { return core.Type.mk(TIf(e_not, e_break, eo), com.basic.tvoid, p); }
					function map_continue (e:core.Type.TExpr) {
						return switch (e.eexpr) {
							case TContinue:
								core.Texpr.duplicate_tvars(e_if(Some(e)));
							case TWhile(_,_,_), TFor(_,_,_):
								e;
							case _:
								core.Type.map_expr(map_continue, e);
						}
					}
					var e2 = (flag == NormalWhile) ? e2 : map_continue(e2);
					var e_if = e_if(None);
					var e_block = (flag == NormalWhile) ? core.Type.concat(e_if, e2) : core.Type.concat(e2, e_if);
					var e_true = core.Type.mk(TConst(TBool(true)), com.basic.tbool, p);
					var e = core.Type.mk(TWhile(core.Texpr.Builder.mk_parent(e_true), e_block, NormalWhile), e.etype, p);
					loop(e);
				case TFor(v, e1, e2):
					var e = core.Texpr.for_remap(com.basic, v, e1, e2, e.epos);
					loop(e);
				case _:
					core.Type.map_expr(loop, e);
			}
		}
		return loop(e);
	}
}

@:structInit
class InterferenceReport {
	public final ir_var_reads:Hashtbl<Int, Bool>;
	public final ir_var_writes:Hashtbl<Int, Bool>;
	public final ir_field_reads:Hashtbl<String, Bool>;
	public final ir_field_writes:Hashtbl<String, Bool>;
	public var ir_state_read : Bool;
	public var ir_state_write : Bool;
	public function new (ivr:Hashtbl<Int, Bool>, ivw:Hashtbl<Int, Bool>, ifr:Hashtbl<String, Bool>, ifw:Hashtbl<String, Bool>, isr:Bool, isw:Bool) {
		ir_var_reads = ivr;
		ir_var_writes = ivw;
		ir_field_reads = ifr;
		ir_field_writes = ifw;
		ir_state_read = isr;
		ir_state_write = isw;
	}

	public static inline function create () {
		return new InterferenceReport(Hashtbl.create(0),Hashtbl.create(0),Hashtbl.create(0),Hashtbl.create(0),false,false);
	}

	public static inline function has_var_read (ir:InterferenceReport, v:core.Type.TVar) : Bool {
		return Hashtbl.mem(ir.ir_var_reads, v.v_id);
	}
	public static inline function has_var_write (ir:InterferenceReport, v:core.Type.TVar) : Bool {
		return Hashtbl.mem(ir.ir_var_writes, v.v_id);
	}
	public static inline function has_field_read (ir:InterferenceReport, s:String) : Bool {
		return Hashtbl.mem(ir.ir_field_reads, s);
	}
	public static inline function has_field_write (ir:InterferenceReport, s:String) : Bool {
		return Hashtbl.mem(ir.ir_field_writes, s);
	}
	public static inline function has_state_read (ir:InterferenceReport) : Bool {
		return ir.ir_state_read;
	}
	public static inline function has_state_write (ir:InterferenceReport) : Bool {
		return ir.ir_state_write;
	}
	public static inline function has_any_field_read (ir:InterferenceReport) : Bool {
		return Hashtbl.length(ir.ir_field_reads) > 0;
	}
	public static inline function has_any_field_write (ir:InterferenceReport) : Bool {
		return Hashtbl.length(ir.ir_field_writes) > 0;
	}
	public static inline function has_any_var_read (ir:InterferenceReport) : Bool {
		return Hashtbl.length(ir.ir_var_reads) > 0;
	}
	public static inline function has_any_var_write (ir:InterferenceReport) : Bool {
		return Hashtbl.length(ir.ir_var_writes) > 0;
	}

	public static function from_texpr (e:core.Type.TExpr) : InterferenceReport {
		trace("TODO: InterferenceReport.from_texpr");
		throw false;
	}
	public static function to_string (ir:InterferenceReport) : String {
		trace("TODO: InterferenceReport.to_string");
		throw false;
	}
}

class Fusion_state {
	public var _changed = false;
	public final var_reads:Hashtbl<Int,Int>;
	public final var_writes:Hashtbl<Int,Int>;
	public function new () {
		var_reads = Hashtbl.create(0);
		var_writes = Hashtbl.create(0);
	}

	inline function change (map:Hashtbl<Int,Int>, v:core.Type.TVar, delta:Int) {
		Hashtbl.replace(map, v.v_id, try { Hashtbl.find(map, v.v_id) + delta; } catch (_:ocaml.Not_found) { delta; } );
	}

	public inline function inc_reads (v:core.Type.TVar) : Void { change(var_reads, v, 1); }
	public inline function dec_reads (v:core.Type.TVar) : Void { change(var_reads, v, -1); }
	public inline function inc_writes (v:core.Type.TVar) : Void { change(var_writes, v, 1); }
	public inline function dec_writes (v:core.Type.TVar) : Void { change(var_writes, v, -1); }

	public inline function get_reads (v:core.Type.TVar) : Int { return try { Hashtbl.find(var_reads, v.v_id); } catch (_:ocaml.Not_found) { 0; }}
	public inline function get_writes (v:core.Type.TVar) : Int { return try { Hashtbl.find(var_writes, v.v_id); } catch (_:ocaml.Not_found) { 0; }}

	public inline function change_writes (v:core.Type.TVar, delta:Int) {
		change(var_writes, v, delta);
	}

	public inline function changed () { _changed = true; }
	public inline function reset () { _changed = false; }
	public inline function did_change () { return _changed; }

	public function infer_from_texpr (e:core.Type.TExpr) : Void {
		function loop (e:core.Type.TExpr) : Void {
			switch (e.eexpr) {
				case TLocal(v): inc_reads(v);
				case TBinop(OpAssign, {eexpr:TLocal(v)}, e2):
					inc_writes(v);
					loop(e2);
				case _:
					core.Type.iter(loop, e);
			}
		}
		return loop(e);
	}
}

class Fusion {
	public static function apply (com:context.Common.Context, config:optimization.AnalyzerConfig, e:core.Type.TExpr) : core.Type.TExpr {
		trace("TODO: Fusion.apply");
		var state = new Fusion_state();
		state.infer_from_texpr(e);
		/* Handles block-level expressions, e.g. by removing side-effect-free ones and recursing into compound constructs like
			array or object declarations. The resulting element list is reversed. */
		function block_element (acc:ImmutableList<core.Type.TExpr>, el:ImmutableList<core.Type.TExpr>) {
			return switch (el) {
				case (e1={eexpr:(TBinop((OpAssign|OpAssignOp(_)), _, _)|TUnop((OpIncrement|OpDecrement),_,_))}) :: el:
					block_element(e1::acc, el);
				case (e1={eexpr:TLocal(_)}) :: el if (!config.local_dce):
					block_element(e1::acc, el);
				case {eexpr:TLocal(v)} :: el:
					state.dec_reads(v);
					block_element(acc, el);
				// no-side-effect
				case {eexpr:(TEnumParameter(_)|TEnumIndex(_)|TFunction(_)|TConst(_)|TTypeExpr(_))} :: el:
					block_element(acc, el);
				case {eexpr:TMeta({name:Pure},_)} :: el:
					block_element(acc, el);
				case {eexpr:TCall({eexpr:TField(e1, fa)}, el1)} :: el2 if (OptimizerTexpr.PurityState.is_pure_field_access(fa) && config.local_dce):
					block_element(acc, e1::List.append(el1, el2));
				case {eexpr:TNew(c, tl, el1)} :: el2 if (config.local_dce && switch (c.cl_constructor) { case Some(cf) if (OptimizerTexpr.PurityState.is_pure(c, cf)): true; case _: false; }):
					block_element(acc, List.append(el1, el2));
				case {eexpr:TIf({eexpr:TConst(TBool(t))}, e1, e2)} :: el:
					if (t) {
						block_element(acc, e1 :: el);
					}
					else {
						switch (e2) {
							case None: block_element(acc, el);
							case Some(e): block_element(acc, e :: el);
						}
					}
				// no-side-effect composites
				case {eexpr:(TParenthesis(e1)|TMeta(_,e1)|TCast(e1,None)|TField(e1,_)|TUnop(_,_,e1))} :: el:
					block_element(acc, e1 :: el);
				case {eexpr:(TArray(e1, e2)|TBinop(_,e1,e2))} :: el:
					block_element(acc, e1 :: e2 :: el);
				case {eexpr:(TArrayDecl(el1)|TCall({eexpr:TField(_,FEnum(_))}, el1))} :: el2: // TODO: check e1 of FEnum
					block_element(acc, List.append(el1, el2));
				case {eexpr:TObjectDecl(fl)} :: el:
					block_element(acc, List.append(List.map(function (f) { return f.expr; }, fl), el));
				case {eexpr:TIf(e1, {eexpr:TBlock([])}, (Some({eexpr:TBlock([])})|None))} :: el:
					block_element(acc, e1 :: el);
				case {eexpr:TBlock([e1])} :: el:
					block_element(acc, e1 :: el);
				case {eexpr:TBlock([])} :: el:
					block_element(acc, el);
				case e1 :: el:
					block_element(e1 :: acc, el);
				case []:
					acc;
			}
		}
		function can_be_fused (v:core.Type.TVar, e:core.Type.TExpr) : Bool {
			var num_uses = state.get_reads(v);
			var num_writes = state.get_writes(v);
			var can_be_used_as_value = AnalyzerTexpr.can_be_used_as_value(com, e);
			var is_compiler_generated = core.Meta.has(CompilerGenerated, v.v_meta);
			var has_type_params = switch (v.v_extra) { case Some({params:tl}) if (tl != Tl): true; case _: false; }
			var b = num_uses <= 1 && num_writes == 0 && can_be_used_as_value && !AnalyzerTexpr.is_asvar_type(v.v_type) && (is_compiler_generated || (config.optimize && config.fusion && config.user_var_fusion && !has_type_params));
			if (config.fusion_debug) {
				Sys.println('FUSION\n\tvar ${v.v_name}<${v.v_id}> = ${AnalyzerTexpr.s_expr_pretty(e)}');
				Sys.println('\tcan_be_used:${b}: num_uses:${num_uses} <= 1 && num_writes:${num_writes} = 0 && can_be_used_as_value:${can_be_used_as_value} && (is_compiler_generated:${is_compiler_generated} || config.optimize:${config.optimize} && config.fusion:${config.fusion} && config.user_var_fusion:${config.user_var_fusion})');
			}
			return b;
		}
		function fuse (acc:ImmutableList<core.Type.TExpr>, el:ImmutableList<core.Type.TExpr>) : core.Type.TExpr {
			return switch (el) {
				case (e1={eexpr:TVar(v1, None)}) :: {eexpr:TBinop(OpAssign, {eexpr:TLocal(v2)}, e2)} :: el if (v1 == v2):
					state.changed();
					var e1 = e1.with({eexpr:TVar(v1, Some(e2))});
					state.dec_writes(v1);
					fuse(e1::acc, el);
				case (e1={eexpr:TIf(eif, ethen, Some(eelse))}) :: el if (
						(switch (com.platform) {case Cpp: !context.Common.defined(com, Cppia); case _: true; })
						&& !(core.Type.ExtType.is_void(e1.etype))
						&& AnalyzerTexpr.can_be_used_as_value(com, e1)
					):
					try {
						var i = new Ref(0);
						var e_ = new Ref(None);
						function check(e1:core.Type.TExpr, f1:core.Type.TExpr->core.Type.TExpr, e2:core.Type.TExpr) : core.Type.TExpr {
							return switch (e_.get()) {
								case None:
									e_.set(Some({fst:e1, snd:f1}));
									e2;
								case Some({fst:e_}):
									if (core.Texpr.equal(e_, e1)) {
										e2;
									}
									else {
										throw ocaml.Exit.instance;
									}
							}
						}
						function check_assign (e:core.Type.TExpr) : core.Type.TExpr {
							return switch (e.eexpr) {
								case TBinop(OpAssign, e1, e2):
									i.set(i.get()+1);
									check(e1, function (e_:core.Type.TExpr) { return e.with({eexpr:TBinop(OpAssign, e1, e_)}); }, e2);
								case _: throw ocaml.Exit.instance;
							}
						}
						var e = AnalyzerTexpr.map_values(check_assign, e1).fst;
						var e = switch (e_.get()) {
							case None: trace("Shall not be seen"); std.Sys.exit(255); throw false;
							case Some({fst:e1, snd:f}):
								switch (e1.eexpr) {
									case TLocal(v): state.change_writes(v, 1 - i.get());
									case _:
								}
								f(e);
						}
						state.changed();
						fuse(e::acc, el);
					}
					catch (_:ocaml.Exit) {
						fuse(e1::acc, el);
					}
				case {eexpr:TVar(v1, Some(e1))} :: el if (config.optimize && config.local_dce && state.get_reads(v1) == 0 && state.get_writes(v1) == 0):
					fuse(acc, e1::el);
				case (ev={eexpr:TVar(v1, None)}) :: el if (!v1.v_capture):
					var found = new Ref(false);
					function replace (deep:Bool, e:core.Type.TExpr) {
						return switch (e.eexpr) {
							case TBinop(OpAssign, {eexpr:TLocal(v2)}, e2) if (v1 == v2):
								if (deep) { throw ocaml.Exit.instance; }
								found.set(true);
								ev.with({eexpr:TVar(v1, Some(e2))});
							case TLocal(v2) if (v1 == v2): throw ocaml.Exit.instance;
							case _: core.Type.map_expr(replace.bind(true), e);
						}
					}
					try {
						function loop(acc:ImmutableList<core.Type.TExpr>, el:ImmutableList<core.Type.TExpr>) {
							return switch (el) {
								case e :: el:
									var e = replace(false, e);
									if (found.get()) { List.append(List.rev(e::acc), el); }
									else { loop(e::acc, el); }
								case []: List.rev(acc);
							}
						}
						var el = loop([], el);
						if (!found.get()) { throw ocaml.Exit.instance; }
						state.changed();
						state.dec_writes(v1);
						fuse(acc, el);
					}
					catch (_:ocaml.Exit) {
						fuse(ev::acc, el);
					}
				case (ev={eexpr:TVar(v1, Some(e1))}) :: el if (!can_be_fused(v1, e1)):
					var found = new Ref(false);
					var blocked = new Ref(false);
					var ir = InterferenceReport.from_texpr(e1);
					if (config.fusion_debug) { Sys.println('\tInterferenceReport: ${InterferenceReport.to_string(ir)}\n\t${core.Type.s_expr_pretty(true,"\t", false, core.Type.s_type.bind(core.Type.print_context()), core.Type.mk(TBlock(el), core.Type.t_dynamic, core.Globals.null_pos))}'); }
					/* This function walks the AST in order of evaluation and tries to find an occurrence of v1. If successful, that occurrence is
						replaced with e1. If there's an interference "on the way" the replacement is canceled. */
					function replace (e:core.Type.TExpr) : core.Type.TExpr {
						function explore (e:core.Type.TExpr) : core.Type.TExpr {
							var old = blocked.get();
							blocked.set(true);
							var e = replace(e);
							blocked.set(old);
							return e;
						}
						function _handle_el (el:ImmutableList<core.Type.TExpr>) : ImmutableList<core.Type.TExpr> {
							/* This mess deals with the fact that the order of evaluation is undefined for call
								arguments on these targets. Even if we find a replacement, we pretend that we
								didn't in order to find possible interferences in later call arguments. */
							var temp_found = false;
							var really_found = new Ref(found.get());
							var el = List.map(function (e:core.Type.TExpr) {
								found.set(temp_found);
								var e = replace(e);
								if (found.get()) { really_found.set(true); }
								return e;
							}, el);
							found.set(really_found.get());
							return el;
						}
						var handle_el = (!AnalyzerTexpr.target_handles_side_effect_order(com)) ? _handle_el : List.map.bind(replace);
						function handle_call(e2:core.Type.TExpr, el:ImmutableList<core.Type.TExpr>) : {e:core.Type.TExpr, el:ImmutableList<core.Type.TExpr>} {
							return switch (com.platform) {
								case Neko:
									// Neko has this reversed at the moment (issue #4787)
									var el = List.map(replace, el);
									var e2 = replace(e2);
									{e:e2, el:el};
								case Cpp:
									var e2 = replace(e2);
									var el = handle_el(el);
									{e:e2, el:el};
								case _:
									var e2 = replace(e2);
									var el = List.map(replace,el);
									{e:e2, el:el};
							}
						}
						return
						if (found.get()) { e; }
						else {
							switch (e.eexpr) {
								case TWhile(_), TTry(_): throw ocaml.Exit.instance;
								case TFunction(_): e;
								case TIf(e1, e2, eo):
									var e1 = replace(e1);
									if (!found.get() && (InterferenceReport.has_state_write(ir) || InterferenceReport.has_any_field_write(ir) || InterferenceReport.has_any_var_write(ir))) {
										throw ocaml.Exit.instance;
									}
									var e2 = replace(e2);
									var eo = ocaml.Option.map(replace, eo);
									e.with({eexpr:TIf(e1, e2, eo)});
								case TSwitch(e1, cases, edef):
									var e1 = switch (com.platform) {
										case Lua, Python: explore(e1);
										case _: replace(e1);
									}
									if (!found.get()) { throw ocaml.Exit.instance; }
									e.with({eexpr:TSwitch(e1, cases, edef)});
								// locals
								case TLocal(v2) if (v1 == v2 && !blocked.get()):
									found.set(true);
									(AnalyzerTexpr.type_change_ok(com, v1.v_type, e1.etype)) ? e1 :  core.Type.mk(TCast(e1, None), v1.v_type, e.epos);
								case TLocal(v):
									if (InterferenceReport.has_var_write(ir, v) || (v.v_capture || AnalyzerTexpr.is_ref_type(v.v_type)) && InterferenceReport.has_state_write(ir)) { throw ocaml.Exit.instance; }
									e;
								case _: trace("finish"); throw false;
							}
						}
					}
					trace("finsih"); throw false;
			}
		}
		throw false;
	}
}

class Cleanup {
	public static function apply (com:context.Common.Context, e:core.Type.TExpr) : core.Type.TExpr {
		trace("TODO: Cleanup.apply");
		throw false;
	}
}

class Purity {
	public static function infer (com:context.Common.Context) : ImmutableList<core.Type.TClassField> {
		trace("TODO Purity.infer");
		throw false;
	}
}

class AnalyzerTexpr {

	public static inline function s_expr_pretty (e:core.Type.TExpr) : String {
		return core.Type.s_expr_pretty(false, "", false, core.Type.s_type.bind(core.Type.print_context()), e);
	}

	public static function is_true_expr(e1:core.Type.TExpr) {
		return switch (e1.eexpr) {
			case TConst(TBool(true)): true;
			case TParenthesis(e1): is_true_expr(e1);
			case _: false;
		}
	}

	public static function is_stack_allocated (c:core.Type.TClass) {
		return core.Meta.has(StructAccess, c.cl_meta);
	}

	public static function map_values (?allow_control_flow:Bool=true, f:core.Type.TExpr->core.Type.TExpr, e:core.Type.TExpr) : {fst:core.Type.TExpr, snd:Option<core.Type.TExpr>} {
		var branching = new Ref(false);
		var efinal = new Ref<Option<core.Type.TExpr>>(None);
		function f (e:core.Type.TExpr) : core.Type.TExpr {
			return
			if (branching.get()) {
				f(e);
			}
			else {
				efinal.set(Some(e));
				core.Type.mk(TConst(TNull), e.etype, e.epos);
			}
		}
		function loop (complex:Bool, e:core.Type.TExpr) : core.Type.TExpr {
			return switch (e.eexpr) {
				case TIf(e1, e2, Some(e3)):
					branching.set(true);
					var e2 = loop(true, e2);
					var e3 = loop(true, e3);
					e.with({eexpr:TIf(e1, e2, Some(e3))});
				case TSwitch(e1, cases, edef):
					branching.set(true);
					var cases = List.map(function (c) { var el = c.values; var e = c.e; return {values:el, e:loop(true, e)}; }, cases);
					var edef = ocaml.Option.map(loop.bind(true), edef);
					e.with({eexpr:TSwitch(e1, cases, edef)});
				case TBlock([e1]):
					loop(complex, e1);
				case TBlock(el):
					switch (List.rev(el)) {
						case e1 :: el:
							var e1 = loop(true, e1);
							var e = e.with({eexpr:TBlock(List.rev(e1::el))});
							e.with({eexpr:TMeta({name:MergeBlock, params:Tl, pos:e.epos}, e)});
						case []:
							f(e);
					}
				case TTry(e1, catches):
					branching.set(true);
					var e1 = loop(true, e1);
					var catches = List.map(function (c) { var v = c.v; var e = c.e; return {v:v, e:loop(true, e)}; }, catches);
					e.with({eexpr:TTry(e1, catches)});
				case TMeta(m, e1):
					e.with({eexpr:TMeta(m, loop(complex, e1))});
				case TParenthesis(e1):
					e.with({eexpr:TParenthesis(loop(complex, e1))});
				case TBreak, TContinue, TThrow(_), TReturn(_):
					if (!allow_control_flow) { throw ocaml.Exit.instance; }
					e;
				case _:
					if (!complex) { throw ocaml.Exit.instance; }
					f(e);
			}
		}
		var e = loop(false, e);
		return {fst:e, snd:efinal.get()};
	}

	public static function can_throw (e:core.Type.TExpr) {
		function loop (e:core.Type.TExpr) {
			return switch (e.eexpr) {
				case TConst(_), TLocal(_), TTypeExpr(_), TFunction(_), TBlock(_):
				case TCall(_,_), TNew(_,_,_), TThrow(_), TCast(_, Some(_)): throw ocaml.Exit.instance;
				case TField(_,_), TArray(_,_): throw ocaml.Exit.instance; // sigh
				case _: core.Type.iter(loop, e);
			}
		}
		return
		try {
			loop(e); false;
		}
		catch (_:ocaml.Exit) {
			true;
		}
	}

	public static function can_be_inlined (e:core.Type.TExpr) : Bool {
		return switch (e.eexpr) {
			case TConst(_): true;
			case TParenthesis(e1), TMeta(_, e1): can_be_inlined(e1);
			case _: false;
		}
	}

	public static function target_handles_unops (com:context.Common.Context) : Bool {
		return switch (com.platform) {
			case Lua, Python: false;
			case _: true;
		}
	}

	public static function target_handles_assign_ops (com:context.Common.Context) : Bool {
		/* Technically PHP can handle assign ops, but unfortunately x += y is not always
			equivalent to x = x + y in case y has side-effects. */
		return switch (com.platform) {
			case Lua, Php: false;
			case Cpp if (!context.Common.defined(com, Cppia)): false;
			case _: true;
		}
	}

	public static function target_handles_side_effect_order (com:context.Common.Context) : Bool {
		return switch (com.platform) {
			case Cpp: context.Common.defined(com, Cppia);
			case Php: false;
			case _: true;
		}
	}

	public static function can_be_used_as_value (com:context.Common.Context, e:core.Type.TExpr) {
		function loop (e:core.Type.TExpr) {
			switch (e.eexpr) {
				case TBlock([e]): loop(e);
				case TBlock(_), TSwitch(_,_,_), TTry(_,_): throw ocaml.Exit.instance;
				case TCall({eexpr:TConst(TString("phi"))},_): throw ocaml.Exit.instance;
				case TReturn(_), TThrow(_), TBreak, TContinue: throw ocaml.Exit.instance;
				case TUnop((OpIncrement|OpDecrement), _, _) if (!target_handles_unops(com)): throw ocaml.Exit.instance;
				case TFunction(_):
				case _: core.Type.iter(loop, e);
			}
		}
		return
		try {
			switch [com.platform, e.eexpr] {
				case [(Cs|Cpp|Java|Flash|Lua), TConst(TNull)]: throw ocaml.Exit.instance;
				case _:
			}
			loop(e);
			true;
		}
		catch (_:ocaml.Exit) {
			false;
		}
	}

	public static function wrap_meta (s:String, e:core.Type.TExpr) : core.Type.TExpr {
		return core.Type.mk(TMeta({name:Custom(s), params:Tl, pos:e.epos}, e), e.etype, e.epos);
	}

	public static function is_really_unbound (s:String) : Bool {
		return switch (s) {
			case "`trace", "__int__": false;
			case _: true;
		}
	}

	// ocaml: let r = Str.regexp "^\\([A-Za-z0-9_]\\)+$"
	public static final r = ~/^[A-Za-z0-9_]+$/;
	public static function is_unbound_call_that_might_have_side_effects (s:String, el:ImmutableList<core.Type.TExpr>) : Bool {
		return switch [s, el] {
			case ["__js__", [{eexpr:TConst(TString(s))}]] if (r.match(s)): false;
			case _: true;
		}
	}

	public static function is_ref_type (t:core.Type.T) : Bool {
		return switch (t) {
			case TType({t_path:{a:["cs"], b:("Ref"|"Out")}}, _): true;
			case TType({t_path:path}, _) if (path.equals(generators.Genphp7.ref_type_path)): true;
			case TType({t_path:{a:["cpp"], b:"Reference"}}, _): true;
			case TAbstract({a_path:{a:["hl", "types"], b:"Ref"}}, _): true;
			case _: false;
		}
	}

	public static function is_asvar_type (t:core.Type.T) : Bool {
		function check (meta:core.Ast.Metadata) {
			return AnalyzerConfig.has_analyzer_option(meta, "as_var");
		}
		return switch (t) {
			case TInst(c, _): check(c.cl_meta);
			case TEnum(en, _): check(en.e_meta);
			case TType(t, tl): check(t.t_meta) || is_asvar_type(core.Type.apply_params(t.t_params, tl, t.t_type));
			case TAbstract(a, _): check(a.a_meta);
			case TLazy(f): is_asvar_type(core.Type.lazy_type(f));
			case TMono(var r):
				switch (r.get()) {
					case Some(t): is_asvar_type(t);
					case _: false;
				}
			case _: false;
		}
	}

	public static function type_change_ok (com:context.Common.Context, t1:core.Type.T, t2:core.Type.T) : Bool {
		trace("TODO: type_change_ok");
		throw false;
	}

	public static function dynarray_map<A> (f:A->A, d:DynArray<A>) : Void {
		for (i in 0...d.length) {
			d[i] = f(d[i]);
		}
	}

	public static function dynarray_mapi<A> (f:(Int, A)->A, d:DynArray<A>) : Void {
		for (i in 0...d.length) {
			d[i] = f(i, d[i]);
		}
	}
}