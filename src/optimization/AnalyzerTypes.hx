package optimization;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
import ocaml.DynArray;
import ocaml.Hashtbl;
import ocaml.List;
import ocaml.PMap;
import ocaml.Ref;

using ocaml.Cloner;

typedef Analyzer_context = {
	com : context.Common.Context,
	config : AnalyzerConfig,
	graph : Graph,
	temp_var_name : String,
	entry : BasicBlock,
	has_unbound : Bool,
	loop_counter : Int,
	loop_stack : ImmutableList<Int>,
	debug_exprs : ImmutableList<{s:String, expr:core.Type.TExpr}>,
	name_stack : ImmutableList<String>,
}

enum BlockKind {
	BKRoot; // The unique root block of the graph
	BKNormal; // A normal block
	BKFunctionBegin(tf:core.Type.TFunc); // Entry block of a function
	BKFunctionEnd; // Exit block of a function
	BKSub; // A sub block
	BKConditional; // A "then", "else" or "case" block
	BKLoopHead; // Header block of a loop
	BKException; // Relay block for exceptions
	BKUnreachable; // The unique unreachable block
	BKCatch(v:core.Type.TVar); // A catch block
}

enum Cfg_edge_Flag {
	FlagExecutable;      // Used by constant propagation to handle live edges
	FlagDce;             // Used by DCE to keep track of handled edges
	FlagCopyPropagation; // Used by copy propagation to track handled eges
}

enum Cfg_edge_kind {
	CFGGoto; // An unconditional branch
	CFGFunction; // Link to a function
	CFGMaybeThrow; // The block may or may not throw an exception
	CFGCondBranch(e:core.Type.TExpr); // A conditional branch
	CFGCondElse; // A conditional alternative (else,default)
}

typedef Cfg_edge = {
	cfg_from : BasicBlock, // The source block
	cfg_to : BasicBlock, // The target block
	cfg_kind : Cfg_edge_kind, // The edge kind
	cfg_flags : ImmutableList<Cfg_edge_Flag> // Edge flags
}

enum Syntax_edge {
	SEIfThen(then:BasicBlock, next:BasicBlock, pos:core.Globals.Pos); // `if` with "then" and "next"
	SEIfThenElse(then:BasicBlock, else_block:BasicBlock, next:BasicBlock, t:core.Type.T, pos:core.Globals.Pos); // `if` with "then", "else" and "next"
	SESwitch(cases:ImmutableList<{el:ImmutableList<core.Type.TExpr>, block:BasicBlock}>, default_block:Option<BasicBlock>, next:BasicBlock, pos:core.Globals.Pos); // `switch` with cases, "default" and "next"
	SETry(exc:BasicBlock, block:BasicBlock, catches:ImmutableList<{v:core.Type.TVar, block:BasicBlock}>, next:BasicBlock, pos:core.Globals.Pos); // `try` with "exc", catches and "next"
	SEWhile(head:BasicBlock, body:BasicBlock, next:BasicBlock); // `while` with "head", "body" and "next"
	SESubBlock(sub:BasicBlock, next:BasicBlock); // "sub" with "next"
	SEMerge(b:BasicBlock); // Merge to same block
	SENone; // No syntax exit
}

/*
	A BasicBlock represents a node in the control flow. It has expression elements similar to TBlock in the AST,
	but also holds additional information related to control flow and variables.

	Basic blocks are created whenever it is relevant for control flow. They differ from TBlock in that only their
	final element can be a control flow expression (the terminator). As a consequence, a given TBlock is split up
	into several basic blocks when control flow expressions are encountered.
*/
@:structInit
class BasicBlock {
	function new (bb_id, bb_type, bb_pos, bb_kind, bb_closed, bb_el, bb_phi, bb_outgoing, bb_incoming, bb_dominator, bb_dominated, bb_df, bb_syntax_edge, bb_loop_groups, bb_scopes, bb_var_writes) {
		this.bb_id = bb_id;
		this.bb_type = bb_type;
		this.bb_pos = bb_pos;
		this.bb_kind = bb_kind;
		this.bb_closed = bb_closed;
		this.bb_el = bb_el;
		this.bb_phi = bb_phi;
		this.bb_outgoing = bb_outgoing;
		this.bb_incoming = bb_incoming;
		this.bb_dominator = bb_dominator;
		this.bb_dominated = bb_dominated;
		this.bb_df = bb_df;
		this.bb_syntax_edge = bb_syntax_edge;
		this.bb_loop_groups = bb_loop_groups;
		this.bb_scopes = bb_scopes;
		this.bb_var_writes = bb_var_writes;
	}
	public final bb_id : Int; // The unique ID of the block
	public final bb_type : core.Type.T; // The block type
	public final bb_pos : core.Globals.Pos; // The block position
	public final bb_kind : BlockKind; // The block kind
	public var bb_closed : Bool; // Whether or not the block has been closed
	/* elements */
	public final bb_el : DynArray<core.Type.TExpr>; // The block expressions
	public final bb_phi : DynArray<core.Type.TExpr>; // SSA-phi expressions
	/* relations */
	public var bb_outgoing : ImmutableList<Cfg_edge>; // Outgoing edges
	public var bb_incoming : ImmutableList<Cfg_edge>; // Incoming edges
	public var bb_dominator : BasicBlock; // The block's dominator
	public var bb_dominated : ImmutableList<BasicBlock>; // The dominated blocks
	public var bb_df : ImmutableList<BasicBlock>; // The dominance frontier
	public var bb_syntax_edge : Syntax_edge; // The syntactic edge
	public var bb_loop_groups : ImmutableList<Int>; // The loop groups this block belongs to
	public var bb_scopes : ImmutableList<Int>; // The scopes this block belongs to
	/* variables */
	public var bb_var_writes: ImmutableList<core.Type.TVar>; // List of assigned variables

	public static function s_block_kind (kind:BlockKind) : String {
		return switch(kind) {
			case BKRoot: "BKRoot";
			case BKNormal: "BKNormal";
			case BKFunctionBegin(_): "BKFunctionBegin";
			case BKFunctionEnd : "BKFunctionEnd";
			case BKSub : "BKSub";
			case BKConditional : "BKConditional";
			case BKLoopHead : "BKLoopHead";
			case BKException : "BKException";
			case BKUnreachable : "BKUnreachable";
			case BKCatch(_) : "BKCatch";
		}
	}

	public static function s_cfg_edge_kind (kind:Cfg_edge_kind) : String {
		return switch (kind) {
			case CFGGoto: "CFGGoto";
			case CFGFunction: "CFGFunction";
			case CFGMaybeThrow: "CFGMaybeThrow";
			case CFGCondBranch(e): "CFGCondBranch " + core.Type.s_expr_pretty(false, "", false, core.Type.s_type.bind(core.Type.print_context()), e);
			case CFGCondElse: "CFGCondElse";
		}
	}

	public static function has_flag (edge:Cfg_edge, flag:Cfg_edge_Flag) : Bool {
		return List.mem(flag, edge.cfg_flags);
	}

	/* expressions */
	public static function add_texpr (bb:BasicBlock, e:core.Type.TExpr) : Void {
		bb.bb_el.push(e);
	}

	public static function get_texpr (bb:BasicBlock, is_phi:Bool, i:Int) : core.Type.TExpr {
		return ((is_phi) ? bb.bb_phi : bb.bb_el)[i];
	}

	/* edges */
	public static function set_syntax_edge (bb:BasicBlock, se:Syntax_edge) : Void {
		bb.bb_syntax_edge = se;
	}

	public static function add_cfg_edge (bb_from:BasicBlock, bb_to:BasicBlock, kind:Cfg_edge_kind) : Void {
		if (bb_from.bb_kind != BKUnreachable) {
			var edge:Cfg_edge = {cfg_from:bb_from, cfg_to:bb_to, cfg_kind:kind, cfg_flags:[]};
			bb_from.bb_outgoing = edge :: bb_from.bb_outgoing;
			bb_to.bb_incoming = edge :: bb_to.bb_incoming;
		}
	}

	public static function _create (id:Int, kind:BlockKind, t:core.Type.T, p:core.Globals.Pos) : BasicBlock {
		var bb:BasicBlock = {
			bb_kind : kind,
			bb_id : id,
			bb_type : t,
			bb_pos : p,
			bb_closed : false,
			bb_el : [],
			bb_phi : [],
			bb_outgoing : [],
			bb_incoming : [],
			bb_dominator : null,
			bb_dominated : [],
			bb_df : [],
			bb_syntax_edge : SENone,
			bb_loop_groups : [],
			bb_var_writes : [],
			bb_scopes : []
		}
		bb.bb_dominator = bb;
		return bb;
	}

	public static function in_scope (bb:BasicBlock, bb_:BasicBlock) : Bool {
		return switch (bb_.bb_scopes) {
			case []: context.Common.abort('Scope-less block (kind: ${s_block_kind(bb_.bb_kind)})', bb_.bb_pos);
			case scope :: _: List.mem(scope, bb.bb_scopes);
		}
	}
}

typedef TExpr_lookup = {block:BasicBlock, b:Bool, i:Int}
typedef TFunc_info = {block:BasicBlock, t:core.Type.T, pos:core.Globals.Pos, tf:core.Type.TFunc}
typedef Var_write = ImmutableList<BasicBlock>;
typedef Var_info = {
	vi_var : core.Type.TVar, // The variable itself
	vi_extra : core.Type.TVarExtra, // The original v_extra
	vi_bb_declare : BasicBlock, // The block where this variable was declared
	vi_origin : core.Type.TVar, // The origin variable of this variable
	vi_writes : Var_write, // A list of blocks that assign to this variable
	vi_value : Option<TExpr_lookup>, // The value of this variable, if known
	vi_ssa_edges : ImmutableList<TExpr_lookup>, // The expressions this variable influences
	vi_reaching_def : Option<core.Type.TVar> // The current reaching definition variable of this variable
}

typedef Dom_bb_info = {
	bb : BasicBlock,
	parent : Dom_bb_info,
	idom : Dom_bb_info,
	semi : Int,
	label : Dom_bb_info,
	ancestor : Dom_bb_info,
	bucket : ImmutableList<Dom_bb_info>
}

/*
	A Graph contains all relevant information for a given method. It is built from the field expression
	and then refined in subsequent modules such as Ssa.
*/
@:structInit
class Graph {
	function new (g_root, g_exit, g_unreachable, g_functions, g_nodes, g_var_infos, g_loops) {
		this.g_root = g_root;
		this.g_exit = g_exit;
		this.g_unreachable = g_unreachable;
		this.g_functions = g_functions;
		this.g_nodes = g_nodes;
		this.g_var_infos = g_var_infos;
		this.g_loops = g_loops;
	}
	public var g_root : BasicBlock; // The unique root block
	public var g_exit : BasicBlock; // The unique exit block
	public var g_unreachable : BasicBlock; // The unique unreachable block
	public var g_functions : Hashtbl<Int, TFunc_info>; // A map of functions, indexed by their block IDs
	public var g_nodes : ImmutableList<BasicBlock>; // A list of all blocks
	public final g_var_infos : DynArray<Var_info>; // A map of variable information
	public var g_loops : PMap<Int, BasicBlock>; // A map containing loop information

	/* variables */
	public static function create_var_info (g:Graph, bb:BasicBlock, v:core.Type.TVar) : Var_info {
		var vi:Var_info = {
			vi_var: v,
			vi_extra: v.v_extra,
			vi_bb_declare: bb,
			vi_origin: v,
			vi_writes: Tl,
			vi_value: None,
			vi_ssa_edges: Tl,
			vi_reaching_def: None
		};
		g.g_var_infos.push(vi);
		var i = g.g_var_infos.length - 1;
		v.v_extra = Some({params:Tl, expr:Some(core.Type.mk(TConst(TInt(i)), core.Type.t_dynamic, core.Globals.null_pos))});
		return vi;
	}

	public static function get_var_info (g:Graph, v:core.Type.TVar) : Var_info {
		return switch (v.v_extra) {
			case Some({expr:Some({eexpr:TConst(TInt(i32))})}): g.g_var_infos[i32];
			case _:
				std.Sys.println("Unbound variable, please report this");
				std.Sys.println(core.type.Printer.s_tvar(v));
				create_var_info(g, g.g_unreachable, v);
		}
	}

	public static function declare_var (g:Graph, v:core.Type.TVar, bb:BasicBlock) : Void {
		create_var_info(g, bb, v);
	}

	public static function add_var_def (g:Graph, bb:BasicBlock, v:core.Type.TVar) : Void {
		if (bb.bb_id > 0) {
			bb.bb_var_writes = v :: bb.bb_var_writes;
			var vi = get_var_info(g, v);
			vi.vi_writes = bb :: vi.vi_writes;
		}
	}

	public static function set_var_value (g:Graph, v:core.Type.TVar, bb:BasicBlock, is_phi:Bool, i:Int) : Void {
		get_var_info(g, v).vi_value = Some({block:bb, b:is_phi, i:i});
	}

	public static function get_var_value (g:Graph, v:core.Type.TVar) : core.Type.TExpr {
		var value = get_var_info(g, v).vi_value;
		var _tmp = switch (value) {
			case None: throw ocaml.Not_found.instance;
			case Some(l): l;
		}
		var bb = _tmp.block; var is_phi = _tmp.b; var i = _tmp.i;
		return switch (BasicBlock.get_texpr(bb, is_phi, i).eexpr) {
			case TVar(_,Some(e)), TBinop(OpAssign,_,e): e;
			case _: trace("Shall not be seen"); std.Sys.exit(255); throw false;
		}
	}

	public static function add_var_origin (g:Graph, v:core.Type.TVar, v_origin:core.Type.TVar) : Void {
		get_var_info(g, v).vi_origin = v_origin;
	}

	public static function get_var_origin (g:Graph, v:core.Type.TVar) : core.Type.TVar {
		return get_var_info(g, v).vi_origin;
	}

	public static function add_ssa_edge (g:Graph, v:core.Type.TVar, bb:BasicBlock, is_phi:Bool, i:Int) : Void {
		var vi = get_var_info(g, v);
		vi.vi_ssa_edges = {block:bb, b:is_phi, i:i} :: vi.vi_ssa_edges;
	}

	/* nodes */
	public static inline function add_function (g:Graph, tf:core.Type.TFunc, t:core.Type.T, p:core.Globals.Pos, bb:BasicBlock) : Void {
		Hashtbl.add(g.g_functions, bb.bb_id, {block:bb, t:t, pos:p, tf:tf});
	}

	public static var alloc_id = {
		var r = new Ref(1);
		function () {
			r.set(r.get()+1);
			return r.get();
		}
	}
	public static function create_node (g:Graph, kind:BlockKind, t:core.Type.T, p:core.Globals.Pos) : BasicBlock {
		var bb = BasicBlock._create(alloc_id(), kind, t, p);
		g.g_nodes = bb :: g.g_nodes;
		return bb;
	}

	public static function close_node (g:Graph, bb:BasicBlock) {
		if (bb.bb_id > 0) {
			// assert(not bb.bb_closed);
			if (bb.bb_closed) { trace("Shall not be seen"); std.Sys.exit(255); }
			bb.bb_closed = true;
		}
	}

	public static function iter_dom_tree_from (g:Graph, bb:BasicBlock, f:BasicBlock->Void) : Void {
		function loop (bb:BasicBlock) {
			f(bb);
			List.iter(loop, bb.bb_dominated);
		}
		loop(bb);
	}

	public static function iter_dom_tree (g:Graph, f:BasicBlock->Void) : Void {
		iter_dom_tree_from(g, g.g_root, f);
	}

	public static function iter_edges_from (g:Graph, bb:BasicBlock, f:Cfg_edge->Void) : Void {
		iter_dom_tree_from(g, bb, function (bb:BasicBlock) { List.iter(f, bb.bb_outgoing); });
	}

	public static function iter_edges (g:Graph, f:Cfg_edge->Void) : Void {
		iter_dom_tree(g, function (bb:BasicBlock) { List.iter(f, bb.bb_outgoing); });
	}

	/* graph */

	public static function create (t:core.Type.T, p:core.Globals.Pos) : Graph {
		var bb_root = BasicBlock._create(1, BKRoot, t, p);
		var bb_unreachable = BasicBlock._create(0, BKUnreachable, core.Type.t_dynamic, core.Globals.null_pos);
		return {
			g_root: bb_root,
			g_exit: bb_unreachable,
			g_unreachable: bb_unreachable,
			g_functions: Hashtbl.create(0),
			g_nodes: [bb_root],
			g_var_infos: [],
			g_loops: PMap.empty()
		};
	}

	public static function check_integrity (g:Graph) : Void {
		List.iter(function (bb:BasicBlock) {
			List.iter(function (edge:Cfg_edge) {
				if (edge.cfg_to == g.g_unreachable) {
					std.Sys.println('Outgoing edge from ${bb.bb_id} to the unreachable block');
				}
				else if (!List.memq(edge, edge.cfg_to.bb_incoming)) {
					std.Sys.println('Outgoing edge ${edge.cfg_from.bb_id} -> ${edge.cfg_to.bb_id} has no matching incoming edge');
				}
			}, bb.bb_outgoing);
			List.iter(function (edge:Cfg_edge) {
				if (edge.cfg_from == g.g_unreachable) {
					std.Sys.println('Incoming edge to ${bb.bb_id} to the unreachable block');
				}
				else if (!List.memq(edge, edge.cfg_from.bb_outgoing)) {
					std.Sys.println('Incoming edge ${edge.cfg_to.bb_id} <- ${edge.cfg_from.bb_id} has no matching outgoing edge');
				}
			}, bb.bb_incoming);
		}, g.g_nodes);
	}

	/* inference */

	// type dom_bb_info = { ...

	/*
		Infers the immediate dominators for all reachable blocks. This function can be run multiple times
		in case an update is necessary.
	*/
	public static function infer_immediate_dominators (g:Graph) : Void {
		var info:Hashtbl<Int,Dom_bb_info> = Hashtbl.create(0);
		var nodes = new DynArray();
		function get_info (i:Int) { return Hashtbl.find(info, i); }
		function add_info (bb:BasicBlock, bb_parent:BasicBlock) {
			var bbi:Dom_bb_info = {
				bb: bb,
				parent: null,
				idom: null,
				semi: nodes.length,
				label: null,
				ancestor: null,
				bucket: Tl
			};
			bbi.parent = bbi; bbi.idom = bbi; bbi.label = bbi; bbi.ancestor = bbi;
			var bbi = (bb == bb_parent) ? bbi: bbi.with({parent:get_info(bb_parent.bb_id)});
			Hashtbl.add(info, bb.bb_id, bbi);
			nodes.push(bbi);
		}
		function loop (bb_parent:BasicBlock, bb:BasicBlock) {
			bb.bb_dominated = Tl;
			add_info(bb, bb_parent);
			List.iter(function (edge:Cfg_edge) {
				var bb_to = edge.cfg_to;
				if (!Hashtbl.mem(info, bb_to.bb_id)) {
					loop(bb, bb_to);
				}
			}, bb.bb_outgoing);
		}
		loop(g.g_root, g.g_root);
		function compress (bbi:Dom_bb_info) {
			function loop (l:ImmutableList<Dom_bb_info>, bbi:Dom_bb_info) {
				return (bbi.ancestor == bbi) ? l : loop(bbi::l, bbi.ancestor);
			}
			var worklist = loop([bbi], bbi.ancestor);
			switch (worklist) {
				case a :: worklist:
					List.fold_left(function (tmp:{fst:Dom_bb_info, snd:Int}, bbi_desc:Dom_bb_info) {
						var a = tmp.fst; var min_semi = tmp.snd;
						var bbi = bbi_desc.label;
						if (bbi.semi > min_semi) {
							bbi_desc.label = a.label;
							return {fst:bbi_desc, snd:min_semi};
						}
						else {
							return {fst:bbi_desc, snd:bbi.semi};
						}
					}, {fst:a, snd:a.label.semi}, worklist);
				case []:
					trace("Shall not be seen"); std.Sys.exit(255);
			}
		}
		function eval (v:Int) : Dom_bb_info {
			var bbi = get_info(v);
			return
			if (bbi.ancestor != bbi) {
				compress(bbi);
				bbi.label;
			}
			else {
				bbi;
			}
		}
		function loop2 (nodes_:ImmutableList<Dom_bb_info>) : Void {
			switch (nodes_) {
				case [_]:
				case []: trace("Shall not be seen"); std.Sys.exit(255);
				case w :: nodes_:
					var semi = List.fold_left(function (acc:Int, v:Cfg_edge) : Int {
						return Std.int(Math.min(acc, eval(v.cfg_from.bb_id).semi));
					}, w.semi, w.bb.bb_incoming);
					w.semi = semi;
					var bbi = nodes[semi];
					bbi.bucket = w :: bbi.bucket;
					var bbi_p = w.parent;
					w.ancestor = bbi_p;
					List.iter( function (v:Dom_bb_info) {
						var u = eval(v.bb.bb_id);
						if (u.semi < v.semi) {
							v.idom = u;
						}
						else {
							v.idom = bbi_p;
						}
					}, bbi_p.bucket);
					bbi_p.bucket = Tl;
					loop2(nodes_);
			}
		}
		var l:ImmutableList<Dom_bb_info> = nodes;
		loop2(List.rev(l));
		List.iter(function (w:Dom_bb_info) {
			if (w.idom != nodes[w.semi]) {
				w.idom = w.idom.idom;
			}
		}, List.tl(l));
		// DynArray.iter (fun bbi ->
		for (bbi in nodes) {
			if (bbi.idom != bbi) {
				var bb = bbi.bb;
				var bb_ = bbi.idom.bb;
				if (bb != bb_) {
					bb.bb_dominator = bb_;
					bb_.bb_dominated = bb :: bb_.bb_dominated;
				}
			}
		}
	}

	/* Infers the dominance frontier for all reachable blocks. This function should only be run once. */
	public static function infer_dominance_frontier (g:Graph) : Void {
		iter_edges(g, function (edge:Cfg_edge) {
			function loop(bb:BasicBlock) {
				if (bb != g.g_unreachable && bb != edge.cfg_to && bb != edge.cfg_to.bb_dominator) {
					if (edge.cfg_to != g.g_exit) { bb.bb_df = edge.cfg_to :: bb.bb_df; }
					if (bb.bb_dominator != bb) { loop(bb.bb_dominator); }
				}
			}
			loop(edge.cfg_from);
		});
	}

	/* Infers variable declarations and definitions. This function should only be run once. */
	public static function infer_var_writes (g:Graph) : Void {
		iter_dom_tree(g, function (bb:BasicBlock) {
			switch (bb.bb_kind) {
				case BKCatch(v):
					declare_var(g, v, bb);
					add_var_def(g, bb, v);
				case BKFunctionBegin(tf):
					List.iter(function (arg) {
						var v = arg.v;
						declare_var(g, v, bb);
						add_var_def(g, bb, v);
					}, tf.tf_args);
				case _:
			}
			for (e in bb.bb_el) {
				switch (e.eexpr) {
					case TVar(v, eo):
						declare_var(g, v, bb);
						if (eo != None) { add_var_def(g, bb, v); }
					case TBinop(OpAssign, {eexpr:TLocal(v)}, _):
						add_var_def(g, bb, v);
					case _:
				}
			}
		});
	}

	/* Infers the scopes of all reachable blocks. This function can be run multiple times
		in case an update is necessary */
	public static function infer_scopes (g:Graph) : Void {
		var next_scope_id = new Ref(0);
		function next_scope(scopes:ImmutableList<Int>) : ImmutableList<Int> {
			next_scope_id.set(next_scope_id.get() + 1);
			return next_scope_id.get() :: scopes;
		}
		function loop(scopes:ImmutableList<Int>, bb:BasicBlock) {
			bb.bb_scopes = scopes;
			switch (bb.bb_syntax_edge) {
				case SEIfThen(bb_then, bb_next, _):
					loop(next_scope(scopes), bb_then);
					loop(scopes, bb_next);
				case SEIfThenElse(bb_then, bb_else, bb_next,_,_):
					loop(next_scope(scopes), bb_then);
					loop(next_scope(scopes), bb_else);
					loop(scopes, bb_next);
				case SESwitch(cases, bbo, bb_next, _):
					List.iter(function (c) { var bb_case = c.block; loop(next_scope(scopes), bb_case); }, cases);
					switch (bbo) { case None: case Some(bb): loop(next_scope(scopes), bb); }
					loop(scopes, bb_next);
				case SETry(bb_try, bb_exc, catches, bb_next, _):
					var scopes_ = next_scope(scopes);
					loop(scopes_, bb_try);
					loop(scopes_, bb_exc);
					List.iter(function (c) { var bb_catch = c.block; loop(next_scope(scopes), bb_catch); }, catches);
					loop(scopes, bb_next);
				case SEWhile(bb_head, bb_body, bb_next):
					var scopes_ = next_scope(scopes);
					loop(scopes_, bb_head);
					loop(scopes_, bb_body);
					loop(scopes, bb_next);
				case SESubBlock(bb_sub, bb_next):
					loop(next_scope(scopes), bb_sub);
					loop(scopes, bb_next);
				case SEMerge(bb):
					loop(scopes, bb);
				case SENone:
			}
		}
		Hashtbl.iter(function (_, fi) { var bb = fi.block; loop([0], bb); }, g.g_functions);
	}
}

class AnalyzerTypes {
}