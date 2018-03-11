package optimization;

import haxe.ds.ImmutableList;
import haxe.ds.Option;
import ocaml.DynArray;
import ocaml.Hashtbl;
import ocaml.PMap;
import ocaml.Ref;

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


	/* expressions */
	public static function add_texpr (bb:BasicBlock, e:core.Type.TExpr) : Void {
		bb.bb_el.push(e);
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
	vi_ssa_edges : Option<TExpr_lookup>, // The expressions this variable influences
	vi_reaching_def : Option<core.Type.TVar> // The current reaching definition variable of this variable
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
}

class AnalyzerTypes {
}