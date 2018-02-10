package context.common;

import haxe.ds.Option;

typedef Cache = {
	c_haxelib : Map<Array<String>, Array<String>>,
	c_files : Map<{file:String, sign:String}, {f:Float, data:core.Ast.Package}>,
	c_modules : Map<{path:core.Path, s:String}, core.Type.ModuleDef>,
	c_directories : Map<String, Array<{s:String, e:{s:String, f:Float}}>>
}


enum ContextOptions {
	NormalContext;
	MacroContext;
	NormalAndMacroContext;
}


class CompilationServer {

	public static var instance : Option<context.common.compilationserver.T>;

	public static function create_cache () : Cache {
		return {
			c_haxelib : new Map<Array<String>, Array<String>>(),
			c_files : new Map<{file:String, sign:String}, {f:Float, data:core.Ast.Package}>(),
			c_modules : new Map<{path:core.Path, s:String}, core.Type.ModuleDef>(),
			c_directories : new Map<String, Array<{s:String, e:{s:String, f:Float}}>>()
		}
	}

	public static function create () : context.common.compilationserver.T {
		var cs = {
			cache : create_cache(),
			signs : []
		};
		instance = Some(cs);
		return cs;
	}

	public static function get () : Option<context.common.compilationserver.T> {
		return instance;
	}

	public static function runs () : Bool {
		return instance == None;
	}

// let get_context_files cs signs =
// 	Hashtbl.fold (fun (file,sign) (_,data) acc ->
// 		if (List.mem sign signs) then (file,data) :: acc
// 		else acc
// 	) cs.cache.c_files []

// (* signatures *)

// let get_sign cs sign =
// 	List.assoc sign cs.signs

// 	let add_sign cs sign =
// 		let i = string_of_int (List.length cs.signs) in
// 		cs.signs <- (sign,i) :: cs.signs;
// 		i

// 	(* modules *)

// 	let find_module cs key =
// 		Hashtbl.find cs.cache.c_modules key

// 	let cache_module cs key value =
// 		Hashtbl.replace cs.cache.c_modules key value

// 	let taint_modules cs file =
// 		Hashtbl.iter (fun _ m -> if m.m_extra.m_file = file then m.m_extra.m_dirty <- Some m) cs.cache.c_modules

// 	(* files *)

// 	let find_file cs key =
// 		Hashtbl.find cs.cache.c_files key

// 	let cache_file cs key value =
// 		Hashtbl.replace cs.cache.c_files key value

// 	let remove_file cs key =
// 		Hashtbl.remove cs.cache.c_files key

// 	let remove_files cs file =
// 		List.iter (fun (sign,_) -> remove_file cs (sign,file)) cs.signs

// 	(* haxelibs *)

// 	let find_haxelib cs key =
// 		Hashtbl.find cs.cache.c_haxelib key

// 	let cache_haxelib cs key value =
// 		Hashtbl.replace cs.cache.c_haxelib key value

// 	(* directories *)

// 	let find_directories cs key =
// 		Hashtbl.find cs.cache.c_directories key

// 	let add_directories cs key value =
// 		Hashtbl.replace cs.cache.c_directories key value

// 	let remove_directory cs key value =
// 		try
// 			let current = find_directories cs key in
// 			Hashtbl.replace cs.cache.c_directories key (List.filter (fun (s,_) -> s <> value) current);
// 		with Not_found ->
// 			()

// 	let has_directory cs key value =
// 		try
// 			List.mem_assoc value (find_directories cs key)
// 		with Not_found ->
// 			false

// 	let add_directory cs key value =
// 		try
// 			let current = find_directories cs key in
// 			add_directories cs key (value :: current)
// 		with Not_found ->
// 			add_directories cs key [value]

// 	let clear_directories cs key =
// 		Hashtbl.remove cs.cache.c_directories key

}