package compiler;

import context.Common;
import context.Common.CompilerMessage;

import core.Globals.*;

typedef Context = {
	com : context.Common.Context,
	flush : Void -> Void,
	setup : Void -> Void,
	messages : Array<CompilerMessage>,
	has_next : Bool,
	has_error : Bool
}

class Server {

	public static var measure_times : Bool = false;
	public static var prompt : Bool = false;
	public static var start_time : Float = core.Timer.get_time();

	public static function do_connect (host:String, port:Int, args:Array<String>) : Void {

	}

	public static function parse_hxml (file:String) : Array<String> {
		return null;
	}

	public static function is_debug_run () : Bool {
		// Sys.getenv "HAXEDEBUG" = "1" with _ -> false
		try {
			return std.Sys.getEnv("HAXEDEBUG") == "1";
		}
		catch (_:Dynamic) {
			return false;
		}
	}

	public static function s_version () : String {
		var str = version_major + "." + version_minor + "." + version_revision;
		str += switch (compiler.Version.version_extra) {
			case Some(v) : " " + v;
			case None : "";
		};
		return str;
	}

	public static function default_flush (ctx:Context) {

		var messages = ocaml.List.rev(ctx.messages);
		
		var printer = function(msg:context.CompilerMessage) {
				switch (msg) {
					case CMInfo(_):
						std.Sys.println(context.Common.compiler_message_string(msg));
					case CMWarning(_), CMError(_):
						var stderr = std.Sys.stderr();
						stderr.writeString(context.Common.compiler_message_string(msg)+"\n");
						stderr.flush();
						stderr.close();
				}
				 return null;
		};
		for (msg in messages) {
			printer(msg);
		}
		if (ctx.has_error && prompt) {
			std.Sys.println("Press enter to exit...");
			//ignore(read_line());
			std.Sys.stdout().flush();
			std.Sys.stdout().close();
			std.Sys.stdin().readLine();
			std.Sys.stdin().close();
		}
		
		if (ctx.has_error) {
			std.Sys.exit(1);
		}
	}

	public static function createContext(params:Array<String>) : Context {
		var ctx = {
			com : Common.create(version, s_version, params),
			flush : function () {},
			setup : function () {},
			messages : [],
			has_next : false,
			has_error : false
		};
		ctx.flush = function () {
			default_flush(ctx);
		};
		return ctx;
	}

	public static function wait_loop (process_params:Dynamic, verbose:Bool, accept:Dynamic) : Dynamic {
		trace("TODO: Server.wait_loop");
		return null;
	}

	public static function init_wait_stdio () : Dynamic {
		trace("TODO: Server.init_wait_stdio");
		return null;
		// set_binary_mode_in stdin true;
		// set_binary_mode_out stderr true;

		// let chin = IO.input_channel stdin in
		// let cherr = IO.output_channel stderr in

		// let berr = Buffer.create 0 in
		// let read = fun () ->
		// 	let len = IO.read_i32 chin in
		// 	IO.really_nread_string chin len
		// in
		// let write = Buffer.add_string berr in
		// let close = fun() ->
		// 	IO.write_i32 cherr (Buffer.length berr);
		// 	IO.nwrite_string cherr (Buffer.contents berr);
		// 	IO.flush cherr
		// in
		// fun() ->
		// 	Buffer.clear berr;
		// 	read, write, close
	}

	public static function init_wait_socket (verbose:Bool, host:String, port:Int) : Dynamic {
		trace("TODO: Server.init_wait_socket");
		return null;
		// let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
		// (try Unix.setsockopt sock Unix.SO_REUSEADDR true with _ -> ());
		// (try Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_of_string host,port)) with _ -> failwith ("Couldn't wait on " ^ host ^ ":" ^ string_of_int port));
		// if verbose then print_endline ("Waiting on " ^ host ^ ":" ^ string_of_int port);
		// Unix.listen sock 10;
		// let bufsize = 1024 in
		// let tmp = Bytes.create bufsize in
		// let accept() = (
		// 	let sin, _ = Unix.accept sock in
		// 	Unix.set_nonblock sin;
		// 	if verbose then print_endline "Client connected";
		// 	let b = Buffer.create 0 in
		// 	let rec read_loop count =
		// 		try
		// 			let r = Unix.recv sin tmp 0 bufsize [] in
		// 			if r = 0 then
		// 				failwith "Incomplete request"
		// 			else begin
		// 				if verbose then Printf.printf "Reading %d bytes\n" r;
		// 				Buffer.add_subbytes b tmp 0 r;
		// 				if Bytes.get tmp (r-1) = '\000' then
		// 					Buffer.sub b 0 (Buffer.length b - 1)
		// 				else
		// 					read_loop 0
		// 			end
		// 		with Unix.Unix_error((Unix.EWOULDBLOCK|Unix.EAGAIN),_,_) ->
		// 			if count = 100 then
		// 				failwith "Aborting inactive connection"
		// 			else begin
		// 				if verbose then print_endline "Waiting for data...";
		// 				ignore(Unix.select [] [] [] 0.05); (* wait a bit *)
		// 				read_loop (count + 1);
		// 			end
		// 	in
		// 	let read = fun() -> (let s = read_loop 0 in Unix.clear_nonblock sin; s) in
		// 	let write s = ssend sin (Bytes.unsafe_of_string s) in
		// 	let close() = Unix.close sin in
		// 	read, write, close
		// ) in
		// accept
	}
}