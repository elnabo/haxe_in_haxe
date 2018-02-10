package core;

typedef TimerInfos = {
	id : Array<String>,
	start : Array<Float>,
	total : Float,
	calls : Int
}

class Timer {

	public static var get_time : Void->Float = std.Sys.time;
	public static var htimers : Map<String, TimerInfos> = new Map<String, TimerInfos>();
	public static var curtime : Array<TimerInfos> = [];

	public static function new_timer (id:Array<String>) : TimerInfos {
		// let key = String.concat "." id in
		var key = id.join(".");
		var t = htimers.get(key);
		if (t != null) {
			t.start.unshift(get_time()); 
			t.calls++;
			return t;
		}
		else {
			t = {
				id : id,
				start : [get_time()],
				total : 0.0,
				calls : 1
			}
			htimers.set(key, t);
			return t;
		}
	}

	public static function close (t:TimerInfos) : Void {
		if (t.start.length == 0) {
			// assert false;
			throw false;
		}
		var start = t.start.shift();
		var now = get_time();
		var dt = now - start;
		t.total = t.total + dt;
		while (true) {
			if (curtime.length == 0) {
				throw "Timer " + t.id.join(".") + " closed while not active";
			}
			else {
				var tt = curtime.shift();
				if (t.id.toString() == tt.id.toString() && t.start.toString() == tt.start.toString() && t.total == tt.total && t.calls == tt.calls) {
					break;
				}
			}
		}
		// let rec loop() =
		// 	match !curtime with
		// 	| [] -> failwith ("Timer " ^ (String.concat "." t.id) ^ " closed while not active")
		// 	| tt :: l -> curtime := l; if t != tt then loop()
		// in
		// loop();
		// if (curtime.length == 0) {
		// 	throw "Timer " + t.id.join(".") + " closed while not active";
		// }
		// for (tt in curtime) {
		// 	if (t.id.toString() == tt.id.toString() && t.start.toString() == tt.start.toString() && t.total == tt.total && t.calls == tt.calls) {
		// 		curtime.remove(tt);
		// 		break;
		// 	}
		// }
		
		// because of rounding errors while adding small times, we need to make sure that we don't have start > now 
		for (ct in curtime) {
			ct.start = [for (t in ct.start) {
				var s = t + dt;
				(s > now) ? now : s;
			}];
		}
	}

	public static function timer (id:Array<String>) : Void->Void {
		var t = new_timer(id);
		curtime.unshift(t);
		return function () { core.Timer.close(t); };
	}

	public static function close_times () : Void {
		for (ct in curtime) {
			close(ct);
		}
	}

	public static function report_times (print:String->Void) : Void {
		trace("TODO core.Timer.report_times");
	}


}