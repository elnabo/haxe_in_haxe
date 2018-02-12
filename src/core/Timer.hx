package core;

import haxe.ds.ImmutableList;
import ocaml.List;
import ocaml.Ref;

typedef TimerInfos = {
	id : ImmutableList<String>,
	start : ImmutableList<Float>,
	total : Float,
	calls : Int
}

class Timer {

	public static var get_time : Void->Float = std.Sys.time;
	public static var htimers : Map<String, TimerInfos> = new Map<String, TimerInfos>();
	public static var curtime = new Ref<ImmutableList<TimerInfos>>([]);

	public static function new_timer (id:ImmutableList<String>) : TimerInfos {
		// let key = String.concat "." id in
		var key = List.join(".", id);
		var t = htimers.get(key);
		if (t != null) {
			t.start = get_time() :: t.start; 
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
		var start = switch (t.start) {
			case []: throw false;
			case s::l:
				t.start = l;
				s;
		}
		var now = get_time();
		var dt = now - start;
		t.total = t.total + dt;
		function loop() {
			switch (curtime.get()) {
				case []: throw "Timer " + List.join(".", t.id) + " closed while not active";
				case tt::l:
					curtime.set(l);
					if (t != tt) {
						loop();
					}
			}
		}
		loop();
		// because of rounding errors while adding small times, we need to make sure that we don't have start > now 
		List.iter(function (ct) {
			ct.start = List.map(function (t) {
				var s = t + dt;
				return (s > now) ? now : s;
			}, ct.start);
		}, curtime.get());
	}

	public static function timer (id:ImmutableList<String>) : Void->Void {
		var t = new_timer(id);
		curtime.set(t::curtime.get());
		return function () { core.Timer.close(t); };
	}

	public static function close_times () : Void {
		switch (curtime.get()) {
			case []:
			case t::_:
				close(t);
				close_times();
		}
	}

	public static function report_times (print:String->Void) : Void {
		trace("TODO core.Timer.report_times");
	}


}