package ocaml;

//  from https://github.com/lucasaiu/ocaml/blob/master/stdlib/arg.ml

enum ArgError {
	Unknown(s:String);
	Missing(s:String);
	Wrong(opt:String, arg:String, expected:String);
	Message(s:String);
}

class Bad {
	public var s:String;
	public function new (s:String) {
		this.s = s;
	}
}

class Help {
	public var s:String;
	public function new (s:String) {
		this.s = s;
	}
}

class Stop {
	public var e:ArgError;
	public function new (e:ArgError) {
		this.e = e;
	}
}

enum Spec {
	S_Unit(f:Void->Void); // Call the function with unit argument
	S_Bool(f:Bool->Void); // Call the function with a bool argument
	S_Set (b:Bool); // Set the reference to true
	S_Clear (b:Bool); // Set the reference to false
	S_String (f:String->Void); // Call the function with a string argument
	S_Set_string (s:String); // Set the reference to the string argument
	S_Int (f:Int->Void); // Call the function with an int argument
	S_Set_int (i:Int); // Set the reference to the int argument
	S_Float(f:Float->Void); // Call the function with a float argument
	S_Set_float(f:Float); // Set the reference to the float argument
	S_Tuple (l:Array<Spec>); // Take several arguments according to the spec list
	S_Symbol (l:Array<String>, f:String->Void); // Take one of the symbols as argument and call the function with the symbol.
	S_Rest (f:String->Void); // Stop interpreting keywords and call the function with each remaining argument
}

class Arg {

	public static function assoc3(x:String, l:Array<{arg:String, spec:Spec, doc:String}>) : Spec {
		if (l.length == 0) {
			throw Not_found.instance;
		}
		else {
			for (s in l) {
				if (s.arg == x) {
					return s.spec;
				}
			}
		}
		throw Not_found.instance;
	}

	public static function make_symlist(prefix:String, sep:String, suffix:String, l:Array<String>) : String {
		if (l.length == 0) {
			return "<none>";
		}
		else {
			var x = prefix + l[0];
			for (i in 1...l.length) {
				x = x + sep + l[i];
			}
			return x;
		}
	}

	public static function print_spec(buf:StringBuf, spec:{arg:String, spec:Spec, doc:String}) {
		if (spec.doc.length > 0) {
			buf.add("  ");
			buf.add(spec.arg);
			switch (spec.spec) {
				case S_Symbol(l,_):
					buf.add(make_symlist("{", "|", "}", l));
				default:
			}
			buf.add(spec.doc);
			buf.add("\n");
		}
	}

	public static function help_action () {
		throw new Stop(Unknown("-help"));
	}

	public static function add_help(speclist:Array<{arg:String, spec:Spec, doc:String}>) {
		try {
			assoc3("-help", speclist);
		}
		catch (e:Not_found) {
			speclist.push({arg:"-help", spec:S_Unit(help_action), doc:" Display this list of options"});
		}
		try {
			assoc3("--help", speclist);
		}
		catch (e:Not_found) {
			speclist.push({arg:"--help", spec:S_Unit(help_action), doc:" Display this list of options"});
		}
		return speclist;

	}

	public static function usage_b (buf:StringBuf, speclist:Array<{arg:String, spec:Spec, doc:String}>, errmsg:String) {
		buf.add(errmsg);
		buf.add("\n");
		for (s in add_help(speclist)) {
			print_spec(buf, s);
		}
	}
	
	public static function usage_string (speclist:Array<{arg:String, spec:Spec, doc:String}>, errmsg:String) {
		var b = new StringBuf();
		usage_b(b, speclist, errmsg);
		return b.toString();
	}
	
	public static function parse_argv(?current:Int=0, argv:Array<String>, speclist:Array<{arg:String, spec:Spec, doc:String}>, anon_fun:String->Void, errmsg:String) : Int {
		var l = argv.length;
		var b = new StringBuf();
		var initpos = current;
		var stop = function (error:ArgError) : Void {
			var progname = (initpos < l) ? argv[initpos] : "(?)";
			switch (error) {
				case Unknown(s):
					switch(s) {
						case "-help", "--help":
						default: 
							b.add(progname);
							b.add(": unknown option `");
							b.add(s);
							b.add("'.\n");
					}
				case Missing(s):
					b.add(progname);
					b.add(": option `");
					b.add(s);
					b.add("' needs an argument.\n");
				case Wrong(opt, arg, expected):
					b.add(progname);
					b.add(": wrong argument `");
					b.add(arg);
					b.add("'; option `");
					b.add(opt);
					b.add("' expects ");
					b.add(expected);
					b.add(".\n");
				case Message(s):
					b.add(progname);
					b.add(": ");
					b.add(s);
					b.add(".\n");
			};
			usage_b(b,speclist,errmsg);
			switch (error) {
				case Unknown(s):
					if (s == "-help" || s == "--help") {
						throw new Help(b.toString());
					}
					else {
						throw new Bad(b.toString());
					}
				default:
					throw new Bad(b.toString());
			}
		};
		current++;
		while (current < l) {
			var s = argv[current];
			if (s.length >= 1 && s.charAt(0) == "-") {
				var action : Spec = null;
				try {
					action = assoc3(s, speclist);
				}
				catch (e:Exception) {
					stop(Unknown(s));
				}

				try {
					function treat_action (action:Spec) {
						switch (action) {
							case S_Unit(f): f();
							case S_Bool (f): 
								if (current + 1 < l) {
									current++;
									var arg = argv[current];
									f((arg == "true") ? true : (arg == "false") ? false : throw new Stop(Wrong(s, arg, "a boolean")));
								}
								else {
									throw new Stop(Missing(s));
								}
							case S_Set(_): trace("S_Set do not work in haxe AFAIK"); Sys.exit(1);
							case S_Clear(_): trace("S_Clear do not work in haxe AFAIK"); Sys.exit(1);
							case S_String(f):
								if (current + 1 < l) {
									current++;
									f(argv[current]);
								}
								else {
									throw new Stop(Missing(s));
								}
							case S_Symbol (symb, f):
								if (current + 1 < l) {
									var arg = argv[current+1];
									if (symb.indexOf(arg) != -1) {
										f(arg);
										current++;
									}
									else {
										throw new Stop(Wrong(s, arg, "one of: "+make_symlist("", " ", "", symb)));
									}
								}
								else {
									throw new Stop(Missing(s));
								}
							case S_Set_string (_): trace("S_Set_string do not work in haxe AFAIK"); Sys.exit(1);
							case S_Int (f): 
								if (current + 1 < l) {
									current++;
									var arg = argv[current];
									var v = Std.parseInt(arg);
									if (v != null) {
										f(v);
									}
									else {
										throw new Stop(Wrong(s, arg, "an integer"));
									}
								}
								else {
									throw new Stop(Missing(s));
								}
							case S_Set_int (_): trace("S_Set_int do not work in haxe AFAIK"); Sys.exit(1);
							case S_Float (f): 
								if (current + 1 < l) {
									current++;
									var arg = argv[current];
									var v = Std.parseFloat(arg);
									if (v != Math.NaN) {
										f(v);
									}
									else {
										throw new Stop(Wrong(s, arg, "a float"));
									}
								}
								else {
									throw new Stop(Missing(s));
								}
							case S_Set_float (_): trace("S_Set_float do not work in haxe AFAIK"); Sys.exit(1);
							case S_Tuple (specs):
								for (a in specs) {
									treat_action(a);
								}
							case S_Rest (f):
								while (current < l -1) {
									current++;
									f(argv[current]);
								}
						}
					}
					treat_action(action);
				}
				catch (e:Bad) {
					stop(Message(e.s));
				}
				catch (e:Stop) {
					stop(e.e);
				}
			}
			else {
				try {
					anon_fun(s);
				}
				catch (e:Bad) {
					stop(Message(e.s));
				}
			}
			current++;
		}
		return current;
	}
}