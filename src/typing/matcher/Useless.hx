package typing.matcher;

import haxe.ds.ImmutableList;
import ocaml.List;

enum EUseless {
	False;
	Pos(p:core.Globals.Pos);
	True;
}

typedef ListListPattern = ImmutableList<ImmutableList<Pattern>>;
typedef ListPattern = ImmutableList<Pattern>;

class Useless {

	// U part

	public static function specialize (is_tuple:Bool, con:Constructor, pM:ListListPattern) : ListListPattern{
		function loop (acc:ListListPattern, pM:ListListPattern) {
			return switch (pM) {
				case patterns :: pM:
					switch (patterns) {
						case {t:PatConstructor(con_, patterns1)} :: patterns2 if (!is_tuple && Constructor.equal(con, con_)):
							loop(List.append(patterns1, patterns2) :: acc, pM);
						case {t:PatTuple(patterns1)} :: patterns2 if (is_tuple):
							loop(List.append(patterns1, patterns2) :: acc, pM);
						case {t:PatAny, pos:p} :: patterns2:
							var patterns1 = List.make(Constructor.arity(con), ({t:PatAny, pos:p} : Pattern));
							loop(List.append(patterns1, patterns2) :: acc, pM);
						case {t:PatBind(_, pat1)} :: patterns2:
							loop(acc, (pat1::patterns)::pM);
						case _: loop(acc, pM);
					}
				case []:
					List.rev(acc);
			}
		}
		return loop([], pM);
	}

	public static function _default (pM:ListListPattern) : ListListPattern {
		function loop (acc:ListListPattern, pM:ListListPattern) {
			return switch (pM) {
				case patterns :: pM:
					switch (patterns) {
						case {t:(PatConstructor(_)|PatTuple(_))} :: _:
							loop(acc, pM);
						case {t:(PatVariable(_)|PatAny)} :: patterns:
							loop(patterns::acc, pM);
						case _:
							loop(acc, pM);
					}
				case []:
					List.rev(acc);
			}
		}
		return loop([], pM);
	}

	public static function u(pM:ListListPattern, q:ListPattern) : Bool {
		return switch [q, pM] {
			case [[], []]: true;
			case [[], _]: false;
			case [q1::ql, _]:
				function loop (pat:Pattern) : Bool {
					return switch (pat.t) {
						case PatConstructor(con, patterns):
							var s = specialize(false, con, pM);
							u(s, List.append(patterns, ql));
						case PatTuple(patterns):
							var s = specialize(true, ConConst(TNull), pM);
							u(s, List.append(patterns, ql));
						case (PatVariable(_)| PatAny):
							var d = _default(pM);
							u(d, ql);
						case PatOr(pat1, pat2):
							u(pM, pat1::ql) || u(pM, pat2::ql);
						case PatBind(_, pat1):
							loop(pat1);
						case PatExtractor(_):
							true; // ?
					}
				}
				loop(q1);
		}
	}

	// U' part
	public static function transfer_column (source:ListListPattern, target:ListListPattern) : {fst:ListListPattern, snd:ListListPattern} {
		var _tmp = List.fold_left2 (function (tmp:{fst:ListListPattern, snd:ListListPattern}, patterns1:ListPattern, patterns2:ListPattern) {
			var source = tmp.fst; var target = tmp.snd;
			return switch (patterns1) {
				case pat :: patterns:
					{fst:patterns :: source, snd: (pat::patterns2) :: target};
				case []: {fst:source, snd:target};
			}
		}, {fst:Tl, snd:Tl}, source, target);
		var source = _tmp.fst; var target = _tmp.snd;
		return {fst:List.rev(source), snd:List.rev(target)};
	}

	public static function copy (p:ListListPattern) : ListListPattern {
		return List.map(function (_) { return Tl; }, p);
	}

	public static function specialize_(is_tuple:Bool, con:Constructor, pM:ListListPattern, qM:ListListPattern, rM:ListListPattern) : {fst:ListListPattern, snd:ListListPattern, trd:ListListPattern} {
		var arity = Constructor.arity(con);
		function loop (pAcc:ListListPattern, qAcc:ListListPattern, rAcc:ListListPattern, pM:ListListPattern, qM:ListListPattern, rM:ListListPattern) {
			return switch [pM, qM, rM] {
				case [p1::pM, q1::qM, r1::rM]:
					function loop2 (p1:ListPattern) {
						return switch (p1) {
							case ({t:PatConstructor(con_, patterns1)}) :: patterns2 if (!is_tuple && Constructor.equal(con, con_)):
								loop((List.append(patterns1, patterns2)::pAcc), q1::qAcc, r1::rAcc, pM, qM, rM);
							case {t:PatTuple(patterns1)} :: patterns2 if (is_tuple):
								loop((List.append(patterns1, patterns2)::pAcc), q1::qAcc, r1::rAcc, pM, qM, rM);
							case {t:(PatVariable(_)|PatAny), pos:p} :: patterns2:
								var patterns1 = List.make(arity, ({t:PatAny, pos:p} : Pattern));
								loop((List.append(patterns1, patterns2)::pAcc), q1::qAcc, r1::rAcc, pM, qM, rM);
							case {t:PatOr(pat1, pat2)} :: patterns2:
								loop(pAcc, qAcc, rAcc, (pat1 :: patterns2) :: (pat2::patterns2) :: pM, q1::q1::qM, r1::r1::rM);
							case{t:PatBind(_, pat1)} :: patterns2:
								loop2(pat1::patterns2);
							case _:
								loop(pAcc, qAcc, rAcc, pM, qM, rM);
						}
					}
					loop2(p1);
				case [[], _, _]:
					{fst:List.rev(pAcc), snd:List.rev(qAcc), trd:List.rev(rAcc)};
				case _:
					trace("Shall not be seen"); std.Sys.exit(255); throw false;
			}
		}
		return loop(Tl, Tl, Tl, pM, qM, rM);
	}

	public static function combine (et1:{et:EUseless, p:core.Globals.Pos}, et2:{et:EUseless, p:core.Globals.Pos}) : EUseless{
		return switch [et1.et, et2.et] {
			case [True, True]: True;
			case [False, False]: False;
			case [True, False]: Pos(et2.p);
			case [False, True]: Pos(et1.p);
			case [True, Pos(_)]: et2.et;
			case [Pos(_), True]: et1.et;
			case [False, Pos(_)]: Pos(et1.p);
			case [Pos(_), _]: et1.et;
		}
	}

	public static function u_ (pM:ListListPattern, qM:ListListPattern, rM:ListListPattern, p:ListPattern, q:ListPattern, r:ListPattern) : EUseless {
		return switch (p) {
			case []:
				switch (r) {
					case []:
						(u(qM, q)) ? True :  False;
					case _:
						List.fold_left(function (tmp:{fst:Int, snd:EUseless}, pat:Pattern) {
							var i = tmp.fst; var et = tmp.snd;
							return switch (pat.t) {
								case PatOr(pat1, pat2):
									function process_row(i:Int, l:ListPattern, q:ListPattern)  : {fst:Pattern, snd:ListPattern} {
										function loop(acc:ListPattern, k:Int, l:ListPattern) : {fst:Pattern, snd:ListPattern} {
											return switch (l) {
												case x :: l if (i == k):
													{fst:x, snd:List.append(List.rev(acc), List.append(l, q))};
												case x :: l:
													loop(x::acc, k+1, l);
												case []: trace("Shall not be seen"); std.Sys.exit(255); throw false;
											}
										}
										return loop(Tl, 0, l);
									}
									var _tmp = List.fold_left2 (function (tmp:{fst:ListListPattern, snd:ListListPattern}, r:ListPattern, q:ListPattern) {
										var col = tmp.fst; var mat = tmp.snd;
										var _tmp = process_row(i, r, q);
										var x = _tmp.fst; var l = _tmp.snd;
										return {fst:([x] : ListPattern)::col, snd:l::mat};
									}, {fst:Tl, snd:Tl}, rM, qM);
									var col = _tmp.fst; var mat = _tmp.snd;
									var col = List.rev(col); var mat = List.rev(mat);
									var r = process_row(i, r, q).snd;
									var et1 = u_(col, mat, copy(mat), [pat1], r, []);
									var qM = List.append(mat, [r]);
									var et2 = u_(List.append(col, [[pat1]]), qM, copy(qM), [pat2], r, []);
									var et3 = combine({et:et1, p:pat1.pos}, {et:et2, p:pat2.pos});
									var p = core.Ast.punion(pat1.pos, pat2.pos);
									var et = combine({et:et, p:p}, {et:et3, p:p});
									{fst:i+1, snd:et};
								case _: trace("Shall not be seen"); std.Sys.exit(255); throw false;
							}
						}, {fst:0, snd:True}, r).snd;
				}
			case (pat :: pl):
				function loop (pat:Pattern) : EUseless {
					return switch (pat.t) {
						case PatConstructor(con, patterns):
							var _tmp = specialize_(false, con, pM, qM, rM);
							var pM = _tmp.fst; var qM = _tmp.snd; var rM = _tmp.trd;
							u_(pM, qM, rM, List.append(patterns, pl), q, r);
						case PatTuple(patterns):
							var _tmp = specialize_(true, ConConst(TNull), pM, qM, rM);
							var pM = _tmp.fst; var qM = _tmp.snd; var rM = _tmp.trd;
							u_(pM, qM, rM, List.append(patterns, pl), q, r);
						case PatAny, PatVariable(_):
							var _tmp = transfer_column(pM, qM);
							var pM = _tmp.fst; var qM = _tmp.snd;
							u_(pM, qM, rM, pl, pat::q, r);
						case PatOr(_):
							var _tmp = transfer_column(pM, rM);
							var pM = _tmp.fst; var rM = _tmp.snd;
							u_(pM, qM, rM, pl, q, pat::r);
						case PatBind(_, pat1):
							loop(pat1);
						case PatExtractor(_):
							True;
					}
				}
				loop(pat);
		}
	}
	// Sane part

	public static function check_case (com:context.Common.Context, p:ImmutableList<Case>, c:Case) {
		var _case = c.fst; var bindings = c.snd; var patterns = c.trd;
		var p = List.map(function (c:Case) { var patterns = c.trd; return patterns; }, p);
		switch (u_(p, copy(p), copy(p), patterns, Tl, Tl)) {
			case False: com.warning("This case is unused", _case.case_pos);
			case Pos(p): com.warning("This pattern is unused", p);
			case True:
		}
	}

	public static function check (com:context.Common.Context, cases) : Void {
		List.fold_left(function (acc:ImmutableList<Case>, c:Case) {
			var case_ = c.fst; var bindings = c.snd; var patterns = c.trd;
			check_case(com, acc, (c));
			return
			if (case_.case_guard == None) {
				List.append(acc, [c]);
			}
			else {
				acc;
			}
		}, [], cases);
	}
}