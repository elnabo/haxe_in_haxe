package neko;

import haxe.ds.Option;

import neko.Nast.Constant;
import neko.Nast.Expr;

import ocaml.Hashtbl;
import ocaml.List;

import sys.io.FileOutput;

typedef Context = {
	ch: FileOutput,
	curfile: String,
	curline: Int,
	scount: Int,
	strings: Hashtbl<String,Int>
}

class Binast {

	public static function b (ctx:Context, n:Int) : Void {
		ctx.ch.writeByte(n);
	}

	public static function write_ui24 (ctx:Context, n:Int) : Void {
		// ctx.ch.writeByte(n);
		// ctx.ch.writeByte(n >>> 8);
		// ctx.ch.writeByte(n >>> 16);
		ctx.ch.writeByte(n & 255);
		ctx.ch.writeByte((n >> 8) & 255);
		ctx.ch.writeByte((n >> 16) & 255);
	}

	public static function write_string (ctx:Context, s:String) : Void {
		try {
			var x = ctx.scount - Hashtbl.find(ctx.strings, s);
			if (x > 0xFF) { throw ocaml.Not_found.instance; }
			b(ctx, x);
		}
		catch (_:ocaml.Not_found) {
			Hashtbl.replace(ctx.strings, s, ctx.scount);
			ctx.scount++;
			b(ctx, 0);
			ctx.ch.writeUInt16(s.length);
			ctx.ch.writeString(s);
		}
	}

	public static function write_constant (ctx:Context, c:Constant) : Void {
		switch (c) {
			case True: b(ctx, 0);
			case False: b(ctx, 1);
			case Null: b(ctx, 2);
			case This: b(ctx, 3);
			case Int(n):
				if (n >= 0 && n <= 0xFF) {
					b(ctx, 4);
					b(ctx, n);
				}
				else {
					b(ctx, 5);
					ctx.ch.writeInt32(n);
				}
			case Float(s):
				b(ctx, 6);
				write_string(ctx, s);
			case String(s):
				b(ctx, 7);
				write_string(ctx, s);
			case Builtin(s):
				b(ctx, 8);
				write_string(ctx, s);
			case Ident(s):
				b(ctx, 9);
				write_string(ctx, s);
			case Int32(n):
				b(ctx, 5); // same as Int
				ctx.ch.writeInt32(n);
		}
	}

	public static function write_op (ctx:Context, op:String) : Void {
		b(ctx, switch(op) {
			case "+": 0;
			case "-": 1;
			case "/": 2;
			case "*": 3;
			case "%": 4;
			case "<<": 5;
			case ">>": 6;
			case ">>>": 7;
			case "|": 8;
			case "&": 9;
			case "^": 10;
			case "==": 11;
			case "!=": 12;
			case ">": 13;
			case ">=": 14;
			case "<": 15;
			case "<=": 16;
			case "=": 17;
			case "&&": 18;
			case "||": 19;
			case "++=": 20;
			case "--=": 21;
			case "+=": 22;
			case "-=": 23;
			case "/=": 24;
			case "*=": 25;
			case "%=": 26;
			case "<<=": 27;
			case ">>=": 28;
			case ">>>=": 29;
			case "|=": 30;
			case "&=": 31;
			case "^=": 32;
			case op: throw "Invalid neko ast op " + op;
		});
	}

	public static function write_expr_opt(ctx:Context, opt:Option<Expr>) : Void {
		switch (opt) {
			case None: b(ctx, 0);
			case Some(e):
				b(ctx, 1);
				write_expr(ctx, e);
		}
	}

	public static function write_expr (ctx:Context, expr:Expr) : Void {
		var e = expr.decl; var p = expr.pos;
		if (p.psource != ctx.curfile) {
			b(ctx, 0);
			write_string(ctx, p.psource);
			write_ui24(ctx, p.pline);
			ctx.curfile = p.psource;
			ctx.curline = p.pline;
		}
		else if (p.pline != ctx.curline) {
			b(ctx, 1);
			write_ui24(ctx, p.pline);
			ctx.curline = p.pline;
		}
		switch (e) {
			case EConst(c):
				b(ctx, 2);
				write_constant(ctx, c);
			case EBlock(el):
				var n = List.length(el);
				if (n <= 0xFF) {
					b(ctx, 3);
					b(ctx, n);
				}
				else {
					b(ctx, 4);
					write_ui24(ctx, n);
				}
				List.iter(write_expr.bind(ctx), el);
			case EParenthesis(e):
				b(ctx, 5);
				write_expr(ctx, e);
			case EField(e, f):
				b(ctx, 6);
				write_expr(ctx, e);
				write_string(ctx, f);
			case ECall(e, el):
				var n = List.length(el);
				if (n <= 0xFF) {
					b(ctx, 7);
					write_expr(ctx, e);
					b(ctx, n);
				}
				else {
					b(ctx, 28);
					write_expr(ctx, e);
					write_ui24(ctx, n);
				}
				List.iter(write_expr.bind(ctx), el);
			case EArray(e1, e2):
				b(ctx, 8);
				write_expr(ctx, e1);
				write_expr(ctx, e2);
			case EVars(vl):
				b(ctx, 9);
				b(ctx, List.length(vl));
				List.iter(function (tmp) {
					var v = tmp.name; var e = tmp.def;
					write_string(ctx, v);
					write_expr_opt(ctx, e);
				}, vl);
			case EWhile(e1, e2, NormalWhile):
				b(ctx, 10);
				write_expr(ctx, e1);
				write_expr(ctx, e2);
			case EWhile(e1, e2, DoWhile):
				b(ctx, 11);
				write_expr(ctx, e1);
				write_expr(ctx, e2);
			case EIf(e1, e2, eo):
				b(ctx, 12);
				write_expr(ctx, e1);
				write_expr(ctx, e2);
				write_expr_opt(ctx, eo);
			case ETry(e1, v, e2):
				b(ctx, 13);
				write_expr(ctx, e1);
				write_string(ctx, v);
				write_expr(ctx, e2);
			case EFunction(pl, e):
				b(ctx, 14);
				b(ctx, List.length(pl));
				List.iter(write_string.bind(ctx), pl);
				write_expr(ctx, e);
			case EBinop(op, e1, e2):
				b(ctx, 15);
				write_op(ctx, op);
				write_expr(ctx, e1);
				write_expr(ctx, e2);
			case EReturn(None):
				b(ctx, 16);
			case EReturn(Some(e)):
				b(ctx, 17);
				write_expr(ctx, e);
			case EBreak(None):
				b(ctx, 18);
			case EBreak(Some(e)):
				b(ctx, 19);
				write_expr(ctx, e);
			case EContinue:
				b(ctx, 20);
			case ENext(e1, e2):
				b(ctx, 21);
				write_expr(ctx, e1);
				write_expr(ctx, e2);
			case EObject(fl):
				var n = List.length(fl);
				if (n <= 0xFF) {
					b(ctx, 22);
					b(ctx, n);
				}
				else {
					b(ctx, 23);
					write_ui24(ctx, n);
				}
				List.iter(function (tmp) {
					var f = tmp.s; var e = tmp.e;
					write_string(ctx, f);
					write_expr(ctx, e);
				}, fl);
			case ELabel(l):
				b(ctx, 24);
				write_string(ctx, l);
			case ESwitch(e, cases, eo):
				var n = List.length(cases);
				if (n <= 0xFF) {
					b(ctx, 25);
					b(ctx, n);
				}
				else {
					b(ctx, 26);
					write_ui24(ctx, n);
				}
				write_expr(ctx, e);
				List.iter(function (tmp) {
					var e1 = tmp.e1; var e2 = tmp.e2;
					write_expr(ctx, e1);
					write_expr(ctx, e2);
				}, cases);
				write_expr_opt(ctx, eo);
			case ENeko(s):
				b(ctx, 27);
				write_ui24(ctx, s.length);
				ctx.ch.writeString(s);

		}
	}

	public static function write (ch:FileOutput, e:Expr) : Void {
		var ctx:Context = {
			ch: ch,
			curfile: "",
			curline: -1,
			scount: 0,
			strings: Hashtbl.create(0)
		};
		ch.writeString("NBA\u0001"); // neko "NBA\001" ???
		write_expr(ctx, e);
	}
}