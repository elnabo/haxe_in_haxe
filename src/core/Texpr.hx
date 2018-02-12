package core;

using equals.Equal;

class Texpr {
	public static function equal_fa (fa1:core.Type.TFieldAccess, fa2:core.Type.TFieldAccess) : Bool {
		return switch {f:fa1,s:fa2} {
			// case {f:FStatic(c1,cf1), s:FStatic(c2,cf2)}: c1 == c2 && cf1 == cf2;
			case {f:FStatic(c1,cf1), s:FStatic(c2,cf2)}: c1.equals(c2) && cf1.equals(cf2);
			// case {f:FInstance(c1,tl1,cf1), s:FInstance(c2,tl2,cf2)}: c1 == c2 && safe_for_all2 type_iseq tl1 tl2 && cf1 == cf2;
			case {f:FInstance(c1,tl1,cf1), s:FInstance(c2,tl2,cf2)}: c1.equals(c2) && core.Ast.safe_for_all2(core.Type.type_iseq, tl1, tl2) && cf1.equals(cf2);
			// TODO: This is technically not correct but unfortunately the compiler makes a distinct tclass_field for each anon field access.
			case {f:FAnon(cf1), s:FAnon(cf2)}: cf1.cf_name == cf2.cf_name;
			case {f:FDynamic(s1), s:FDynamic(s2)}: s1.equals(s2);
			// case {f:FClosure(None,cf1),s:FClosure(None,cf2)}: cf1 == cf2;
			case {f:FClosure(None,cf1),s:FClosure(None,cf2)}: cf1.equals(cf2);
			// case {f:FClosure(Some(c1,tl1),cf1),s:FClosure(Some(c2,tl2),cf2)}: c1 == c2 && safe_for_all2 type_iseq tl1 tl2 && cf1 == cf2;
			case {f:FClosure(Some({c:c1,params:tl1}),cf1),s:FClosure(Some({c:c2,params:tl2}),cf2)}: c1.equals(c2) && core.Ast.safe_for_all2(core.Type.type_iseq, tl1, tl2) && cf1.equals(cf2);
			// case {f:FEnum(en1,ef1), s:FEnum(en2,ef2)}: en1 == en2 && ef1 == ef2;
			case {f:FEnum(en1,ef1), s:FEnum(en2,ef2)}: en1.equals(en2) && ef1.equals(ef2);
			case _: false;
		}
	}

	public static function equal (e1:core.Type.TExpr, e2:core.Type.TExpr) : Bool {
		return switch ({f:e1.eexpr, s:e2.eexpr}) {
			case {f:TConst(ct1), s:TConst(ct2)}: ct1.equals(ct2);
			// case {f:TLocal(v1) , s:TLocal(v2)}: v1 == v2;
			case {f:TLocal(v1) , s:TLocal(v2)}: v1.equals(v2);
			case {f:TArray(eb1,ei1), s:TArray(eb2,ei2)}: equal(eb1, eb2) && equal(ei1, ei2);
			case {f:TBinop(op1,lhs1,rhs1), s:TBinop(op2,lhs2,rhs2)}: op1.equals(op2) && equal(lhs1, lhs2) && equal(rhs1, rhs2);
			case {f:TField(e1,fa1), s:TField(e2,fa2)}: equal(e1, e2) && equal_fa(fa1, fa2);
			// case {f:TTypeExpr(mt1), s:TTypeExpr(mt2)}: mt1 == mt2;
			case {f:TTypeExpr(mt1), s:TTypeExpr(mt2)}: mt1.equals(mt2);
			case {f:TParenthesis(e1), s:TParenthesis(e2)}: equal(e1, e2);
			// case {f:TObjectDecl(fl1), s:TObjectDecl(fl2)}: safe_for_all2(function (s1,e1) (s2,e2)  s1 = s2 && equal e1 e2) fl1 fl2
			case {f:TObjectDecl(fl1), s:TObjectDecl(fl2)}: core.Ast.safe_for_all2(function (o1:core.Type.TObjectField, o2:core.Type.TObjectField) { return o1.a.equals(o2.a) && equal(o1.expr, o2.expr);}, fl1, fl2);
			case {f:TArrayDecl(el1), s:TArrayDecl(el2)}, {f:TBlock(el1), s:TBlock(el2)}: core.Ast.safe_for_all2(equal, el1, el2);
			case {f:TCall(e1,el1), s:TCall(e2,el2)}: equal(e1, e2) && core.Ast.safe_for_all2(equal, el1, el2);
			// case {f:TNew(c1,tl1,el1), s:TNew(c2,tl2,el2)}: c1 == c2 && safe_for_all2 type_iseq tl1 tl2 && safe_for_all2 equal el1 el2
			case {f:TNew(c1,tl1,el1), s:TNew(c2,tl2,el2)}: c1.equals(c2) && core.Ast.safe_for_all2(core.Type.type_iseq, tl1, tl2) && core.Ast.safe_for_all2(equal, el1, el2);
			case {f:TUnop(op1,flag1,e1), s:TUnop(op2,flag2,e2)}: op1.equals(op2) && flag1.equals(flag2) && equal(e1, e2);
			// case {f:TFunction(tf1), s:TFunction(tf2)}: tf1 == tf2;
			case {f:TFunction(tf1), s:TFunction(tf2)}: tf1.equals(tf2);
			// case {f:TVar(v1,None), s:TVar(v2,None)}: v1 == v2;
			case {f:TVar(v1,None), s:TVar(v2,None)}: v1.equals(v2);
			// case {f:TVar(v1,Some e1), s:TVar(v2,Some e2)}: v1 == v2 && equal(e1, e2);
			case {f:TVar(v1,Some(e1)), s:TVar(v2,Some(e2))}: v1.equals(v2) && equal(e1, e2);
			// case {f:TFor(v1,ec1,eb1), s:TFor(v2,ec2,eb2)}: v1 == v2 && equal(ec1, ec2) && equal(eb1, eb2);
			case {f:TFor(v1,ec1,eb1), s:TFor(v2,ec2,eb2)}: v1.equals(v2) && equal(ec1, ec2) && equal(eb1, eb2);
			case {f:TIf(e1,ethen1,None), s:TIf(e2,ethen2,None)}: equal(e1, e2) && equal(ethen1, ethen2);
			case {f:TIf(e1,ethen1,Some(eelse1)), s:TIf(e2,ethen2,Some(eelse2))}: equal(e1, e2) && equal(ethen1, ethen2) && equal(eelse1, eelse2);
			case {f:TWhile(e1,eb1,flag1), s:TWhile(e2,eb2,flag2)}: equal(e1, e2) && equal(eb2, eb2) && flag1.equals(flag2);
			case {f:TSwitch(e1,cases1,eo1), s:TSwitch(e2,cases2,eo2)}:
				equal(e1, e2) &&
				core.Ast.safe_for_all2(function (c1, c2) { 
					var el1 = c1.values; var e1 = c1.e;
					var el2 = c2.values; var e2 = c2.e;
					return core.Ast.safe_for_all2(equal, el1, el2) && equal(e1, e2);}, cases1, cases2) &&
				switch {f:eo1,s:eo2} {
					case {f:None, s:None}: true;
					case {f:Some(e1), s:Some(e2)}: equal(e1, e2);
					case _: false;
				};
			// case {f:TTry(e1,catches1),TTry(e2,catches2) -> equal e1 e2 && safe_for_all2 (fun (v1,e1) (v2,e2) -> v1 == v2 && equal e1 e2) catches1 catches2
			case {f:TTry(e1,catches1),s:TTry(e2,catches2)}: equal(e1, e2) && core.Ast.safe_for_all2 (function (c1:{v:core.Type.TVar, e:core.Type.TExpr}, c2:{v:core.Type.TVar, e:core.Type.TExpr}) {
				var v1 = c1.v; var e1 = c1.e;
				var v2 = c2.v; var e2 = c2.e;
				return v1.equals(v2) && equal(e1, e2); }, catches1, catches2);
			case {f:TReturn(None),s:TReturn(None)}: true;
			case {f:TReturn(Some(e1)),s:TReturn(Some(e2))}: equal(e1, e2);
			case {f:TThrow(e1),s:TThrow(e2)}: equal(e1, e2);
			case {f:TCast(e1,None),s:TCast(e2,None)}: equal(e1, e2);
			// case {f:TCast(e1,Some mt1),TCast(e2,Some mt2)}: equal(e1, e2) && mt1 == mt2;
			case {f:TCast(e1,Some(mt1)),s:TCast(e2,Some(mt2))}: equal(e1, e2) && mt1.equals(mt2);
			// case {f:TMeta((m1,el1,_),e1),TMeta((m2,el2,_),e2)}: m1 = m2 && safe_for_all2 (fun e1 e2 -> (* TODO: cheating? *) (Ast.s_expr e1) = (Ast.s_expr e2)) el1 el2 && equal e1 e2
			case {f:TBreak,s:TBreak}, {f:TContinue,s:TContinue}: true;
			// case {f:TEnumParameter(e1,ef1,i1),TEnumParameter(e2,ef2,i2)}: equal(e1, e2) && ef1 == ef2 && i1.equals(i2);
			case {f:TEnumParameter(e1,ef1,i1),s: TEnumParameter(e2,ef2,i2)}: equal(e1, e2) && ef1.equals(ef2) && i1.equals(i2);
			case _: false;
		}
	}

	public static function skip (e:core.Type.TExpr) : core.Type.TExpr {
		return switch (e.eexpr) {
			case TParenthesis(e1), TMeta(_, e1), TBlock([e1]), TCast(e1, None): skip(e1);
			case _: e;
		}
	}
}