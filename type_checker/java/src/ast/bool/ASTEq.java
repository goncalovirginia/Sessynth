package ast.bool;

import ast.ASTNode;
import ast.ASTPair;
import environment.Environment;
import exceptions.IDDeclaredTwiceException;
import exceptions.TypeErrorException;
import exceptions.UndeclaredIdentifierException;
import types.TBool;
import types.Type;
import types.VBool;
import types.Value;

public class ASTEq extends ASTPair {

	public ASTEq(ASTNode l, ASTNode r) {
		super(l, r);
	}

	@Override
	public Value eval(Environment<Value> e) throws UndeclaredIdentifierException, IDDeclaredTwiceException, TypeErrorException {
		Value lv = l.eval(e), rv = r.eval(e);

		if (!lv.getClass().equals(rv.getClass())) {
			throw new TypeErrorException("== requires both operands to be of same type.");
		}

		return new VBool(l.eval(e).equals(r.eval(e)));
	}

	@Override
	public Type typeCheck(Environment<Type> e) throws TypeErrorException, UndeclaredIdentifierException, IDDeclaredTwiceException {
		if (!(l.typeCheck(e).getClass().equals(r.typeCheck(e).getClass()))) {
			throw new TypeErrorException("Both operands in comparison must be of same type.");
		}
		return new TBool();
	}

}
