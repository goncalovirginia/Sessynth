package ast.bool;

import ast.ASTNode;
import ast.integer.ASTIntPair;
import environment.Environment;
import exceptions.IDDeclaredTwiceException;
import exceptions.TypeErrorException;
import exceptions.UndeclaredIdentifierException;
import types.VBool;
import types.VInt;
import types.Value;

public class ASTGr extends ASTIntPair {

	public ASTGr(ASTNode l, ASTNode r) {
		super(l, r);
	}

	@Override
	public Value eval(Environment<Value> e) throws UndeclaredIdentifierException, IDDeclaredTwiceException, TypeErrorException {
		Value lv = l.eval(e), rv = r.eval(e);

		if (!(lv instanceof VInt && rv instanceof VInt)) {
			throw new TypeErrorException("> requires both operands to be of type int.");
		}

		return new VBool(((VInt) l.eval(e)).getValue() > ((VInt) r.eval(e)).getValue());
	}

}
