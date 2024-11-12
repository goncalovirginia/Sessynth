package ast.bool;

import ast.ASTNode;
import ast.ASTPair;
import environment.Environment;
import exceptions.IDDeclaredTwiceException;
import exceptions.TypeErrorException;
import exceptions.UndeclaredIdentifierException;
import types.TBool;
import types.Type;

public abstract class ASTBoolPair extends ASTPair {

	public ASTBoolPair(ASTNode l, ASTNode r) {
		super(l, r);
	}

	@Override
	public Type typeCheck(Environment<Type> e) throws TypeErrorException, UndeclaredIdentifierException, IDDeclaredTwiceException {
		if (!(l.typeCheck(e) instanceof TBool && r.typeCheck(e) instanceof TBool)) {
			throw new TypeErrorException("Both operands must be of type boolean.");
		}
		return new TBool();
	}

}
