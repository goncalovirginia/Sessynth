package ast.integer;

import ast.ASTNode;
import ast.ASTPair;
import environment.Environment;
import exceptions.IDDeclaredTwiceException;
import exceptions.TypeErrorException;
import exceptions.UndeclaredIdentifierException;
import types.TInt;
import types.Type;

public abstract class ASTIntPair extends ASTPair {

	public ASTIntPair(ASTNode l, ASTNode r) {
		super(l, r);
	}

	@Override
	public Type typeCheck(Environment<Type> e) throws TypeErrorException, UndeclaredIdentifierException, IDDeclaredTwiceException {
		if (!(l.typeCheck(e) instanceof TInt && r.typeCheck(e) instanceof TInt)) {
			throw new TypeErrorException("Both operands must be of type int.");
		}
		return new TInt();
	}

}
