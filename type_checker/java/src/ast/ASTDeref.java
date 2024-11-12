package ast;

import environment.Environment;
import exceptions.IDDeclaredTwiceException;
import exceptions.TypeErrorException;
import exceptions.UndeclaredIdentifierException;
import types.TCell;
import types.Type;
import types.VCell;
import types.Value;

public class ASTDeref implements ASTNode {

	private final ASTNode n;

	public ASTDeref(ASTNode n) {
		this.n = n;
	}

	@Override
	public Value eval(Environment<Value> e) throws UndeclaredIdentifierException, IDDeclaredTwiceException, TypeErrorException {
		Value v = n.eval(e);

		if (!(v instanceof VCell)) {
			throw new TypeErrorException("~ operation requires type mutable reference.");
		}

		return ((VCell) v).getValue();
	}

	@Override
	public Type typeCheck(Environment<Type> e) throws TypeErrorException, UndeclaredIdentifierException, IDDeclaredTwiceException {
		Type t = n.typeCheck(e);

		if (!(t instanceof TCell)) {
			throw new TypeErrorException("~ operation requires type mutable reference.");
		}

		return ((TCell) t).getContentType();
	}

}
