package ast;

import environment.Environment;
import exceptions.UndeclaredIdentifierException;
import types.Type;
import types.Value;

public class ASTId implements ASTNode {

	private final String id;

	public ASTId(String id) {
		this.id = id;
	}

	@Override
	public Value eval(Environment<Value> e) throws UndeclaredIdentifierException {
		return e.find(id);
	}

	@Override
	public Type typeCheck(Environment<Type> e) throws UndeclaredIdentifierException {
		return e.find(id);
	}

}
