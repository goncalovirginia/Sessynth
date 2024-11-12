package ast.bool;

import ast.ASTNode;
import environment.Environment;
import exceptions.IDDeclaredTwiceException;
import exceptions.TypeErrorException;
import exceptions.UndeclaredIdentifierException;
import types.TBool;
import types.Type;
import types.VBool;
import types.Value;

public class ASTBool implements ASTNode {

	private final boolean v;

	public ASTBool(boolean v) {
		this.v = v;
	}

	@Override
	public Value eval(Environment<Value> e) throws UndeclaredIdentifierException, IDDeclaredTwiceException {
		return new VBool(v);
	}

	@Override
	public Type typeCheck(Environment<Type> e) throws TypeErrorException {
		return new TBool();
	}

}
