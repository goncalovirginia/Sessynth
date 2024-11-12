package ast;

import environment.Environment;
import exceptions.IDDeclaredTwiceException;
import exceptions.TypeErrorException;
import exceptions.UndeclaredIdentifierException;
import types.Type;
import types.Value;

import java.util.List;

public class ASTFunctionCall implements ASTNode {

	private final String name;
	private final List<ASTNode> params;

	public ASTFunctionCall(String name, List<ASTNode> params) {
		this.name = name;
		this.params = params;
	}

	@Override
	public Value eval(Environment<Value> e) throws UndeclaredIdentifierException, IDDeclaredTwiceException, TypeErrorException {
		return null;
	}

	@Override
	public Type typeCheck(Environment<Type> e) throws TypeErrorException, UndeclaredIdentifierException, IDDeclaredTwiceException {
		for (int i = 0; i < params.size(); i++) {
			if (!params.get(i).typeCheck(e).toString().equals(e.find(name + "." + i).toString()))
				throw new TypeErrorException("Invalid parameter type on call to function " + name + " (parameter " + i + ")");
		}

		return e.find(name + ".");
	}

}
