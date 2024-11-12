package ast.integer;

import ast.ASTNode;
import environment.Environment;
import exceptions.TypeErrorException;
import types.TInt;
import types.Type;
import types.VInt;
import types.Value;

public class ASTInt implements ASTNode {

	private final int val;

	public ASTInt(int val) {
		this.val = val;
	}

	@Override
	public Value eval(Environment<Value> e) {
		return new VInt(val);
	}

	@Override
	public Type typeCheck(Environment<Type> e) throws TypeErrorException {
		return new TInt();
	}

}

