package ast;

import environment.Environment;
import exceptions.IDDeclaredTwiceException;
import exceptions.TypeErrorException;
import exceptions.UndeclaredIdentifierException;
import types.TCell;
import types.Type;
import types.VCell;
import types.Value;

public class ASTNew implements ASTNode {

	private final ASTNode exp;

	public ASTNew(ASTNode exp) {
		this.exp = exp;
	}

	@Override
	public Value eval(Environment<Value> e) throws UndeclaredIdentifierException, IDDeclaredTwiceException, TypeErrorException {
		return new VCell(exp.eval(e));
	}

	@Override
	public Type typeCheck(Environment<Type> e) throws TypeErrorException, UndeclaredIdentifierException, IDDeclaredTwiceException {
		return new TCell(exp.typeCheck(e));
	}


}
