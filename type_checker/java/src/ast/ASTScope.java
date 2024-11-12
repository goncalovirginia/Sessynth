package ast;

import environment.Environment;
import exceptions.IDDeclaredTwiceException;
import exceptions.TypeErrorException;
import exceptions.UndeclaredIdentifierException;
import types.Type;
import types.Value;

import java.util.Map;
import java.util.Map.Entry;

public class ASTScope implements ASTNode {

	private final Map<String, ASTNode> bindings;
	private final ASTNode body;

	public ASTScope(Map<String, ASTNode> bindings, ASTNode body) {
		this.bindings = bindings;
		this.body = body;
	}

	@Override
	public Value eval(Environment<Value> e) throws UndeclaredIdentifierException, IDDeclaredTwiceException, TypeErrorException {
		Environment<Value> eCurr = e.beginScope();

		for (Entry<String, ASTNode> binding : bindings.entrySet()) {
			eCurr.assoc(binding.getKey(), binding.getValue().eval(eCurr));
		}

		return body.eval(eCurr);
	}

	@Override
	public Type typeCheck(Environment<Type> e) throws TypeErrorException, UndeclaredIdentifierException, IDDeclaredTwiceException {
		Environment<Type> eTCurr = e.beginScope();

		for (Entry<String, ASTNode> binding : bindings.entrySet()) {
			eTCurr.assoc(binding.getKey(), binding.getValue().typeCheck(eTCurr));
		}

		return body.typeCheck(eTCurr);
	}

}
