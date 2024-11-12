package ast;

import environment.Environment;
import exceptions.IDDeclaredTwiceException;
import exceptions.TypeErrorException;
import exceptions.UndeclaredIdentifierException;
import types.TBool;
import types.TInt;
import types.Type;
import types.Value;

import java.util.List;

public class ASTFunction implements ASTNode {

	private final String name;
	private final List<String> params, types;
	private final ASTNode body;

	public ASTFunction(String name, List<String> params, List<String> types, ASTNode body) {
		this.name = name;
		this.params = params;
		this.types = types;
		this.body = body;
	}

	@Override
	public Value eval(Environment<Value> e) throws UndeclaredIdentifierException, IDDeclaredTwiceException, TypeErrorException {
		return null;
	}

	@Override
	public Type typeCheck(Environment<Type> e) throws TypeErrorException, UndeclaredIdentifierException, IDDeclaredTwiceException {
		for (int i = 0; i < params.size(); i++) {
			Type currParamType = switch (types.get(i)) {
				case "bool" -> new TBool();
				case "int" -> new TInt();
				default -> throw new TypeErrorException("Invalid parameter type");
			};
			e.assoc(name + "." + i, currParamType);
		}

		Type bodyType = body.typeCheck(e);
		e.assoc(name + ".", bodyType);
		return bodyType;
	}

}
