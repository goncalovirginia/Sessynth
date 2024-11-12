import ast.*;
import ast.bool.ASTBool;
import ast.integer.ASTMult;
import ast.integer.ASTInt;
import environment.Environment;

import java.util.ArrayList;
import java.util.List;

public class Main {

	public static void main(String[] args) {
		try {
			ASTNode ast = buildAST();
			ast.typeCheck(new Environment<>());
			System.out.println("Type Checking Complete.");
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private static ASTNode buildAST() {
		//return new ASTAdd(new ASTNum(2), new ASTNum(2));

		List<String> fDefParamsNames = new ArrayList<>();
		fDefParamsNames.add("x");
		List<String> fDefParamsTypes = new ArrayList<>();
		fDefParamsTypes.add("int");
		List<ASTNode> fCallParams = new ArrayList<>();
		fCallParams.add(new ASTMult(new ASTInt(2), new ASTInt(2)));

		return new ASTExpSeq(
				new ASTFunction("f", fDefParamsNames, fDefParamsTypes, new ASTInt(1)),
				new ASTFunctionCall("f", fCallParams)
		);
	}

}
