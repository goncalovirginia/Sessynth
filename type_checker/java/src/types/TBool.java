package types;

public class TBool implements Type {

	@Override
	public String toString() {
		return "bool";
	}

	@Override
	public String toCompilationString() {
		return "I";
	}

}
