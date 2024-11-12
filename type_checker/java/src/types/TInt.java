package types;

public class TInt implements Type {

	@Override
	public String toString() {
		return "int";
	}

	@Override
	public String toCompilationString() {
		return "I";
	}

}
