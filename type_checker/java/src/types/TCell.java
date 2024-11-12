package types;

public class TCell implements Type {

	private final Type contentType;

	public TCell(Type contentType) {
		this.contentType = contentType;
	}

	public Type getContentType() {
		return contentType;
	}

	public String getClassName() {
		Type currType = contentType;
		StringBuilder className = new StringBuilder().append("ref_");

		while (currType instanceof TCell) {
			currType = ((TCell) currType).contentType;
			className.append("ref_");
		}

		return className.append("int").toString();
	}

	@Override
	public String toString() {
		return "cell";
	}

	@Override
	public String toCompilationString() {
		return "L" + getClassName() + ";";
	}

}
