package types;

public class VBool implements Value {

	private final boolean v;

	public VBool(boolean v) {
		this.v = v;
	}

	public boolean getValue() {
		return v;
	}

	@Override
	public String toString() {
		return Boolean.toString(v);
	}

	@Override
	public boolean equals(Object obj) {
		return v == ((VBool) obj).v;
	}

}
