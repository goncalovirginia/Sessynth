package types;

public class VInt implements Value {

	private final int v;

	public VInt(int v) {
		this.v = v;
	}

	public int getValue() {
		return v;
	}

	@Override
	public String toString() {
		return Integer.toString(v);
	}

	@Override
	public boolean equals(Object obj) {
		return v == ((VInt) obj).v;
	}

}
