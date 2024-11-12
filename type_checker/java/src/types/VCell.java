package types;

public class VCell implements Value {

	private Value v;

	public VCell(Value v) {
		this.v = v;
	}

	public Value getValue() {
		return v;
	}

	public Value set(Value v) {
		return this.v = v;
	}

	@Override
	public String toString() {
		return v.toString();
	}

}
