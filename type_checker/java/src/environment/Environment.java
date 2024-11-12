package environment;

import exceptions.IDDeclaredTwiceException;
import exceptions.UndeclaredIdentifierException;

import java.util.HashMap;
import java.util.Map;

public class Environment<V> {

	private final Map<String, V> assocs;
	private final Environment<V> parent;
	private final int depth;

	public Environment() {
		this.assocs = new HashMap<>();
		parent = null;
		depth = -1;
	}

	public Environment(Environment<V> parent) {
		this.assocs = new HashMap<>();
		this.parent = parent;
		depth = parent.depth + 1;
	}

	public Environment<V> beginScope() {
		return new Environment<>(this);
	}

	public Environment<V> endScope() {
		return parent;
	}

	public void assoc(String id, V val) throws IDDeclaredTwiceException {
		if (assocs.put(id, val) != null) {
			throw new IDDeclaredTwiceException(id);
		}
	}

	public V find(String id) throws UndeclaredIdentifierException {
		try {
			V value = assocs.get(id);
			return value != null ? value : parent.find(id);
		} catch (NullPointerException e) {
			throw new UndeclaredIdentifierException(id);
		}
	}

	public int depth() {
		return depth;
	}

}
