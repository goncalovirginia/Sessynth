package ast;

public abstract class ASTPair implements ASTNode {

	protected final ASTNode l, r;

	public ASTPair(ASTNode l, ASTNode r) {
		this.l = l;
		this.r = r;
	}

}
