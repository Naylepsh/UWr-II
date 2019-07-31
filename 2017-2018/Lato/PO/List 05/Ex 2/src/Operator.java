import java.util.HashMap;

abstract class Operator extends Expression {
    private Expression leftChild;
    private Expression rightChild;

    public abstract double Eval(HashMap<String, Double> eval);

    public Expression getLeftChild() {
        return leftChild;
    }

    public Expression getRightChild() {
        return rightChild;
    }

    public void setRightChild(Expression rightChild) {
        this.rightChild = rightChild;
    }

    public void setLeftChild(Expression leftChild) {
        this.leftChild = leftChild;
    }
}
