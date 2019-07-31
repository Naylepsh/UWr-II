import java.util.HashMap;

public class Divide extends Operator {
    public Divide(Expression leftChild, Expression rightChild){
        this.setLeftChild(leftChild);
        this.setRightChild(rightChild);
    }

    public double Eval(HashMap<String, Double> vals){
        return getLeftChild().Eval(vals) / getRightChild().Eval(vals);
    }

    @Override
    public String toString() {
        return "(" + getLeftChild() + " / " + getRightChild() + ")";
    }
}
