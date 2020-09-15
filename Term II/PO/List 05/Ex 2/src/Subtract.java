import java.util.HashMap;

public class Subtract extends Operator {
    public Subtract(Expression leftChild, Expression rightChild){
        this.setLeftChild(leftChild);
        this.setRightChild(rightChild);
    }

    public double Eval(HashMap<String, Double> vals){
        return getLeftChild().Eval(vals) - getRightChild().Eval(vals);
    }

    @Override
    public String toString() {
        return "(" + getLeftChild() + " - " + getRightChild() + ")";
    }
}
