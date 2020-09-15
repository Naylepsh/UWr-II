import com.sun.org.apache.xpath.internal.operations.Mult;

import java.util.HashMap;

public class Multiply extends Operator{
    public Multiply(Expression leftChild, Expression rightChild){
        this.setLeftChild(leftChild);
        this.setRightChild(rightChild);
    }

    public double Eval(HashMap<String, Double> vals){
        return getLeftChild().Eval(vals) * getRightChild().Eval(vals);
    }

    @Override
    public String toString() {
        return "(" + getLeftChild() + " * " + getRightChild() + ")";
    }
}
