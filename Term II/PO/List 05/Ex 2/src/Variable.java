import java.util.HashMap;

public class Variable extends Expression {
    private String variable;

    public Variable(String variable){
        this.variable = variable;
    }

    @Override
    public String toString() {
        return variable;
    }

    public double Eval(HashMap<String, Double> vals) {
        return vals.get(variable);
    }
}
