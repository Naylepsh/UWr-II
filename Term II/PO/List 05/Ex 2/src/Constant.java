import java.util.HashMap;

public class Constant extends Expression{
    double value;

    public Constant(double value){
        this.value = value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    public double Eval(HashMap<String, Double> vals) {
        return value;
    }
}
