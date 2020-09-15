import java.util.HashMap;

public class Main {
    public static void main(String[] args) throws Exception{
        HashMap<String, Double> eval = new HashMap<>();
        eval.put("x", 7.0);
        eval.put("y", 2.0);
        eval.put("zet", 7.0);

        Expression expression = new Add(new Constant(5), new Constant(6));
        System.out.print(expression);
        System.out.println(" = " + expression.Eval(eval));

        Expression expression1 = new Multiply(new Constant(8), new Variable("x"));
        System.out.println(expression1);
        System.out.println("(when x = 7) = " + expression1.Eval(eval));

        HashMap<String, Double> eval2 = new HashMap<>();
        eval2.put("x", 10.0);
        System.out.println("(when x = 10) = " + expression1.Eval(eval2));

        Expression expression2 = new Subtract(
                new Add(new Variable("x"), new Variable("y")),
                new Add( new Variable("x"), new Variable("zet")));
        System.out.print(expression2);
        System.out.println(" = " + expression2.Eval(eval));

        Expression expression3 = new Add(new Constant(5.0), expression2);
        System.out.print(expression3);
        System.out.println(" = " + expression3.Eval(eval));

        Expression expression4 = new Multiply(expression1, expression2);
        System.out.print(expression4);
        System.out.println(" = " + expression4.Eval(eval));

        Expression expression5 = new Divide(new Constant(4), new Constant(2));
        System.out.print(expression5);
        System.out.println(" = " + expression5.Eval(eval));
    }
}
