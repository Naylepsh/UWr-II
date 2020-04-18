namespace Ex_2
{
    public abstract class UnaryExpression : AbstractExpression
    {
        protected AbstractExpression _expr;
    }

    public class Neg : UnaryExpression
    {
        public Neg(AbstractExpression expr)
        {
            _expr = expr;
        }

        public override bool Interpret(Context context)
        {
            return !_expr.Interpret(context);
        }
    }
}
