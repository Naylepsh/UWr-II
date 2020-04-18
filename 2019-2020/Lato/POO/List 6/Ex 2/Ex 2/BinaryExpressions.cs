namespace Ex_2
{
    public abstract class BinaryExpressions : AbstractExpression
    {
        protected AbstractExpression _left;
        protected AbstractExpression _right;

        public BinaryExpressions(AbstractExpression left, AbstractExpression right)
        {
            _left = left;
            _right = right;
        }
    }

    public class And : BinaryExpressions
    {
        public And(AbstractExpression left, AbstractExpression right) : base(left, right)
        {
        }

        public override bool Interpret(Context context)
        {
            return _left.Interpret(context) && _right.Interpret(context);
        }
    }

    public class Or : BinaryExpressions
    {
        public Or(AbstractExpression left, AbstractExpression right) : base(left, right)
        {
        }

        public override bool Interpret(Context context)
        {
            return _left.Interpret(context) || _right.Interpret(context);
        }
    }
}
