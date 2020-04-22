using Microsoft.VisualStudio.TestTools.UnitTesting;
using Ex_2;

namespace Ex_2_Tests
{
    [TestClass]
    public class UnaryExprTests
    {
        [TestMethod]
        public void InterpretNegation()
        {
            Context context = new Context();
            var expr = new ConstExpression(true);
            var negated = new Neg(expr);

            Assert.AreEqual(expr.Interpret(context), !negated.Interpret(context));
        }

        [TestMethod]
        public void InterpretDoubleNegation()
        {
            Context context = new Context();
            var expr = new ConstExpression(true);
            var doubleNegated = new Neg(new Neg(expr));

            Assert.AreEqual(expr.Interpret(context), doubleNegated.Interpret(context));
        }
    }
}
