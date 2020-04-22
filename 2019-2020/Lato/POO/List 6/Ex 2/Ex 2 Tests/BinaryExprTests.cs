using Microsoft.VisualStudio.TestTools.UnitTesting;
using Ex_2;

namespace Ex_2_Tests
{
    [TestClass]
    public class BinaryExprTests
    {
        [TestMethod]
        public void InterpretAnd()
        {
            Context context = new Context();
            context.SetValue("x", true);
            context.SetValue("y", false);
            var var1 = new VarExpression("x");
            var var2 = new VarExpression("y");
            var expr = new And(var1, var2);

            Assert.IsFalse(expr.Interpret(context));
        }

        [TestMethod]
        public void InterpretOr()
        {
            Context context = new Context();
            context.SetValue("x", true);
            context.SetValue("y", false);
            var var1 = new VarExpression("x");
            var var2 = new VarExpression("y");
            var expr = new Or(var1, var2);

            Assert.IsTrue(expr.Interpret(context));
        }

        [TestMethod]
        public void InterpretComplex()
        {
            Context context = new Context();
            context.SetValue("x", true);
            context.SetValue("y", false);
            var var1 = new VarExpression("x");
            var var2 = new VarExpression("y");
            var left = new Or(var1, var2);
            var right = new Neg(new And(var1, var2));
            var expr = new And(left, right);

            Assert.IsTrue(expr.Interpret(context));
        }
    }
}
