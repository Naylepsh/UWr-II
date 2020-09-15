using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Ex_2;

namespace Ex_2_Tests
{
    [TestClass]
    public class TokensTest
    {
        [TestMethod]
        public void InterpretTrueToken()
        {
            Context context = new Context();
            var expr = new ConstExpression(true);

            Assert.AreEqual(expr.Interpret(context), true);
        }

        [TestMethod]
        public void InterpretFalseToken()
        {
            Context context = new Context();
            var expr = new ConstExpression(false);

            Assert.AreEqual(expr.Interpret(context), false);
        }

        [TestMethod]
        public void InterpretValidVarToken()
        {
            Context context = new Context();
            bool value = true;
            string name = "x";
            context.SetValue(name, value);
            var expr = new VarExpression(name);

            Assert.AreEqual(expr.Interpret(context), value);
        }

        [TestMethod]
        public void InterpretInvalidVarToken()
        {
            Context context = new Context();
            bool value = true;
            string name = "x";
            context.SetValue(name, value);
            var expr = new VarExpression("y");

            Assert.ThrowsException<ArgumentException>(() =>
            {
               expr.Interpret(context);
            });
        }
    }
}
