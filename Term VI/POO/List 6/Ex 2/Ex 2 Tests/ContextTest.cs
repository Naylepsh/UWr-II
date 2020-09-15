using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Ex_2;

namespace Ex_2_Tests
{
    [TestClass]
    public class ContextTest
    {
        [TestMethod]
        public void AddSingleVar()
        {
            Context context = new Context();
            bool value = true;
            string name = "x";
            context.SetValue(name, value);

            Assert.AreEqual(context.GetValue(name), value);
        }

        [TestMethod]
        public void AddTheSameVar()
        {
            Context context = new Context();
            bool value = true;
            string name = "x";
            context.SetValue(name, value);
            context.SetValue(name, !value);

            Assert.AreEqual(context.GetValue(name), !value);
        }

        [TestMethod]
        public void GetValueOfInvalidVariable()
        {
            Context context = new Context();
            bool value = true;
            string name = "x";
            context.SetValue(name, value);

            Assert.ThrowsException<ArgumentException>(() =>
            {
                context.GetValue("y");
            });
        }
    }
}
