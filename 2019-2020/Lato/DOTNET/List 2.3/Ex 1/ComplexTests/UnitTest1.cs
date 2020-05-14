using Ex_1;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace ComplexTests
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void Should_BeEqual_WhenComparingToIdenticalValue()
        {
            double real = 1;
            double imaginary = 1;
            Complex c1 = new Complex(real, imaginary);
            Complex c2 = new Complex(real, imaginary);

            Assert.IsTrue(c1 == c2);
        }

        [TestMethod]
        public void Should_NotBeEqual_WhenComparingToDifferentValue()
        {
            double real = 1;
            double imaginary = 1;
            Complex c1 = new Complex(real, imaginary);
            Complex c2 = new Complex(real + 1, imaginary);
            Complex c3 = new Complex(real, imaginary + 1);
            Complex c4 = new Complex(real + 1, imaginary + 1);

            Assert.IsTrue(c1 != c2);
            Assert.IsTrue(c1 != c3);
            Assert.IsTrue(c1 != c4);
            Assert.IsTrue(c2 != c3);
            Assert.IsTrue(c2 != c4);
            Assert.IsTrue(c3 != c4);
        }

        [TestMethod]
        public void Should_AddProperly()
        {
            double real = 1;
            double imaginary = 1;
            Complex c1 = new Complex(real, imaginary);
            Complex c2 = new Complex(real + 1, imaginary + 1);
            Complex expectedResult = new Complex(real + real + 1, imaginary + imaginary + 1);

            Complex result = c1 + c2;

            Assert.IsTrue(result == expectedResult);
        }

        [TestMethod]
        public void Should_SubtractProperly()
        {
            double real = 1;
            double imaginary = 1;
            Complex c1 = new Complex(real, imaginary);
            Complex c2 = new Complex(real + 1, imaginary + 1);
            Complex expectedResult = new Complex(real - real - 1, imaginary - imaginary - 1);

            Complex result = c1 - c2;

            Assert.IsTrue(result == expectedResult);
        }

        [TestMethod]
        public void Should_MultiplyProperly()
        {
            double real = 1;
            double imaginary = 1;
            Complex c1 = new Complex(real, imaginary);
            Complex c2 = new Complex(real, imaginary);
            Complex expectedResult = new Complex(0, 2);

            Complex result = c1 * c2;

            Assert.IsTrue(result == expectedResult);
        }

        [TestMethod]
        public void Should_DivideProperly()
        {
            double real = 1;
            double imaginary = 1;
            Complex c1 = new Complex(real, imaginary);
            Complex c2 = new Complex(real, imaginary);
            Complex expectedResult = new Complex(1, 1);

            Complex result = c1 / c2;

            Assert.IsTrue(result == expectedResult);
        }

        [TestMethod]
        public void Should_FormatProperly_WhenUsingDFormat()
        {
            double real = 1;
            double imaginary = 2;
            Complex c1 = new Complex(real, imaginary);
            string expectedResult = string.Format("{0}+{1}i", real, imaginary);

            string formatted = string.Format("{0:d}", c1);

            Assert.IsTrue(formatted == expectedResult);
        }

        [TestMethod]
        public void Should_HaveTheSameFormat_WhenFormattingWithDefaultAndD()
        {
            double real = 1;
            double imaginary = 2;
            Complex c1 = new Complex(real, imaginary);

            string formattedWithD = string.Format("{0:d}", c1);
            string defaultFormatting = string.Format("{0}", c1);

            Assert.IsTrue(formattedWithD == defaultFormatting);
        }



        [TestMethod]
        public void Should_FormatProperly_WhenUsingWFormat()
        {
            double real = 1;
            double imaginary = 2;
            Complex c1 = new Complex(real, imaginary);
            string expectedResult = string.Format("[{0},{1}]", real, imaginary);

            string formatted = string.Format("{0:w}", c1);

            Assert.IsTrue(formatted == expectedResult);
        }
    }
}