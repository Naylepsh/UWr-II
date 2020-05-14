using System;
using System.Security.Cryptography;

namespace Ex_1
{
    public class Complex : IFormattable
    {
        private double _real;
        private double _imaginary;

        public Complex(double real = 0, double imaginary = 0)
        {
            _real = real;
            _imaginary = imaginary;
        }

        public string ToString(string format, IFormatProvider formatProvider)
        {
            switch (format)
            {
                case "w":
                    return string.Format("[{0},{1}]", _real, _imaginary);
                case "d":
                default:
                    return string.Format("{0}+{1}i", _real, _imaginary);
            }
        }

        public static Complex operator +(Complex c1, Complex c2)
        {
            double newReal = c1._real + c2._real;
            double newImaginary = c1._imaginary + c2._imaginary;
            return new Complex(newReal, newImaginary);
        }

        public static Complex operator -(Complex c1, Complex c2)
        {
            double newReal = c1._real - c2._real;
            double newImaginary = c1._imaginary - c2._imaginary;
            return new Complex(newReal, newImaginary);
        }

        public static Complex operator *(Complex c1, Complex c2)
        {
            double newReal = c1._real * c2._real - c1._imaginary * c2._imaginary;
            double newImaginary = c1._real * c2._imaginary + c1._imaginary * c2._real;
            return new Complex(newReal, newImaginary);
        }

        public static Complex operator /(Complex c1, Complex c2)
        {
            double divider = c2._real * c2._real + c2._imaginary * c2._imaginary;
            double newReal = (c1._real * c2._real + c1._imaginary * c2._imaginary) / divider ;
            double newImaginary = (c1._real * c2._imaginary + c1._imaginary * c2._real) / divider;
            return new Complex(newReal, newImaginary);
        }

        public static bool operator==(Complex c1, Complex c2)
        {
            return c1._real == c2._real && c1._imaginary == c2._imaginary;
        }

        public static bool operator!=(Complex c1, Complex c2)
        {
            return !(c1 == c2);
        }
    }
}