using System;
using Ex_2;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace ShapeFactoryTests
{
    [TestClass]
    public class UnitTest1
    {
        public class Rectangle : IShape
        {
            private int _width;
            private int _height;

            public Rectangle(int width, int height)
            {
                _width = width;
                _height = height;
            }

            public int GetArea()
            {
                return _width * _height;
            }
        }

        public class RectangleWorker : IFactoryWorker
        {
            public bool AcceptsParameters(string name, params object[] parameters)
            {
                bool properName = name == "Rectangle";
                bool properArgLength = parameters.Length == 2;
                bool properTypes = parameters[0] is int && parameters[1] is int;

                return properName && properArgLength && properTypes;
            }

            public IShape Create(params object[] parameters)
            {
                int width = (int)parameters[0];
                int height = (int)parameters[1];
                return new Rectangle(width, height);
            }
        }

        [TestMethod]
        public void TestSquare()
        {
            ShapeFactory factory = new ShapeFactory();
            int size = 4;
            IShape square = factory.Create("Square", size);
            int properArea = size * size;
            int area = square.GetArea();

            Assert.AreEqual(area, properArea);
        }

        [TestMethod]
        public void TestRectangle()
        {
            ShapeFactory factory = new ShapeFactory();
            factory.RegisterWorker(new RectangleWorker());
            int width = 4;
            int height = 5;
            IShape rectangle = factory.Create("Rectangle", width, height);
            int properArea = width * height;
            int area = rectangle.GetArea();
            
            Assert.AreEqual(area, properArea);
        }

        [TestMethod]
        public void TestCompareDifferentShapes()
        {
            ShapeFactory factory = new ShapeFactory();
            factory.RegisterWorker(new RectangleWorker());
            int width = 4;
            int height = 5;
            IShape rectangle = factory.Create("Rectangle", width, height);
            IShape square = factory.Create("Square", width);

            Assert.AreNotEqual(rectangle, square);
        }

        [TestMethod]
        [ExpectedException(typeof(ArgumentException))]
        public void TestNotRegisteredShape()
        {
            ShapeFactory factory = new ShapeFactory();
            int width = 4;
            int height = 5;
            IShape rectangle = factory.Create("Rectangle", width, height);
        }

        [TestMethod]
        [ExpectedException(typeof(ArgumentException))]
        public void TestWrongNumberOfParameters()
        {
            ShapeFactory factory = new ShapeFactory();
            int width = 4;
            int height = 5;
            IShape rectangle = factory.Create("Rectangle", width, height, width, height);
        }
    }
}
