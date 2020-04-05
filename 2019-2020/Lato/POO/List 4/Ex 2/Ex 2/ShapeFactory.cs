using System;
using System.Collections.Generic;

namespace Ex_2
{
    public interface IShape {
        int GetArea();
    }

    public class Square : IShape
    {
        private int _size;

        public Square(int size)
        {
            _size = size;
        }

        public int GetArea()
        {
            return _size * _size;
        }
    }

    public interface IFactoryWorker
    {
        bool AcceptsParameters(string name, params object[] parameters);
        IShape Create(params object[] parameters);
    }

    public class SquareWorker : IFactoryWorker
    {
        public IShape Create(params object[] parameters)
        {
            int size = (int)parameters[0];
            return new Square(size);
        }

        public bool AcceptsParameters(string name, params object[] parameters)
        {
            bool properName = name == "Square";
            bool properArgLength = parameters.Length == 1;
            bool properType = parameters[0] is int;

            return properName && properArgLength && properType;
        }
    }

    public class ShapeFactory
    {
        private List<IFactoryWorker> _workers = new List<IFactoryWorker>();

        public ShapeFactory()
        {
            _workers.Add(new SquareWorker());
        }

        public void RegisterWorker(IFactoryWorker worker)
        {
            _workers.Add(worker);
        }

        public IShape Create(string name, params object[] parameters)
        {
            foreach (var worker in _workers)
            {
                if (worker.AcceptsParameters(name, parameters))
                {
                    return worker.Create(parameters);
                }
            }

            throw new ArgumentException(string.Format("Couldn't find {0} factory worker", name));
        }
    }

}
