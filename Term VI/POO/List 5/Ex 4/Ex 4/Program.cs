using System;
using System.Collections;

namespace Ex_4
{
    class ComparisonToComparerAdapter<T> : IComparer
    {
        private readonly Comparison<T> _comparison;

        public ComparisonToComparerAdapter(Comparison<T> comparison)
        {
            _comparison = comparison;
        }

        public int Compare(object x, object y)
        {
            return _comparison((T)x, (T)y);
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            ArrayList a = new ArrayList() { 1, 5, 3, 3, 2, 4, 3 };
            var adapter = new ComparisonToComparerAdapter<int>(IntComparer);
            a.Sort(adapter);
            foreach ( var x in a)
            {
                Console.Write("{0}, ", x);
            }

            Console.ReadLine();
        }

        static int IntComparer(int x, int y)
        {
            return x.CompareTo(y);
        }

    }
}
