using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex_1
{
    class Program
    {
        static void Main(string[] args)
        {
            int iters = 100000;
            int res;
            DateTime start = DateTime.Now;
            for (int i = 0; i < iters; i++)
            {
                res = Foo(i, i + 1);
            }
            DateTime end = DateTime.Now;
            TimeSpan time = end - start;
            Console.WriteLine("Foo: {0}", time);

            start = DateTime.Now;
            for (int i = 0; i < iters; i++)
            {
                res = DynamicFoo(i, i + 1);
            }
            end = DateTime.Now;
            time = end - start;
            Console.WriteLine("Dynamic: {0}", time);

            Console.ReadLine();
        }

        static int Foo(int x, int y)
        {
            return x * y + x / y;
        }

        static dynamic DynamicFoo(dynamic x, dynamic y)
        {
            return x * y + x / y;
        }
    }
}
