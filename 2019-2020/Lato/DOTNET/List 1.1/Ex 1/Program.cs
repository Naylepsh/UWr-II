using System;
using System.Collections.Generic;

namespace ex1
{
    class Program
    {
        static void Main(string[] args)
        {
            List<int> xs = new List<int>();
            for (int i = 1; i <= 100000; i++) {
                if (Foo(i)) {
                    xs.Add(i);
                }
            }
            foreach (var x in xs) {
                Console.WriteLine(x);
            }
        }

        static bool Foo(int x) {
            int digitSum = 0;
            for (int y = x; y > 0; y /= 10) {
                int digit = y % 10;
                if (digit == 0 || x % digit != 0) {
                    return false;
                }
                digitSum += digit;
            }
            return x % digitSum == 0;
        }
    }
}
