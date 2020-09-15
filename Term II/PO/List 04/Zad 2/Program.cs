using System;

namespace Zad_2
{
    class Program
    {
        static void Main(string[] args)
        {
            var pc = new PrimeCollection();
            foreach (int e in pc)
            {
                Console.WriteLine(e);
                Console.ReadKey();
            }
        }
    }
}