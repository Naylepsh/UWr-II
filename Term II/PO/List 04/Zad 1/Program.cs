using System;

namespace Zad_1
{
    class Program
    {
        static void Main()
        {
            var pc = new PrimeCollection();
            Console.Write("First 6 primes: ");
            for (int i = 0; i < 6; i++)
                Console.Write(pc.Next() + " ");
            Console.WriteLine("\nNow accessing the 6th prime: pc[5] = " + pc[5]);
            Console.WriteLine(pc);
            for (int i = 0; i < 6; i++)
                pc.Next();
            Console.WriteLine(pc);
            Console.WriteLine("Current length of pc: " + pc.Length);
            
            var pc2 = new PrimeCollection();
            Console.WriteLine("Printing first 13 primes. The last prime should be " + pc[12]);
            for (int i = 0; i < 13; i++)
                Console.Write(pc2.Next() + " ");
            
            Console.WriteLine("\nNow testing IntCollection");
            var ic = new IntCollection();
            for (int i = 0; i < 20; i++)
                Console.Write(ic.Next() + " ");
        }
    }
}