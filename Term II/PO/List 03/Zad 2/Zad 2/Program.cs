using System;
using Dictionary;

namespace Zad_2
{
    class Program
    {
        static void Main(string[] args)
        {
            var dict = new Dictionary<string, int>();
            dict.Add("Mamma mia", 1);
            dict.Add("It's me, Mario", 2);
            Console.WriteLine("Dict after adding two items: " + dict.ToString());
            dict.Add("Hey, sup", 3);
            Console.WriteLine("Dict after adding one more item: " + dict.ToString());
            dict.Add("Mamma mia", 4);
            Console.WriteLine("Dict after adding already existing item: " + dict.ToString());
            dict.Remove("Hey, sup");
            Console.WriteLine("Dict after removal of 'hey, sup': " + dict.ToString());
            Console.WriteLine("Finding the value of 'Mamma mia': " + dict.Find("Mamma mia"));

            Console.Write("Trying to find the value of non-existing key: ");
            try
            {
                Console.WriteLine(dict.Find("spam"));
            }
            catch (ArgumentException ae)
            {
                Console.WriteLine("Key does not exists.");
            }
        }
    }
}
