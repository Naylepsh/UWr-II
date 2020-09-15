using System;


namespace Zad_1
{
    class Program
    {
        public static void Main()
        {
            Console.WriteLine("Making a list");
            List<int> list = new List<int>();
            Console.WriteLine(list.ToString());
            Console.WriteLine("Adding -2- at the beginning.");
            list.AddAtBegin(2);
            Console.WriteLine(list.ToString());
            Console.WriteLine("Adding -3- at the beginning.");
            list.AddAtBegin(3);
            Console.WriteLine(list.ToString());
            Console.WriteLine("Adding -7- at the beginning.");
            list.AddAtBegin(7);
            Console.WriteLine(list.ToString());
            Console.WriteLine("Adding -8- at the end.");
            list.AddAtEnd(8);
            Console.WriteLine(list.ToString());
            Console.WriteLine("Adding -4- at the end.");
            list.AddAtEnd(4);
            Console.WriteLine(list.ToString());

            try
            {
                Console.WriteLine("Removing 2 times from the beginning.");
                Console.WriteLine("Removed: " + list.RemoveFromBegin());
                Console.WriteLine("Removed: " + list.RemoveFromBegin());
                Console.WriteLine("Now we have: " + list.ToString());
                Console.WriteLine("Removing 2 times from the end.");
                Console.WriteLine("Removed: " + list.RemoveFromEnd());
                Console.WriteLine("Removed: " + list.RemoveFromEnd());
                Console.WriteLine("Now we have: " + list.ToString());
                
                Console.WriteLine("Now we remove 'till our heart's content!");
                while (true)
                    Console.WriteLine("Removed: " + list.RemoveFromEnd());
            }
            catch (Exception exception)
            {
                Console.WriteLine("Oh no! List is already empty!");
            }
            Console.WriteLine("Adding -51- at the end");
            list.AddAtEnd(51);
            Console.WriteLine(list.ToString());
        }
    }
}
