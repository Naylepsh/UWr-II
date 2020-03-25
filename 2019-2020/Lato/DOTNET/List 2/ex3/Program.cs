using System.Diagnostics;
using System.Collections.Generic;
using System;

namespace ex3
{
    class Program
    {
        static void Main(string[] args)
        {
            ConvertAll();
            FindAll();
            ForEach();
            RemoveAll();
            Sort();
        }

        static void ConvertAll() {
            Console.Write("Convert All===");
            List<int> intList = new List<int>();
            for (int i = 0; i < 5; i++) {
                intList.Add(i);
            }
            List<string> stringList = intList.ConvertAll( x => (x+7).ToString() );
            foreach (var str in stringList) {
                Console.Write("{0}, ", str);
            }
            Console.WriteLine();
        }

        static void FindAll() {
            Console.Write("Find All===");
            List<int> intList = new List<int>();
            for (int i = 0; i < 5; i++) {
                intList.Add(i);
            }
            foreach (var x in intList.FindAll( y => y > 0 )) {
                Console.Write("{0}, ", x);
            }
            Console.WriteLine();
        }

        static void ForEach() {
            Console.Write("For Each===");
            List<int> intList = new List<int>();
            for (int i = 0; i < 5; i++) {
                intList.Add(i);
            }
            intList.ForEach( x => { 
                int y = x + 42;
                Console.Write("{0}, ", y);
            });
            Console.WriteLine();
        }

        static void RemoveAll() {
            Console.Write("Remove All===");
            List<int> intList = new List<int>();
            for (int i = 0; i < 5; i++) {
                intList.Add(i);
            }
            intList.RemoveAll( x => x > 2 );
            foreach (var x in intList) {
                Console.Write("{0}, ", x);
            }
            Console.WriteLine();
        }

        static void Sort() {
            Console.Write("Sort===");
            List<int> intList = new List<int>();
            for (int i = 0; i < 5; i++) {
                intList.Add(i);
                intList.Add(10-i);
            }
            intList.Sort( (x, y) => x.CompareTo(y) );
            foreach (var x in intList) {
                Console.Write("{0}, ", x);
            }
            Console.WriteLine();
        }
    }
}
