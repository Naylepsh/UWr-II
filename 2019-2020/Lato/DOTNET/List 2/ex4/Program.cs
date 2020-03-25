using System.Collections.Generic;
using System;

namespace ex4
{
    class Program
    {
        static void Main(string[] args)
        {
            List<int> intList = new List<int>();
            for (int i = 0; i < 5; i++) {
                intList.Add(i);
            }

            List<string> stringList = ListHelper.ConvertAll(
                intList, x => "str(" + x.ToString() + "}");
            Console.Write("ConvertAll===");
            foreach (var x in stringList) {
                Console.Write("{0}, ", x);
            }
            Console.WriteLine();

            List<int> foundItems = ListHelper.FindAll(intList, x => x % 2 == 0);
            Console.Write("FindAll(isEven)===");
            foreach (var x in foundItems) {
                Console.Write("{0}, ", x);
            }
            Console.WriteLine();

            Console.Write("ForEach(Print)===");
            ListHelper.ForEach(intList, x => { Console.Write("{0}, ", x); });
            Console.WriteLine();

            Console.Write("RemoveAll(StartsWith\"s\")===");
            int itemsRemoved = ListHelper.RemoveAll(stringList, x => x.StartsWith("s"));
            Console.WriteLine("{0}", itemsRemoved);

            intList.Reverse();
            Console.Write("Sort===");
            ListHelper.Sort(intList, (x, y) => x.CompareTo(y) );
            foreach (var x in intList) {
                Console.Write("{0}, ", x);
            }
        }
    }

    public class ListHelper {
        public static List<TOutput> ConvertAll<T, TOutput>(
            List<T> list,
            Converter<T, TOutput> converter ) {
                List<TOutput> convertedList = new List<TOutput>();
                foreach (T item in list) {
                    convertedList.Add(converter(item));
                }
                return convertedList;
            }

        public static List<T> FindAll<T>( List<T> list, Predicate<T> match ) {
            List<T> foundItems = new List<T>();
            foreach (T item in list) {
                if (match(item)) {
                    foundItems.Add(item);
                }
            }
            return foundItems;
        }

        public static void ForEach<T>( List<T> list, Action<T> action ) {
            foreach (T x in list) {
                action(x);
            }
        }

        public static int RemoveAll<T>( List<T> list, Predicate<T> match ) {
            var leftItems = FindAll(list, new Predicate<T>(x => !match(x)));
            return list.Count - leftItems.Count;
        }

        // Bubble Sort because why not
        public static void Sort<T>( List<T> list, Comparison<T> comparison ) {
            for (int i = 0; i < list.Count - 1; i++) {
                for (int j = i + 1; j < list.Count; j++) {
                    if (comparison(list[i], list[j]) == 1) {
                        T temp = list[i];
                        list[i] = list[j];
                        list[j] = temp;
                    }
                }
            }
        }
    }
}
