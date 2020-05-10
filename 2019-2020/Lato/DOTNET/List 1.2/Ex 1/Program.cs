using System.Linq;
using System;
using System.Collections.Generic;

namespace Ex_1
{
  class Program
  {
    const int numberOfItems = 1000;
    static void Main(string[] args)
    {
      if (args.Length != 1)
      {
        Console.WriteLine("Invalid number of args: {0}; Need exactly 1", args.Length);
        return;
      }
      int testNumber = Int16.Parse(args[0]);

      List<int> list = new List<int>();
      ListUtils.FillList(list, numberOfItems);
      Dictionary<int, int> dict = new Dictionary<int, int>();
      DictUtils.FillDict(dict, numberOfItems);

      DateTime start = DateTime.Now;
      switch (testNumber)
      {
        case 0: FillList(); break;
        case 1: RemoveFromList(list); break;
        case 2: RemoveFromListBackwards(list); break;
        case 3: SearchInList(list); break;
        case 4: FillDict(); break;
        case 5: RemoveFromDict(dict); break;
        case 6: SearchInDict(dict); break;
      }
      DateTime end = DateTime.Now;
      TimeSpan time = end - start;
      Console.WriteLine("Elapsed time: {0}", end - start);
    }

    static void FillList()
    {
      Console.WriteLine("Filling list with {0} items", numberOfItems);
      List<int> list = new List<int>();
      ListUtils.FillList(list, numberOfItems);
    }

    static void RemoveFromList(List<int> list)
    {
      Console.WriteLine("Removing {0} items from list", numberOfItems);
      ListUtils.RemoveFromList(list, numberOfItems);
    }

    static void RemoveFromListBackwards(List<int> list)
    {
      Console.WriteLine("Removing {0} items from list backwards", numberOfItems);
      ListUtils.RemoveFromListBackwards(list, numberOfItems);
    }

    static void SearchInList(List<int> list)
    {
      Console.WriteLine("Searching for {0} items in list", numberOfItems);
      ListUtils.SearchInList(list, numberOfItems);
    }

    static void FillDict()
    {
      Console.WriteLine("Filling dict with {0} items", numberOfItems);
      Dictionary<int, int> dict = new Dictionary<int, int>();
      DictUtils.FillDict(dict, numberOfItems);
    }

    static void RemoveFromDict(Dictionary<int, int> dict)
    {
      Console.WriteLine("Removing {0} items from dict", numberOfItems);
      DictUtils.RemoveFromDict(dict, numberOfItems);
    }

    static void SearchInDict(Dictionary<int, int> dict)
    {
      Console.WriteLine("Searching for {0} items in dict", numberOfItems);
      DictUtils.SearchInDict(dict, numberOfItems);
    }
  }

  class ListUtils
  {
    public static void FillList(List<int> list, int numOfItems)
    {
      for (int i = 0; i < numOfItems; i++)
      {
        list.Add(i);
      }
    }

    public static void RemoveFromList(List<int> list, int numOfItems)
    {
      for (int i = 0; i < numOfItems; i++)
      {
        list.RemoveAt(0);
      }
    }

    public static void RemoveFromListBackwards(List<int> list, int numOfItems)
    {
      for (int i = numOfItems - 1; i > 0; i--)
      {
        list.RemoveAt(i);
      }
    }

    public static void SearchInList(List<int> list, int numOfItems)
    {
      for (int i = 0; i < numOfItems; i++)
      {
        if (i % 2 == 0)
        {
          list.ElementAt(i);
        }
        else
        {
          list.ElementAt(numOfItems - i);
        }
      }
    }
  }

  class DictUtils
  {
    public static void FillDict(Dictionary<int, int> dict, int numOfItems)
    {
      for (int i = 0; i < numOfItems; i++)
      {
        dict.Add(i, i);
      }
    }

    public static void RemoveFromDict(Dictionary<int, int> dict, int numOfItems)
    {
      for (int i = 0; i < numOfItems; i++)
      {
        dict.Remove(i);
      }
    }

    public static void SearchInDict(Dictionary<int, int> dict, int numOfItems)
    {
      for (int i = 0; i < numOfItems; i++)
      {
        if (i % 2 == 0)
        {
          dict.ElementAt(i);
        }
        else
        {
          dict.ElementAt(numOfItems - i);
        }
      }
    }
  }
}
