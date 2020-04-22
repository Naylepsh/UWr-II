using System.Collections.Generic;
using System.Linq;
using System;

namespace Ex_7
{
  class Program
  {
    static void Main(string[] args)
    {
      // Mozna tak
      var list = new[]
      {
          new { Field1 = "The value", Field2 = 5 },
          new { Field1 = "The other value", Field2 = 50 }
      }.ToList();

      foreach (var item in list)
      {
        Console.WriteLine(String.Format("Field1={0}, Field2={1}", item.Field1, item.Field2));
      }
      Console.WriteLine();

      // Albo tak
      var thing = new { Field1 = "The value", Field2 = 5 };
      var theList = CreateEmptyGenericList(thing);
      theList.Add(new { Field1 = "The value", Field2 = 5 });
      theList.Add(new { Field1 = "The other value", Field2 = 50 });

      foreach (var item in theList)
      {
        Console.WriteLine(String.Format("Field1={0}, Field2={1}", item.Field1, item.Field2));
      }
    }

    public static List<T> CreateEmptyGenericList<T>(T sampleObject)
    {
      return new List<T>();
    }
  }
}
