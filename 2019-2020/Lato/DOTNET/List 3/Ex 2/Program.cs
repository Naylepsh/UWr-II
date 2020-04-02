using System.Collections;
using System.Linq;
using System;
/*
  Roznica miedzy parametrami operatorow where/orderby a parametrami funkcji Where,OrderBy:
  W funkcjach podajemy całe delegaty / lambdy, zaś w operatorach podajemy same ciała funkcji
*/

namespace Ex_2
{
  class Program
  {
    static void Main(string[] args)
    {
      ReadFromFile();
    }

    static void ReadFromFile()
    {
      string[] lines = System.IO.File.ReadAllLines(@".\numbers.txt");
      LinqMethods(lines);
      Console.WriteLine();
      LinqExpressions(lines);
    }

    static void LinqMethods(string[] lines)
    {
      var numbers = lines
            .Select(str => Int32.Parse(str))
            .Where(x => x > 100)
            .OrderByDescending(x => x);

      foreach (int x in numbers)
      {
        Console.Write("{0} ", x);
      }
    }

    static void LinqExpressions(string[] lines)
    {
      int[] linesInt = lines
      .Select(str => Int32.Parse(str)).ToArray();

      foreach (int e in
      from x in linesInt
      where x > 100
      orderby -x
      select x)
      {
        Console.Write("{0} ", e);
      }
    }
  }
}
