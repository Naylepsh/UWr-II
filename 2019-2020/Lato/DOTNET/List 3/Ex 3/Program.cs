using System;
using System.Linq;

namespace Ex_3
{
  class Program
  {
    static void Main(string[] args)
    {
      string[] surnames = System.IO.File.ReadAllLines(@".\surnames.txt");
      var res =
        from surname in surnames
        group surname by surname[0] into firstLetters
        orderby firstLetters.Key
        select new { firstLetter = firstLetters.Key };

      foreach (var firstLetter in res)
      {
        Console.WriteLine("{0} ", firstLetter.firstLetter);
      }
    }
  }
}
