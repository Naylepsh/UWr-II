using System.Text.RegularExpressions;
using System;
using System.Linq;

namespace Ex_1
{
  class Program
  {
    static void Main(string[] args)
    {
      Console.WriteLine("Kobyła ma mały bok.".IsPalindrome());
      Console.WriteLine("Kobyża ma mały bok.".IsPalindrome());
    }
  }

  public static class StringExtensions
  {
    public static bool IsPalindrome(this string str)
    {
      var chars = str.ToLower().ToCharArray().Where(ch => Char.IsLetterOrDigit(ch)).ToArray();
      return chars.SequenceEqual(chars.Reverse());
    }
  }
}
