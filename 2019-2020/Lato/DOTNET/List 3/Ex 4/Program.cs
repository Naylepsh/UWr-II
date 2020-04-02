using System;
using System.Linq;

namespace Ex_4
{
  class Program
  {
    static void Main(string[] args)
    {
      string path = "./";
      var files = System.IO.Directory.GetFiles(path);
      int totalLength =
      files
        .Select(pathToFile => GetFileLength(pathToFile))
        .Aggregate((result, next) => result + next);
      Console.WriteLine(totalLength);
    }

    // Works on both \ and / directory separators
    static int GetFileLength(string path)
    {
      string[] pathElements = path.Split(new char[] { '\\', '/' });
      string fileName = pathElements[pathElements.Length - 1];
      return fileName.Length;
    }
  }
}
