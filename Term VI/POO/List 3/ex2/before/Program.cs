using System;

namespace before
{
  class Program
  {
    static void Main(string[] args)
    {
      var reportPrinter = new ReportPrinter();
      reportPrinter.PrintReport();
    }
  }

  public class ReportPrinter
  {
    private string _data;

    public string GetData()
    {
      return "sample-data\nand-some-more-content";
    }

    public void FormatDocument()
    {
      string formatted = "";

      int i = 0;
      foreach (string line in _data.Split('\n'))
      {
        formatted += String.Format("{0}: {1}\n", i, line);
        i++;
      }

      _data = formatted;
    }

    public void PrintReport()
    {
      _data = GetData();
      FormatDocument();
      Console.Write(_data);
    }
  }
}
