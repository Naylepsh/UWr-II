using System;

namespace after
{
  class Program
  {
    static void Main(string[] args)
    {
      var reportProvider = new ReportDataProvider();
      var reportFormatter = new ReportFormatter();
      var reportPrinter = new ReportPrinter();

      string report = reportProvider.GetData();
      string formattedReport = reportFormatter.FormatDocument(report);
      reportPrinter.PrintReport(formattedReport);
    }
  }

  class ReportPrinter
  {
    public void PrintReport(string report)
    {
      Console.Write(report);
    }
  }

  class ReportFormatter : IDocumentFormatter
  {
    public string FormatDocument(string report)
    {
      string formatted = "";

      int i = 0;
      foreach (string line in report.Split('\n'))
      {
        formatted += String.Format("{0}: {1}\n", i, line);
        i++;
      }

      return formatted;
    }
  }

  class ReportDataProvider : IDataProvider
  {
    public string GetData()
    {
      return "sample-data\nand-some-more-content";
    }
  }

  interface IDataProvider
  {
    string GetData();
  }

  interface IDocumentFormatter
  {
    string FormatDocument(string report);
  }
}
