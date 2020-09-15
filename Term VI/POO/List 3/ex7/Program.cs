using System;

namespace ex7
{
  class Program
  {
    static void Main(string[] args)
    {
      var reportProvider = new ReportDataProvider();
      var reportFormatter = new ReportFormatter();
      var reportPrinter = new ReportPrinter();
      var reportComposer = new ReportComposer(reportProvider, reportFormatter, reportPrinter);

      reportComposer.ComposeReport();
    }
  }

  public class ReportComposer
  {
    private IDataProvider _dataProvider;
    private IDocumentFormatter _formater;
    private IReportPrinter _printer;

    public ReportComposer(IDataProvider dataProvider, IDocumentFormatter formatter, IReportPrinter printer)
    {
      _dataProvider = dataProvider;
      _formater = formatter;
      _printer = printer;
    }

    public void ComposeReport()
    {
      string report = _formater.FormatDocument(_dataProvider.GetData());
      _printer.PrintReport(report);
    }

  }

  class ReportPrinter : IReportPrinter
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

  public interface IDataProvider
  {
    string GetData();
  }

  public interface IDocumentFormatter
  {
    string FormatDocument(string report);
  }

  public interface IReportPrinter
  {
    void PrintReport(string report);
  }
}
