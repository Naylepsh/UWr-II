using System;
using System.Linq;

namespace Ex_6
{
  class Program
  {
    static void Main(string[] args)
    {
      string path = @".\logs.txt";
      string[] lines = System.IO.File.ReadAllLines(path);
      int itemsToShow = 3;
      var res =
      lines
        .Select(line => StringToRequest(line))
        .GroupBy(request => request.IP)
        .Select(group => new
        {
          IP = group.Key,
          Count = group.Count()
        })
        .OrderByDescending(x => x.Count)
        .Take(itemsToShow);

      foreach (var item in res)
      {
        Console.WriteLine("{0} {1}", item.IP, item.Count);
      }
    }

    static Request StringToRequest(string str)
    {
      string[] data = str.Split(' ');
      string time = data[0];
      string ip = data[1];
      string requestType = data[2];
      string resource = data[3];
      string status = data[4];
      return new Request(time, ip, requestType, resource, status);
    }
  }

  public class Request
  {
    public string Time { get; set; }
    public string IP { get; set; }
    public string Type { get; set; }
    public string Resource { get; set; }
    public string ResponseStatus { get; set; }

    public Request(string time, string ip, string requestType, string resource, string responseStatus)
    {
      Time = time;
      IP = ip;
      Type = requestType;
      Resource = resource;
      ResponseStatus = responseStatus;
    }
  }
}
