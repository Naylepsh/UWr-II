using System;
using NullObject;

namespace ClientCode
{
    class Program
    {
        static void Main(string[] args)
        {
            ILogger logger1 = LoggerFactory.Instance.GetLogger(LogType.File, @"./foo.txt");
            logger1.Log("foo bar"); // logowanie do pliku

            ILogger logger2 = LoggerFactory.Instance.GetLogger(LogType.None);
            logger2.Log("qux"); // brak logowania

            ILogger logger3 = LoggerFactory.Instance.GetLogger(LogType.Console);
            logger3.Log("baz"); // logowanie do konsoli

            Console.ReadKey();
        }
    }
}
