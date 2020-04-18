using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NullObject
{
    public interface ILogger {
        void Log(string Message);
    }

    public enum LogType { None, Console, File }

    public class FileLogger : ILogger
    {
        private string _pathToFile;

        public FileLogger(string pathToFile)
        {
            _pathToFile = pathToFile;
        }

        public void Log(string Message)
        {
            using (StreamWriter w = File.AppendText(_pathToFile))
            {
                w.WriteLine(Message);
            }
        }
    }

    public class ConsoleLogger : ILogger
    {
        public void Log(string Message)
        {
            Console.WriteLine(Message);
        }
    }

    public class NoneLogger : ILogger
    {
        public void Log(string Message)
        {
        }
    }

    public class LoggerFactory {
        private static LoggerFactory _instance;

        public ILogger GetLogger(LogType LogType, string Parameters = null) {
            ILogger logger = new NoneLogger();

            switch (LogType)
            {
                case LogType.Console:
                    logger = new ConsoleLogger();
                    break;
                case LogType.File:
                    logger = new FileLogger(Parameters);
                    break;
            }

            return logger;

        }

        public static LoggerFactory Instance {
            get
            {
                if (_instance == null)
                {
                    _instance = new LoggerFactory();
                }
                return _instance;
            }
        }
    }
}
