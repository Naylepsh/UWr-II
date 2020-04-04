using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;

namespace Ex_1
{
    public class BasicSingleton
    {
        private static BasicSingleton _instance = null;
        private static object _lock = new object();

        public static BasicSingleton Instance()
        {
            if (_instance == null)
            {
                lock (_lock)
                {
                    if (_instance == null)
                    {
                        _instance = new BasicSingleton();
                    }
                }
            }
            return _instance;
        }
    }

    public class ThreadSingleton
    {
        private static ThreadLocal<ThreadSingleton> _instance = new ThreadLocal<ThreadSingleton>();
        private static object _lock = new object();

        public static ThreadSingleton Instance()
        {
            if (_instance.Value == null)
            {
                lock (_lock)
                {
                    if (_instance.Value == null)
                    {
                        _instance.Value = new ThreadSingleton();
                    }
                }
            }
            return _instance.Value;
        }
    }

    public class FiveSecondSingleton
    {
        private static FiveSecondSingleton _instance = null;
        private static DateTime timer;
        private static object _lock = new object();

        private static bool IsNewInstanceAllowed()
        {
            if (_instance == null)
            {
                return true;
            }
            TimeSpan time = DateTime.Now - timer;
            return time.Seconds >= 5;
        }

        public static FiveSecondSingleton Instance()
        {
            if (IsNewInstanceAllowed())
            {
                lock (_lock)
                {
                    if (IsNewInstanceAllowed())
                    {
                        timer = DateTime.Now;
                        _instance = new FiveSecondSingleton(); 
                    }
                }
            }
            return _instance;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            BasicSingleton s1 = BasicSingleton.Instance();
            BasicSingleton s2 = BasicSingleton.Instance();

            Console.ReadLine();
        }
    }
}
