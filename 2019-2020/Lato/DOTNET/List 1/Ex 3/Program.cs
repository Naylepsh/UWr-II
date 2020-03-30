using System;
using System.Reflection;

namespace ex3
{
    class Program
    {
        static void Main(string[] args)
        {
            Foo foo = new Foo();
            // Console.WriteLine(GetPrivateField(foo, "value"));
            // Console.WriteLine(GetPrivateMethod(foo, "getVal"));

            // Reflection
            int n = 1000;
            DateTime start = DateTime.Now;
            for (int i = 0; i < n; i++) {
                var x = GetPublicField(foo, "x");
            }
            DateTime end = DateTime.Now;
            TimeSpan time = end - start;
            Console.WriteLine("Reflection access test time: {0}", end - start); // About 00:00:00.0042868

            // Normal
            start = DateTime.Now;
            for (int i = 0; i < n; i++) {
                var x = foo.x;
            }
            end = DateTime.Now;
            time = end - start;
            Console.WriteLine("Normal access test time: {0}", end - start); // About 00:00:00.0000064
        }

        public static object GetPrivateField(object src, string fieldName) {
            return src.GetType().GetField(fieldName, BindingFlags.NonPublic | BindingFlags.Instance).GetValue(src);
        }

        public static object GetPrivateMethod(object src, string methodName) {
            return src.GetType().GetMethod(methodName, BindingFlags.NonPublic | BindingFlags.Instance).Invoke(src, null);
        }

        public static object GetPublicField(object src, string fieldName) {
            return src.GetType().GetField(fieldName).GetValue(src);
        }
    }

    class Foo {
        private int value = 42;
        public int x = 7;

        private int getVal() {
            return this.value;
        }

        public Foo() {}
    }
}
