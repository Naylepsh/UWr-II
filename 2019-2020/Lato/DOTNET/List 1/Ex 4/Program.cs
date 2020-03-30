using System;
using System.Reflection;
using System.Collections.Generic;

namespace ex4
{
    class Program
    {
        static void Main(string[] args)
        {
            var methods = GetNullaryMethodsOfType(new Foo(), typeof (int));
            InvokeMethodsOfAttribute(methods, typeof (OznakowaneAttribute));
        }

        static List<MethodInfo> GetNullaryMethodsOfType(object src, Type type) {
            List<MethodInfo> filtered = new List<MethodInfo>();
            var methods = src.GetType().GetMethods();
            foreach (var method in methods) {
                var returnParamType = method.ReturnParameter.ParameterType;
                var paramLength = method.GetParameters().Length;
                if (returnParamType == type && paramLength == 0) {
                    filtered.Add(method);
                }
            }
            return filtered;
        }

        static void InvokeMethodsOfAttribute(List<MethodInfo> methods, Type type) {
            foreach (var method in methods) {
                var attr = method.GetCustomAttribute(type);
                if (attr != null) {
                    method.Invoke(new Foo(), null);
                }
            }
        }
    }

    class OznakowaneAttribute : Attribute {

        public OznakowaneAttribute() {
        }
    }

    class Foo {
        public int Bar() {
            return 4;
        }

        public char Baz() {
            return '4';
        }

        public int Quz(int x) {
            return 4;
        }

        [Oznakowane]
        public int Quux() {
            Console.WriteLine("Hello");
            return 4;
        }
    }


}
