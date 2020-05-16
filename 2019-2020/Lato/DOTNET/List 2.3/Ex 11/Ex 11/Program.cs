using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.CodeDom.Compiler;
using Microsoft.CSharp;

namespace Ex_11
{
    class Program
    {
        public static string userClassName = "UserClass";
        public static string userMethodName = "Foo";
        static void Main(string[] args)
        {
            var code = GetUserInput();
            var compiled = Compile(code);
            var result = Run(compiled);

            Console.WriteLine(result);
        }

        static string GetUserInput()
        {
            StringBuilder input = new StringBuilder();
            string prev = null;
            string current = null;

            string openingString = $"public class {userClassName} {{\n\tpublic string {userMethodName}() {{";
            input.AppendLine(openingString);

            Console.WriteLine("Press <Enter> twice to finalize function creation.");
            Console.WriteLine($"Using System;\n\n {openingString}");

            while (true)
            {
                Console.Write("\t\t");
                string line = Console.ReadLine();

                prev = current;
                current = line;
                if (prev == current && current.Length == 0)
                {
                    break;
                }

                input.AppendLine(current);
            }

            string closingString = "\t}\n}";

            input.AppendLine(closingString);
            Console.WriteLine(closingString);

            return input.ToString();
        }

        static CompilerResults Compile(string code)
        {
            var options = new CompilerParameters();
            options.GenerateExecutable = false;
            options.GenerateInMemory = false;

            var provider = new CSharpCodeProvider();
            var compile = provider.CompileAssemblyFromSource(options, code);

            return compile;
        }

        static object Run(CompilerResults compiled)
        {
            var type = compiled.CompiledAssembly.GetType(userClassName);
            var userClass = Activator.CreateInstance(type);

            var method = type.GetMethod(userMethodName);
            var result = method.Invoke(userClass, null);

            return result;
        }
    }
}
