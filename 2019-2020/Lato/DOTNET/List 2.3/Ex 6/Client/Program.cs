using Ex_6;
using System;
using System.IO;
using System.Net.Sockets;
using System.Runtime.Serialization.Formatters.Binary;

namespace Client
{
    internal class Program
    {
        private static void Main(string[] args)
        {
            BinaryFormatter formatter = new BinaryFormatter();

            while (true)
            {
                TcpClient client;
                try
                {
                    client = new TcpClient("localhost", 8080);
                }
                catch (Exception e)
                {
                    Console.WriteLine(e.Message);
                    Console.ReadKey();
                    return;
                }

                Console.WriteLine("Connection established");
                Stream stream = client.GetStream();

                Console.WriteLine("Enter complex number in following format: real imaginary");
                string line = Console.ReadLine();
                double[] data;
                try
                {
                    data = Array.ConvertAll<string, double>(line.Split(" "), double.Parse);
                    if (data.Length != 2)
                    {
                        Console.WriteLine("Invalid line");
                        continue;
                    }
                }
                catch (Exception)
                {
                    Console.WriteLine("Something went wrong during parsing. Were you not following complex format perhaps?");
                    continue;
                }

                Complex complex = new Complex { Real = data[0], Imaginary = data[1] };

                Console.WriteLine($"Sending complex = {complex.Real} + {complex.Imaginary}i");
                formatter.Serialize(stream, complex);
                client.Close();
            }
        }
    }
}