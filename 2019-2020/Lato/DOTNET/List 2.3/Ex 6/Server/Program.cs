using Ex_6;
using System;
using System.IO;
using System.Net;
using System.Net.Sockets;
using System.Runtime.Serialization.Formatters.Soap;

namespace Server
{
    internal class Program
    {
        private static void Main(string[] args)
        {
            SoapFormatter formatter = new SoapFormatter();
            IPAddress address = IPAddress.Parse("127.0.0.1");
            TcpListener server = new TcpListener(address, 8080);

            using (var fs = new FileStream("server-output.soap", FileMode.Create, FileAccess.Write))
            {
                try
                {
                    server.Start();
                    TcpClient client;
                    while (true)
                    {
                        Console.WriteLine("Waiting for connection...");
                        client = server.AcceptTcpClient();
                        Console.WriteLine("Client connected");

                        NetworkStream stream = client.GetStream();

                        Complex complex = (Complex)formatter.Deserialize(stream);

                        Console.WriteLine($"Complex number received: {complex.Real} + {complex.Imaginary}i");

                        // To save an object to a file we have to serialize it again, it was kinda pointless?
                        formatter.Serialize(fs, complex);
                        client.Close();
                    }
                }
                catch (SocketException e)
                {
                    Console.WriteLine("SocketException: {0}", e);
                }
                finally
                {
                    server.Stop();
                }

                Console.Read();
            }
        }
    }
}
