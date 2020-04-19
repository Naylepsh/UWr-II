using System;
using AirportPool;
using AirportProxies;

namespace ConsoleApp1
{
    class Program
    {
        static void Main(string[] args)
        {
            int capacity = 1;
            IAirport airportTimed = new AirportTimeProxy(capacity);
            IAirport airportLogged = new AirportLoggingProxy(capacity);
            Plane plane;

            try
            {
                plane = airportTimed.AquirePlane();
            }
            catch (UnauthorizedAccessException e)
            {
                Console.WriteLine(e.Message);
            }

            plane = airportLogged.AquirePlane();
            airportLogged.ReleasePlane(plane);

            Console.ReadLine();
        }
    }
}
