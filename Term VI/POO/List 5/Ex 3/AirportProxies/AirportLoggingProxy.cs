using System;
using AirportPool;

namespace AirportProxies
{
    public class AirportLoggingProxy : IAirport
    {
        private Airport _airport;

        public AirportLoggingProxy(int capacity)
        {
            _airport = new Airport(capacity);
        }

        public Plane AquirePlane()
        {
            Console.WriteLine("{0} - AquirePlane - No params", DateTime.Now);
            var plane = _airport.AquirePlane();
            Console.WriteLine("{0} - returning: {1}", DateTime.Now, plane);
            return plane;
        }

        public void ReleasePlane(Plane plane)
        {
            Console.WriteLine("{0} - ReleasePlane - {1}", DateTime.Now, plane.ToString());
            _airport.ReleasePlane(plane);
            Console.WriteLine("{0} - returning: No params", DateTime.Now);
        }
    }
}