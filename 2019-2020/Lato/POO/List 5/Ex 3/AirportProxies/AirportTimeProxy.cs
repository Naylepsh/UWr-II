using System;
using AirportPool;

namespace AirportProxies
{
    public class AirportTimeProxy
    {
        private const int OPENING_HOUR = 8;
        private const int CLOSING_HOUR = 22;
        private Airport _airport;

        public AirportTimeProxy(int capacity)
        {
            _airport = new Airport(capacity);
        }

        private void EnsureProperTime()
        {
            int currentHour = DateTime.Now.Hour;
            if (currentHour < OPENING_HOUR || currentHour >= CLOSING_HOUR)
            {
                throw new UnauthorizedAccessException(
                    string.Format("Access to Airport only allowed during {0}-{1}.",
                        OPENING_HOUR, CLOSING_HOUR));
            }
        }

        public Plane AquirePlane()
        {
            EnsureProperTime();
            return _airport.AquirePlane();
        }

        public void ReleasePlane(Plane plane)
        {
            EnsureProperTime();

            _airport.ReleasePlane(plane);
        }
    }
}
