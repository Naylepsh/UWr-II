using System;
using System.Collections.Generic;

namespace AirportPool
{
    public class Plane
    {

    }

    public interface IAirport
    {
        Plane AquirePlane();
        void ReleasePlane(Plane plane);
    }

    public class Airport : IAirport
    {
        private int _capacity;
        private List<Plane> _ready = new List<Plane>();
        private List<Plane> _released = new List<Plane>();

        public Airport(int capacity)
        {
            if (capacity <= 0)
            {
                throw new ArgumentException("Capacity cannot be below or equal to 0");
            }
            _capacity = capacity;
        }

        public Plane AquirePlane()
        {
            if (_released.Count >= _capacity)
            {
                throw new ArgumentOutOfRangeException("Cannot release any more planes");
            }

            if (_ready.Count == 0)
            {
                Plane newPlane = new Plane();
                _ready.Add(newPlane);
            }

            Plane plane = _ready[0];
            _ready.Remove(plane);
            _released.Add(plane);
            return plane;
        }

        public void ReleasePlane(Plane plane)
        {
            if (!_released.Contains(plane))
            {
                throw new ArgumentException("Given plane doesn't belong to this airport");
            }
            _released.Remove(plane);
            _ready.Add(plane);
        }
    }
}
