using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Ex_3;

namespace AirplaneTests
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void InvalidCapacity()
        {
            int capacity = 0;

            Assert.ThrowsException<ArgumentException>(
                () =>
                {
                    Airport airport = new Airport(capacity);
                });
        }

        [TestMethod]
        public void AquirePlane()
        {
            int capacity = 1;
            Airport airport = new Airport(capacity);
            Plane plane = airport.AquirePlane();

            Assert.IsNotNull(plane);
        }

        [TestMethod]
        public void CapacityDepleted()
        {
            int capacity = 1;
            Airport airport = new Airport(capacity);
            Plane plane1 = airport.AquirePlane();

            Assert.ThrowsException<ArgumentOutOfRangeException>(
                () =>
                {
                    Plane plane2 = airport.AquirePlane();
                });
        }

        [TestMethod]
        public void AquireDistinctPlanes()
        {
            int capacity = 2;
            Airport airport = new Airport(capacity);
            Plane plane1 = airport.AquirePlane();
            Plane plane2 = airport.AquirePlane();

            Assert.AreNotSame(plane1, plane2);
        }

        [TestMethod]
        public void ReusePlane()
        {
            int capacity = 1;
            Airport airport = new Airport(capacity);
            Plane plane1 = airport.AquirePlane();
            airport.ReleasePlane(plane1);
            Plane plane2 = airport.AquirePlane();

            Assert.AreEqual(plane1, plane2);
        }

        [TestMethod]
        public void ReleaseInvalidPlane()
        {
            int capacity = 1;
            Airport airport = new Airport(capacity);
            Plane plane = new Plane();

            Assert.ThrowsException<ArgumentException>(
                () =>
                {
                    airport.ReleasePlane(plane);
                });
        }
    }
}
