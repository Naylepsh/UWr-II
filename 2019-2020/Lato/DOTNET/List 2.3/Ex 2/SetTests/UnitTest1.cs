using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Ex_2;
using System.Collections;

namespace SetTests
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void Should_AddObject()
        {
            Set set = new Set();
            set.Add(1);

            Assert.AreEqual(set.Count, 1);
        }

        [TestMethod]
        public void Should_NotAddObject_WhenDuplicateExists()
        {
            Set set = new Set();
            set.Add(1);
            set.Add(1);

            Assert.AreEqual(set.Count, 1);
        }

        [TestMethod]
        public void Should_RemoveObject_IfExists()
        {
            Set set = new Set
            {
                1
            };

            set.Remove(1);

            Assert.AreEqual(set.Count, 0);
        }

        [TestMethod]
        public void Should_ThrowException_IfRemovingObjectNotInSet()
        {
            Set set = new Set { 1 };

            Assert.ThrowsException<ArgumentException>(() =>
            {
                set.Remove(2);
            });
        }

        [TestMethod]
        public void Should_HaveCountAndValuesCountEqual()
        {
            Set set = new Set { 1 };

            Assert.AreEqual(set.Count, set.Values.Count);
        }

        [TestMethod]
        public void Should_IterateOverAllObjects()
        {
            Set set = new Set { 1, 2, 3 };

            int count = 0;
            foreach (object o in set)
            {
                count++;
            }

            Assert.AreEqual(set.Count, count);
        }

        [TestMethod]
        public void Should_IterateOverExistingObjects()
        {
            ArrayList array = new ArrayList { 1, 2, 3 };
            Set set = new Set();
            foreach (int x in array)
            {
                set.Add(x);
            }

            foreach (int x in set)
            {
                Assert.IsTrue(array.Contains(x));
            }

        }
    }
}
