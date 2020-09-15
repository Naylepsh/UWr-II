using System;
using System.Threading;
using Ex_1;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace SingletonsTest
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void TestBasicSingleton()
        {
            BasicSingleton s1 = BasicSingleton.Instance();
            BasicSingleton s2 = BasicSingleton.Instance();

            Assert.IsNotNull(s1);
            Assert.IsNotNull(s2);
            Assert.AreSame(s1, s2);
        }

        [TestMethod]
        public void TestBasicSingletonConcurrently()
        {
            Thread t1, t2;
            BasicSingleton s1 = null, s2 = null;

            t1 = new Thread(() =>
            {
                s1 = BasicSingleton.Instance();
            }
                );
            t2 = new Thread(() =>
            {
                s2 = BasicSingleton.Instance();
            }
                );

            t1.Start();
            t2.Start();
            t1.Join();
            t2.Join();

            Assert.IsNotNull(s1);
            Assert.IsNotNull(s2);
            Assert.AreSame(s1, s2);
        }

        [TestMethod]
        public void TestBasicSingletonConcurrently2()
        {
            Thread t1, t2;
            BasicSingleton s1 = null, s2 = null;

            t1 = new Thread(() =>
            {
                s1 = BasicSingleton.Instance();
            }
                );
            t2 = new Thread(() =>
            {
                s2 = BasicSingleton.Instance();
            }
                );

            t1.Start();
            t2.Start();
            t2.Join();
            t1.Join();

            Assert.IsNotNull(s1);
            Assert.IsNotNull(s2);
            Assert.AreSame(s1, s2);
        }

        [TestMethod]
        public void TestThreadSingleton()
        {
            ThreadSingleton s1 = ThreadSingleton.Instance();
            ThreadSingleton s2 = ThreadSingleton.Instance();

            Assert.IsNotNull(s1);
            Assert.IsNotNull(s2);
            Assert.AreSame(s1, s2);
        }

        [TestMethod]
        public void TestThreadSingletonConcurrently()
        {
            Thread t1, t2;
            ThreadSingleton s1 = null, s2 = null;

            t1 = new Thread(() =>
            {
                s1 = ThreadSingleton.Instance();
            }
                );
            t2 = new Thread(() =>
            {
                s2 = ThreadSingleton.Instance();
            }
                );

            t1.Start();
            t2.Start();
            t2.Join();
            t1.Join();

            Assert.IsNotNull(s1);
            Assert.IsNotNull(s2);
            Assert.AreNotSame(s1, s2);
        }

        [TestMethod]
        public void TestThreadSingletonConcurrently2()
        {
            Thread t1, t2;
            ThreadSingleton s1 = null, s2 = null;

            t1 = new Thread(() =>
            {
                s1 = ThreadSingleton.Instance();
            }
                );
            t2 = new Thread(() =>
            {
                s2 = ThreadSingleton.Instance();
            }
                );

            t1.Start();
            t2.Start();
            t1.Join();
            t2.Join();

            Assert.IsNotNull(s1);
            Assert.IsNotNull(s2);
            Assert.AreNotSame(s1, s2);
        }

        [TestMethod]
        public void Test5SecondSingletonImmediate()
        {
            FiveSecondSingleton s1 = FiveSecondSingleton.Instance();
            FiveSecondSingleton s2 = FiveSecondSingleton.Instance();

            Assert.IsNotNull(s1);
            Assert.IsNotNull(s2);
            Assert.AreSame(s1, s2);
        }

        [TestMethod]
        public void Test5SecondSingletonWithDelay()
        {
            FiveSecondSingleton s1 = FiveSecondSingleton.Instance();
            Thread.Sleep(TimeSpan.FromSeconds(4));
            FiveSecondSingleton s2 = FiveSecondSingleton.Instance();
            Thread.Sleep(TimeSpan.FromSeconds(6));
            FiveSecondSingleton s3 = FiveSecondSingleton.Instance();

            Assert.IsNotNull(s1);
            Assert.IsNotNull(s2);
            Assert.IsNotNull(s3);
            Assert.AreSame(s1, s2);
            Assert.AreNotSame(s1, s3);
        }
    }
}
