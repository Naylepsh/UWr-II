using Dependency_Injection_Engine;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;

namespace DIEngineTest
{
    [TestClass]
    public class UnitTest1
    {
        private class Foo
        {
        }

        private class Qux
        {
        }

        public interface IBar
        {
        }

        private class Bar : IBar
        {
        }

        private abstract class AbstractBaz
        {
        }

        private class Baz : AbstractBaz
        {
        }

        [TestMethod]
        public void Should_CompareSameInstance_WhenUsingSingletonPolicy()
        {
            var container = new SimpleContainer();
            container.RegisterType<Foo>(true);

            Foo f1 = container.Resolve<Foo>();
            Foo f2 = container.Resolve<Foo>();

            Assert.AreEqual(f1, f2);
        }

        [TestMethod]
        public void Should_AllowRegisteringMoreThanOneType()
        {
            var container = new SimpleContainer();
            container.RegisterType<Foo>(false);
            container.RegisterType<Qux>(false);

            Assert.AreEqual(2, container.RegisteredTypes().Count);
        }

        [TestMethod]
        public void Should_ProperlyResolveMultipleTypes_WhenMoreThanOneTypeIsRegistered()
        {
            var container = new SimpleContainer();
            container.RegisterType<Foo>(false);
            container.RegisterType<Qux>(false);

            var f = container.Resolve<Foo>();
            var q = container.Resolve<Qux>();

            Assert.IsNotNull(f);
            Assert.IsNotNull(q);
        }

        [TestMethod]
        public void Should_CompareDifferentInstances_WhenUsingTransientPolicy()
        {
            var container = new SimpleContainer();
            container.RegisterType<Foo>(false);

            Foo f1 = container.Resolve<Foo>();
            Foo f2 = container.Resolve<Foo>();

            Assert.AreNotEqual(f1, f2);
        }

        [TestMethod]
        public void Should_ReturnChildType_WhenResolvingInterface()
        {
            var container = new SimpleContainer();
            container.RegisterType<IBar, Bar>(false);

            IBar bar = container.Resolve<IBar>();
            Assert.IsInstanceOfType(bar, typeof(Bar));
        }

        [TestMethod]
        public void Should_ReturnChildType_WhenResolvingAbstractClass()
        {
            var container = new SimpleContainer();
            container.RegisterType<AbstractBaz, Baz>(false);

            AbstractBaz bar = container.Resolve<AbstractBaz>();
            Assert.IsInstanceOfType(bar, typeof(Baz));
        }

        [TestMethod]
        public void Should_ThrowException_WhenResolvingUnregisteredType()
        {
            var container = new SimpleContainer();

            Assert.ThrowsException<ArgumentException>(() =>
            {
                container.Resolve<Foo>();
            });
        }
    }
}