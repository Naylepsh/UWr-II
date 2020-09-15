using InversionOfControlEngine;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Security.Cryptography.X509Certificates;

namespace IoCETest
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

        private class AnotherBar : IBar
        {
        }

        private abstract class AbstractBaz
        {
        }

        private class Baz : AbstractBaz
        {
        }

        private class Quux
        {
            public int X { get; set; }

            public Quux()
            {
                X = 0;
            }

            public Quux(int x)
            {
                X = x;
            }
        }

        private class DependancyA
        {
            private DependancyB _b;

            public DependancyA(DependancyB b)
            {
                _b = b;
            }
        }

        private class DependancyB
        {
            private DependancyA _a;

            public DependancyB(DependancyA a)
            {
                _a = a;
            }
        }

        private class ClassWithAttribute
        {
            public int X { get; set; }

            [DependencyConstructor]
            public ClassWithAttribute()
            {
                X = 0;
            }

            public ClassWithAttribute(int x)
            {
                X = x;
            }
        }

        private class ClassWithTwoAttributedConstructors
        {
            [DependencyConstructor]
            public ClassWithTwoAttributedConstructors()
            {
            }

            [DependencyConstructor]
            public ClassWithTwoAttributedConstructors(int x)
            {
            }
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

        [TestMethod]
        public void Should_AllowTransientReregistration()
        {
            var container = new SimpleContainer();
            container.RegisterType<IBar, Bar>(false);
            container.RegisterType<IBar, AnotherBar>(false);

            IBar bar = container.Resolve<IBar>();

            Assert.IsInstanceOfType(bar, typeof(AnotherBar));
        }

        [TestMethod]
        public void Should_AllowSingletonReregistration()
        {
            var container = new SimpleContainer();
            container.RegisterType<IBar, Bar>(true);
            container.RegisterType<IBar, AnotherBar>(true);

            IBar bar = container.Resolve<IBar>();

            Assert.IsInstanceOfType(bar, typeof(AnotherBar));
        }

        [TestMethod]
        public void Should_AllowMixedPolicyReregistration()
        {
            var container = new SimpleContainer();
            container.RegisterType<IBar, Bar>(false);
            container.RegisterType<IBar, AnotherBar>(true);

            IBar bar = container.Resolve<IBar>();

            Assert.IsInstanceOfType(bar, typeof(AnotherBar));
        }

        [TestMethod]
        public void Should_AllowRegistrationOfInstance()
        {
            Foo foo = new Foo();
            var container = new SimpleContainer();
            container.RegisterInstance<Foo>(foo);

            Foo acquired = container.Resolve<Foo>();

            Assert.AreEqual(foo, acquired);
        }

        [TestMethod]
        public void Should_DistinguishBetweenInstances_WhenComparingResolvedToNewInstance()
        {
            Foo foo = new Foo();
            var container = new SimpleContainer();
            container.RegisterInstance<Foo>(foo);

            Foo acquired = container.Resolve<Foo>();
            Foo otherInstance = new Foo();

            Assert.AreNotEqual(otherInstance, acquired);
        }

        [TestMethod]
        public void Should_ChangeRegisteredInstance_WhenRegisteringNewInstance()
        {
            Foo firstFoo = new Foo();
            var container = new SimpleContainer();
            container.RegisterInstance<Foo>(firstFoo);
            Foo secondFoo = new Foo();
            container.RegisterInstance<Foo>(secondFoo);

            Foo acquired = container.Resolve<Foo>();

            Assert.AreNotEqual(firstFoo, acquired);
            Assert.AreEqual(secondFoo, acquired);
        }

        [TestMethod]
        public void Should_OverrideSingletonPolicy_WhenRegisteringInstanceAfterPolicyWasRegistered()
        {
            var container = new SimpleContainer();
            container.RegisterType<Foo>(true);
            Foo firstFoo = container.Resolve<Foo>();
            Foo foo = new Foo();
            container.RegisterInstance<Foo>(foo);
            Foo secondFoo = container.Resolve<Foo>();

            Assert.AreEqual(foo, secondFoo);
            Assert.AreNotEqual(foo, firstFoo);
        }

        [TestMethod]
        public void Should_ResolveSimpleTypes()
        {
            var container = new SimpleContainer();
            int x = 42;
            container.RegisterInstance<int>(x);

            int result = container.Resolve<int>();

            Assert.AreEqual(x, result);
        }

        [TestMethod]
        public void Should_ThrowError_WhenResolvingTypeDependantOnUnregisteredType()
        {
            var container = new SimpleContainer();
            container.RegisterType<Quux>(false);

            Assert.ThrowsException<ResolveException>(container.Resolve<Quux>);
        }

        [TestMethod]
        public void Should_ResolveDependantType_WhenDependanciesAreRegistered()
        {
            var container = new SimpleContainer();
            int x = 42;
            container.RegisterInstance<int>(x);
            container.RegisterType<Quux>(false);

            Quux acquired = container.Resolve<Quux>();

            Assert.AreEqual(x, acquired.X);
        }

        [TestMethod]
        public void Should_ThrowException_WhenDependancyCycleIsFound()
        {
            var container = new SimpleContainer();
            container.RegisterType<DependancyA>(false);
            container.RegisterType<DependancyB>(false);

            Assert.ThrowsException<ResolveException>(container.Resolve<DependancyA>);
            Assert.ThrowsException<ResolveException>(container.Resolve<DependancyB>);
        }

        [TestMethod]
        public void Should_UseConstructorWithAttribute_IfOneIsDefined()
        {
            var container = new SimpleContainer();
            int x = 42;
            container.RegisterInstance<int>(x);
            container.RegisterType<ClassWithAttribute>(false);

            var acquired = container.Resolve<ClassWithAttribute>();

            Assert.AreEqual(0, acquired.X);
        }

        [TestMethod]
        public void Should_ThrowError_WhenMoreThanOneAttributedConstructorIsFound()
        {
            var container = new SimpleContainer();
            container.RegisterType<ClassWithTwoAttributedConstructors>(false);

            Assert.ThrowsException<ResolveException>(container.Resolve<ClassWithTwoAttributedConstructors>);
        }
    }
}