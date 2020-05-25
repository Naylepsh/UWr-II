using System;
using System.Collections.Generic;

namespace InversionOfControlEngine
{
    public class SimpleContainer
    {
        private readonly Dictionary<Type, LifecyclePolicy> _registeredTypes = new Dictionary<Type, LifecyclePolicy>();

        private LifecyclePolicy GetPolicy<T>(bool Singleton)
        {
            LifecyclePolicy policy;

            if (Singleton)
            {
                policy = new SingletonLifecyclePolicy(typeof(T));
            }
            else
            {
                policy = new TransientLifecyclePolicy(typeof(T));
            }

            return policy;
        }

        public void RegisterType<T>(bool Singleton) where T : class
        {
            var policy = GetPolicy<T>(Singleton);
            _registeredTypes[typeof(T)] = policy;
        }

        public void RegisterType<From, To>(bool Singleton) where To : From
        {
            var policy = GetPolicy<To>(Singleton);
            _registeredTypes[typeof(From)] =  policy;
        }

        public T Resolve<T>()
        {
            if (!_registeredTypes.ContainsKey(typeof(T)))
            {
                throw new ArgumentException(string.Format("Type {0} not registered", typeof(T)));
            }

            return (T)_registeredTypes[typeof(T)].Resolve();
        }

        public ICollection<Type> RegisteredTypes()
        {
            return _registeredTypes.Keys;
        }
    }
}