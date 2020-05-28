using System;
using System.Collections.Generic;

namespace InversionOfControlEngine
{
    public abstract class LifecyclePolicy
    {
        protected readonly Type _type;

        public LifecyclePolicy(Type type)
        {
            _type = type;
        }

        public abstract object Resolve();
    }

    public class SingletonLifecyclePolicy : LifecyclePolicy
    {
        private static readonly Dictionary<Type, object> _instances = new Dictionary<Type, object>();

        public SingletonLifecyclePolicy(Type type) : base(type)
        {
        }

        public SingletonLifecyclePolicy(Type type, object instance) : base(type)
        {
            _instances[type] = instance;
        }

        public override object Resolve()
        {
            if (!_instances.ContainsKey(_type))
            {
                _instances[_type] = Activator.CreateInstance(_type);
            }

            return _instances[_type];
        }
    }

    public class TransientLifecyclePolicy : LifecyclePolicy
    {
        public TransientLifecyclePolicy(Type type) : base(type)
        {
        }

        public override object Resolve()
        {
            return Activator.CreateInstance(_type);
        }
    }
}