using System;
using System.Linq;
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

        public virtual object Resolve(Dictionary<Type, LifecyclePolicy> policies)
        {
            var constructors = _type
                .GetConstructors()
                .OrderByDescending(constr => constr.GetParameters().Length)
                ;

            var constructor = constructors.First();

            var resolvedParameters = new List<object>();
            foreach (var parameter in constructor.GetParameters())
            {
                try
                {
                    Type parameterType = parameter.ParameterType;
                    object resolved = policies[parameterType].Resolve(policies);
                    resolvedParameters.Add(resolved);
                }
                catch (Exception e)
                {
                    throw new InvalidOperationException("Could not resolve at least one of the constructors");
                }
            }

            var instance = constructor.Invoke(resolvedParameters.ToArray());
            return instance;
        }
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

        public override object Resolve(Dictionary<Type, LifecyclePolicy> policies)
        {
            if (!_instances.ContainsKey(_type))
            {
                var instance = base.Resolve(policies);
                _instances[_type] = instance;
            }

            return _instances[_type];
        }
    }

    public class TransientLifecyclePolicy : LifecyclePolicy
    {
        public TransientLifecyclePolicy(Type type) : base(type)
        {
        }

        public override object Resolve(Dictionary<Type, LifecyclePolicy> policies)
        {
            return base.Resolve(policies);
        }
    }
}