using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

namespace InversionOfControlEngine
{
    public class ResolveException : Exception
    {
        public ResolveException(string message) : base(message)
        {
        }
    }

    public abstract class LifecyclePolicy
    {
        protected readonly Type _type;

        public LifecyclePolicy(Type type)
        {
            _type = type;
        }

        private ConstructorInfo GetConstructor()
        {
            var constructors = _type
                .GetConstructors()
                .OrderByDescending(constr => constr.GetParameters().Length)
                ;

            var constructor = constructors.First();
            return constructor;
        }

        private List<object> ResolveParameters(
            ConstructorInfo constructor,
            Dictionary<Type, LifecyclePolicy> policies,
            Dictionary<Type, bool> typesNotResolvedYet)
        {
            var resolvedParameters = new List<object>();
            foreach (var parameter in constructor.GetParameters())
            {
                Type parameterType = parameter.ParameterType;
                if (typesNotResolvedYet.ContainsKey(parameterType))
                {
                    throw new ResolveException("Dependancy cycle found");
                }

                if (!policies.ContainsKey(parameterType))
                {
                    throw new ResolveException($"{parameterType} not registered");
                }

                object resolved = policies[parameterType].Resolve(policies, typesNotResolvedYet);
                resolvedParameters.Add(resolved);
            }

            return resolvedParameters;
        }

        public virtual object Resolve(
            Dictionary<Type, LifecyclePolicy> policies,
            Dictionary<Type, bool> typesNotResolvedYet = null)
        {
            if (typesNotResolvedYet == null)
            {
                typesNotResolvedYet = new Dictionary<Type, bool>();
            }
            typesNotResolvedYet.Add(_type, true);

            var constructor = GetConstructor();
            var resolvedParameters = ResolveParameters(constructor, policies, typesNotResolvedYet);

            typesNotResolvedYet.Remove(_type);

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

        public override object Resolve(
            Dictionary<Type, LifecyclePolicy> policies,
            Dictionary<Type, bool> typesNotResolvedYet)
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

        public override object Resolve(
            Dictionary<Type, LifecyclePolicy> policies,
            Dictionary<Type, bool> typesNotResolvedYet = null)
        {
            return base.Resolve(policies, typesNotResolvedYet);
        }
    }
}