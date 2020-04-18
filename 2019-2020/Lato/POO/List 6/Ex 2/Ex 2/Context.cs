using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex_2
{
    public class Context
    {
        private Dictionary<string, bool> _variables = new Dictionary<string, bool>();

        public bool GetValue(string VariableName) {
            if (_variables.ContainsKey(VariableName))
            {
                return _variables[VariableName];
            }
            else
            {
                throw new ArgumentException("No such variable name in context");
            }
        }

        public void SetValue(string VariableName, bool Value) {
            if (_variables.ContainsKey(VariableName))
            {
                _variables.Remove(VariableName);
            }
            _variables.Add(VariableName, Value);
        }
    }
}
