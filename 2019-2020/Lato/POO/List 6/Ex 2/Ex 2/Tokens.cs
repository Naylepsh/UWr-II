using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex_2
{
    public class ConstExpression : AbstractExpression
    {
        private readonly bool _value;

        public ConstExpression(bool value)
        {
            _value = value;
        }

        public override bool Interpret(Context context)
        {
            return _value;
        }
    }

    public class VarExpression : AbstractExpression
    {
        private readonly string _name;

        public VarExpression(string name)
        {
            _name = name;
        }

        public override bool Interpret(Context context)
        {
            return context.GetValue(_name);
        }
    }
}
