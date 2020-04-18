using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex_2
{
    public abstract class AbstractExpression {
        public abstract bool Interpret(Context context);
    }
}
