using System.Collections; // needed for IEnumerator

namespace Zad_1
{
    public class PrimeNumberator : IEnumerator
    {
        private int _n;

        public bool MoveNext()
        {
            ++_n;
            // while n is not prime and it hasn't hit the limit val -> search for next prime
            while (!HelperFunctions.IsPrime(_n) && _n < int.MaxValue) _n++;
            return (_n < int.MaxValue);
        }

        public object Current => _n;

        public void Reset()
        {
            _n = 0;
        }
    }

    public class PrimeCollection : IEnumerable, IStream
    {
        // supposedly you shouldn't init fields by its default value
        private int _val;
        private int _length;
        
        // Enumerator for IEnumerable
        public IEnumerator GetEnumerator()
        {
            return new PrimeNumberator();
        }
        
        // functions 'imported' from IStream
        public bool Eos()
        {
            return _val == int.MaxValue;
        }

        public int Next()
        {
            // if 'this' is freshly made or if it has been reset
            // set the value to 2
            if (_val < 2)
            {
                _val = 2;
                _length = 1;
            }

            int old = _val;

            if (_val == 2)
            {
                _val++;
                _length++;
            }
            else if (!Eos())
            {
                // this loop is achieveable only for val >= 3
                // thus, we can safely skip all even numbers
                // and increment by 2 until we find next prime number
                _val += 2;
                while (!Eos() && !HelperFunctions.IsPrime(_val))
                {
                    _val += 2;
                }
                // if we hit the max value, set val back to the state before Next() has been called
                if (Eos())
                    _val = old;
                else
                    _length++;
            }
            return old;
        }

        public void Reset()
        {
            _val = 0;
        }
        
        // ToString method
        public override string ToString()
        {
            return "Current prime: " + _val;
        }

        // indexed access
        public int this[int index]
        {
            get
            {
                var pc = new PrimeCollection();
                for (var i = 0; i < index; i++)
                    pc.Next();

                return pc.Next();
            }
        }
        
        // property Length
        public int Length => _length;
    }
}