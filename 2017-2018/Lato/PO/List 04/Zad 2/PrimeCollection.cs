using System.Collections;

namespace Zad_2
{
    public class PrimeEnumerator : IEnumerator
    {
        private int n;

        private bool IsPrime()
        {
            if (n < 2) return false;
	        if (n == 2) return true;
            for (var i = 2; i * i <= n; i++)
                if (n % i == 0) return false;
            return true;
        }

        public bool MoveNext()
        {
            ++n;
            // while n is not prime and it hasn't hit the limit val -> search for next prime
            while (!IsPrime() && n < int.MaxValue) n++;
            return (n < int.MaxValue);
        }

        public object Current => n;

        public void Reset()
        {
            n = 0;
        }
    }

    public class PrimeCollection : IEnumerable
    {
        public IEnumerator GetEnumerator()
        {
            return new PrimeEnumerator();
        }
    }
}