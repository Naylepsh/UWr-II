using System;


namespace Streams
{
    class IntStream
    {
        protected int val;

        public IntStream(int val = 0)
        {
            this.val = val;
        }

        public virtual int Next()
        {
            // set to-return value to current value
            int old = val;
            // increment current value if possible
            if (!(Eos()))
            {
                val++;
            }
            return old;
        }

        public virtual bool Eos()
        {
            return val == int.MaxValue;
        }

        public void Reset()
        {
            val = 0;
        }
    }

    class PrimeStream : IntStream
    {
        public override int Next()
        {
            // if 'this' is freshly made or if it has been reset
            // set the value to 2
            if (val < 2)
                val = 2;
            int old = val;

            if (val == 2)
                val++;
            else if (!Eos())
            {
                // this loop is achieveable only for val >= 3
                // thus, we can safely skip all even numbers
                // and increment by 2 until we find next prime number
                val += 2;
                while (!Eos() && !IsPrime())
                {
                    val += 2;
                }
                // if we hit the max value, set val back to the state before Next() has been called
                if (Eos())
                    val = old;
            }
            return old;
        }

        private bool IsPrime()
        {
            for (int i = 3; i*i < val + 1; i += 2)
            {
                if (val % i == 0)
                    return false;
            }
            return true;
        }

    }

    class RandomStream : IntStream
    {
        private Random rnd = new Random();
        private int max;

        public RandomStream(int max = int.MaxValue)
        {
            this.max = max;
        }

        public override int Next()
        {
            int old = val;
            val = rnd.Next(0, max);
            return old;
        }

        public override bool Eos()
        {
            return false;
        }
    }

    class RandomWordStream : PrimeStream
    {
        string s;
        RandomStream rs;

        public RandomWordStream()
        {
            rs = new RandomStream(26);
            // pre-set s to a random string
            Next();
        }

        public new string Next()
        {
            string old_s = s;
            int len = base.Next();
            string temp = "";
            for (int i = 0; i < len; i++)
            {
                // decide whether to use upper or lower letter
                int option = rs.Next() % 2;
                // uppercase letter
                if (option == 0)
                    temp += (char)(rs.Next() + 'a');
                // lowercase letter
                else
                    temp += (char)(rs.Next() + 'A');
            }
            s = temp;
            return old_s;
        }
    }
}
