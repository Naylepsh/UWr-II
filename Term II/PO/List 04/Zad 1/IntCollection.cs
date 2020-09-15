namespace Zad_1
{
    public class IntCollection : IStream
    {
        private int _val;

        public int Next()
        {
            // set to-return value to current value
            var old = _val;
            // increment current value if possible
            if (!(Eos()))
            {
                _val++;
            }
            return old;
        }

        public bool Eos()
        {
            return _val == int.MaxValue;
        }

        public void Reset()
        {
            _val = 0;
        }
    }
}