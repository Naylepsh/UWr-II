using System;
using System.Collections;

namespace Ex_2
{
    public class Set : IEnumerable
    {
        private Hashtable _set = new Hashtable();

        public int Count
        {
            get
            {
                return _set.Count;
            }
        }

        public ICollection Values
        {
            get
            {
                return _set.Values;
            }
        }

        public void Add(object o)
        {
            if (_set[o] == null)
            {
                _set.Add(o, o);
            }
        }

        public void Remove(object o)
        {
            if (_set.ContainsKey(o))
            {
                _set.Remove(o);
            }
            else
            {
                throw new ArgumentException(string.Format("{0} not in set", o));
            }
        }

        public IEnumerator GetEnumerator()
        {
            return _set.Values.GetEnumerator();
        }

    }
}
