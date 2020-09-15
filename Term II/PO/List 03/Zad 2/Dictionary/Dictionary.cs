using System;

namespace Dictionary
{
    public class Dictionary<TK, TV> where TK : IComparable<TK>
    {
        private Dictionary<TK, TV> _next;
        private TK _key;
        private TV _value;

        public Dictionary()
        {
            _next = null;
            _key = default(TK);
            _value = default(TV);
        }

        public void Add(TK key, TV value)
        {
            // if the whole dictionary has been searched and
            // the key hasn't been found, then 'place' new item at the very end
            if (_next == null)
            {
                _next = new Dictionary<TK, TV>();
                _key = key;
                _value = value;
            }
            // if the key exists in dictionary, then abort the Add function
            else if (_key.CompareTo(key) == 0)
                return;
            // if we're still in the middle of browing through the dictionary
            // then just keep on browsing
            else
                _next.Add(key, value);
        }

        public void Remove(TK key)
        {
            // if the very first item has the desired key
            if (_key.CompareTo(key) == 0)
            {
                _key = _next._key;
                _value = _next._value;
                _next = _next._next;
            }
            else if (_next._next != null)
            {
                _next.Remove(key);
            }
        }

        public TV Find(TK key)
        {
            if (_key.CompareTo(key) == 0)
                return _value;
            if (_next._next != null)
                return _next.Find(key);
            // else if key hasn't been found
            throw new ArgumentException("No key found.");
        }

        public override string ToString()
        {
            // if dict is totally empty, then return empty string
            if (_next == null)
                return "";
            // else make a string out of key, value and {} and move to the next object in dict
            return "{" + _key + ":" + _value + "} " + _next.ToString();
        }
    }
}