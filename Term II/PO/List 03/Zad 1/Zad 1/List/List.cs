using System;

namespace Zad_1
{
    public class Element<T>
    {
        // 'pointers' to previous and next elements
        public Element<T> previous;
        public Element<T> next;
        public T val;

        public Element(T val)
        {
            previous = null;
            next = null;
            this.val = val;
        }
    }

    public class List<T>
    {
        private Element<T> begin;
        private Element<T> end;
        
        public bool IsEmpty()
        {
            return (begin == null && end == null);
        }

        public void AddAtBegin(T val)
        {
            var temp = new Element<T>(val);
            // if list is empty point both 'pointers' to the same element
            if (IsEmpty())
            {
                begin = temp;
                end = temp;
            }
            // else link new element and begin-pointer and let begin point at new element
            else
            {
                temp.next = begin;
                begin.previous = temp;
                begin = temp;
            }
        }

        public T RemoveFromBegin()
        {
            if (IsEmpty())
                throw new Exception("List is empty.");

            var temp = begin;
            // if there's only one item remaining
            // turn both 'pointers' to null
            if (begin == end) 
                begin = end = null;
            else
            {
                begin = begin.next;
                begin.previous = null;
            }

            return temp.val;
        }

        public void AddAtEnd(T val)
        {
            var temp = new Element<T>(val);
            // if list is empty point both 'pointers' to the same element
            if (IsEmpty())
            {
                begin = temp;
                end = temp;
            }
            // else link new element and end-pointer and let end point at new element
            else
            {
                temp.previous = end;
                end.next = temp;
                end = temp;
            }
        }

        public T RemoveFromEnd()
        {
            if (IsEmpty())
                throw new Exception("List is empty.");

            var temp = end;
            // if there's only one item remaining
            // turn both 'pointers' to null
            if (begin == end) 
                begin = end = null;
            else
            {
                end = end.previous;
                end.next = null;
            }
            return temp.val;
        }

        public override string ToString()
        {
            var str = "[";
            var temp = begin;
            while (temp != null)
            {
                str += " " + temp.val;
                temp = temp.next;
            }

            return str + " ]";
        }
        
        /*
        public string PrintList()
        {
            var str = "[";
            var temp = end;
            while (temp != null)
            {
                str += " " + temp.val;
                temp = temp.previous;
            }

            return str + " ]";
        }*/
    }
}
