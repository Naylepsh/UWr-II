using System;
using System.Collections.Generic;
using System.Linq;


namespace Listy
{
    class ListaLeniwa
    {
        protected List<int> element;
        private Random rnd;

        public ListaLeniwa()
        {
            rnd = new Random();
            element = new List<int>();
        }

        public int Size()
        {
            return element.Count();
        }

        public virtual int Element(int i)
        {
            // jesli zostal podany indeks niewiekszy niz 0 -> wyrzuc wyjatek
            if (i <= 0)
                throw new ArgumentOutOfRangeException("i");
            // jesli i jest powyzej obecnego zasiegu listy
            // powieksz liste i wypelnij ja losowymi liczbami
            if (i > Size())
            {
                for (int j = Size(); j < i; j++)
                    element.Add(rnd.Next());
            }
            return element[i - 1];
        }
    }

    class Pierwsze : ListaLeniwa
    {
        public override int Element(int i)
        {
            if (i <= 0)
                throw new ArgumentOutOfRangeException("i");
            if (i > Size())
            {
                for (int j = Size(); j < i; j++)
                {
                    if (Size() == 0)
                        element.Add(2);
                    else if (Size() == 1)
                        element.Add(3);
                    else
                    {
                        // wezmy ostatnia zapiana liczbe pierwsza
                        // i zwiekszmy ja o 2
                        int n = element[element.Count() - 1] + 2;
                        while (!CzyPierwsza(n))
                            n += 2;
                        // nalezy jeszcze sprawdzac czy nie wyszlismy poza gorny limit inta
                        element.Add(n);
                    }
                }
            }
            return element[i - 1];
        }

        private static bool CzyPierwsza(int n)
        {
            if (n % 2 == 0)
                return false;
            for (int i = 3; i < Math.Sqrt(n) + 1; i += 2)
            {
                if (n % i == 0)
                    return false;
            }
            return true;
        }

        public override string ToString()
        {
            string s = "[";
            for (int i = 0; i < Size(); i++)
                s += element[i] + " ";
            return s + "]";
        }
    }
}
