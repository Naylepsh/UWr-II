using System;

namespace Zad_4
{
    public class Vector
    {
        private int _n;
        private float[] _components;

        public Vector(int n, float[] components)
        {
            _n = n;
            _components = new float[n];
            for (int i = 0; i < n; i++)
                _components[i] = components[i];
        }

        public static Vector operator +(Vector vect1, Vector vect2)
        {
            if (vect1._n != vect2._n)
                throw new Exception("Vectors have different amount of components.");
            
            float[] components = new float[vect1._n];
            for (int i = 0; i < vect1._n; i++)
                components[i] = vect1._components[i] + vect2._components[i];

            return new Vector(vect1._n, components);
        }

        public static Vector operator *(Vector vect1, Vector vect2)
        {
            if (vect1._n != vect2._n)
                throw new Exception("Vectors have different amount of components.");

            float[] components = new float[vect1._n];
            for (int i = 0; i < vect1._n; i++)
                components[i] = vect1._components[i] * vect2._components[i];
            
            return new Vector(vect1._n, components);
        }

        public static Vector operator *(Vector vect, float alfa)
        {
            float[] components = new float[vect._n];
            for (int i = 0; i < vect._n; i++)
                components[i] = vect._components[i] * alfa;
            
            return new Vector(vect._n, components);
        }

        public override string ToString()
        {
            string s = "";
            if (_n > 0)
                s += "[" + _components[0];
            for (int i = 1; i < _n; i++)
                s += ", " + _components[i];
            return s + "]";
        }

        public int GetN()
        {
            return _n;
        }

        public float[] GetComponents()
        {
            return _components;
        }
    }
}