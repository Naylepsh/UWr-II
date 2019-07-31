using System;
using System.Runtime.InteropServices;

namespace Zad_4
{
    public class Matrix
    {
        private int _n;
        private Vector[] _vectors;

        public Matrix(int n, Vector[] vectors)
        {
            // checking whether all vectors have the same number of components
            for (int i = 0; i < n - 1; i++)
            {
                if (vectors[i].GetN() != vectors[i+1].GetN())
                    throw new Exception("Vectors have different amount of components.");
            }
            
            _n = n;
            _vectors = new Vector[_n];
            for (int i = 0; i < _n; i++)
                _vectors[i] = new Vector(vectors[i].GetN(), vectors[i].GetComponents());
        }
        
        public static Matrix operator +(Matrix m1, Matrix m2)
        {
            // checking whether matrixes have the same dimensions
            if (m1._n != m2._n || 
                m1._vectors[0].GetN() != m2._vectors[0].GetN())
                throw new Exception("Matrixes have different dimensions.");
            
            
            Vector[] vectors = new Vector[m1._n];
            for (int i = 0; i < m1._n; i++)
                vectors[i] = m1._vectors[i] + m2._vectors[i];

            return new Matrix(m1._n, vectors);
        }

        public static Matrix operator *(Matrix A, Matrix B)
        {
            /*
             * using coefficients-vectors method (from wikipedia)
             * each row in a new matrix is 
             * a sum of (all vectors from the second matrix times
             * their corresponding scalar from that row in first matrix
             * EXAMPLE
             * A = | 1 0 2|    B = |3 1|
             *     |-1 3 1|        |2 1|
             *                     |1 0|
             * then AB = | 1*[3 1] + 0*[2 1] + 2*[1 0]| = |5 1|
             *           |-1*[3 1] + 3*[2 1] + 1*[1 0]|   |4 2|
             */
            
            // checking whether matrixes have valid size
            // that is if (rows in A) == (columns in B)
            if (A._n != B._vectors[0].GetN())
                throw new Exception("Invalid size of matrixes.");
            
            
            Vector[] vectors = new Vector[A._n];
            for (int i = 0; i < A._n; i++)
            {
                vectors[i] = B._vectors[0] * A._vectors[i].GetComponents()[0];
                for (int j = 1; j < A._vectors[0].GetN(); j++)
                {
                    vectors[i] = vectors[i] + (B._vectors[j]
                                               * A._vectors[i].GetComponents()[j]);
                }
            }
            
            return new Matrix(A._n, vectors);
        }
        
        public static Matrix operator *(Matrix A, Vector v)
        {
            if (A._n != v.GetN())
                throw new Exception("Invalid size of matrix / vector.");

            Vector[] vectors = new Vector[A._n];
            for (int i = 0; i < A._n; i++)
            {
                vectors[i] = A._vectors[i] * v.GetComponents()[i];
            }
            
            return new Matrix(A._n, vectors);
        }

        public override String ToString()
        {
            string s = "";
            for (int i = 0; i < _n; i++)
                s += _vectors[i] + "\n";
            return s;
        }
    }
}