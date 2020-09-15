using System;

namespace Zad_4
{
    class Program
    {
        public static void Main()
        {
            float[] comp1 = {1, 2, 3};
            var vect1 = new Vector(3, comp1);

            float[] comp2 = {4, 5, 6};
            var vect2 = new Vector(3, comp2);

            var vect3 = vect1 + vect2;
            //Console.WriteLine("Vect3 = " + vect3);
            var vect4 = vect1 * vect2;
            //Console.WriteLine("Vect4 = " + vect4);
            var vect5 = vect1 * 3;
            //Console.WriteLine("Vect5 = " + vect5);

            Console.WriteLine();
            
            Console.WriteLine("Let m1 be a matrix of vectors:\n" + vect1 + vect2);
            Vector[] vects = {vect1, vect2};
            var m1 = new Matrix(2,vects);
            Console.WriteLine("m1:\n" + m1);
            Console.WriteLine("Let m2 be a matrix of vectors:\n" + vect3 + vect4);
            Vector[] vects2 = {vect3, vect4};
            var m2 = new Matrix(2, vects2);
            Console.WriteLine("m2:\n" + m2);
            
            Console.WriteLine("Then let m3 = m1 + m2\nNow m3:");
            var m3 = m1 + m2;
            Console.WriteLine(m3);
            
            
            // lets make a matrix of size 2 x 3
            float[] c1 = {1, 0, 2};
            float[] c2 = {-1, 3, 1};
            var v1 = new Vector(3, c1);
            var v2 = new Vector(3, c2);
            Vector[] v = {v1, v2};
            var m4 = new Matrix(2,v);
            Console.WriteLine("Let m4:\n" + m4);
            
            // lets make a matrix of size 3 x 2
            float[] c3 = {3, 1};
            float[] c4 = {2, 1};
            float[] c5 = {1, 0};
            v1 = new Vector(2, c3);
            v2 = new Vector(2, c4);
            var v3 = new Vector(2, c5);
            Vector[] vv = {v1, v2, v3};
            var m5 = new Matrix(3,vv);
            Console.WriteLine("Let m5:\n" + m5);
            
            Console.WriteLine("Now, let m6 = m4 * m5");
            var m6 = m4 * m5;
            Console.WriteLine("m6:\n" + m6);
            


            float[] c7 = {2, -2};
            var vvv = new Vector(2, c7);
            var m7 = m6 * vvv;
            Console.WriteLine("Let m7 = m6 * " + vvv);
            Console.WriteLine("Now, m7:\n" + m7);
        }
    }
}