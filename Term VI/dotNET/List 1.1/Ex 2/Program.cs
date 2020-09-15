using System;

namespace ex2
{
    class Program
    {
        static void Main(string[] args)
        {
            Grid grid = new Grid(4,4);
            int[] rowdata = grid[1];
            Console.WriteLine(rowdata);
            int val = grid[1,1];
            Console.WriteLine(val);
            grid[1,1] = 5;
            Console.WriteLine(grid[1,1]);
        }
    }

    class Grid {
        private int[][] grid;

        public Grid(int rows, int cols) {
            this.grid = new int[rows][];
            for (var i = 0; i < rows; i++) {
                this.grid[i] = new int[cols];
            }
        }

        public int[] this[int i] {
            get { return this.grid[i]; }
        }

        public int this[int i, int j] {
            get { return this.grid[i][j]; }
            set { this.grid[i][j] = value; }
        }
    }
}
