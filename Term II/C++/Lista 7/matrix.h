#pragma once
#include <iostream>
#include "eval_exceptions.h"

namespace eval {
	class Matrix
	{
	protected:
		int rows;
		int cols;
		double ** matrix;
	public:
		Matrix(const int rows);
		Matrix(const int rows, const int cols);
		Matrix(Matrix & matrix);
		Matrix(Matrix && matrix);
		~Matrix();

		Matrix & operator=(const Matrix & matrix);
		Matrix & operator=(Matrix && matrix);

		// arithmetic operators
		Matrix & operator*=(double scalar);
		Matrix operator*(double scalar);
		Matrix & operator+=(const Matrix & matrix);
		Matrix operator+(const Matrix & matrix);
		Matrix & operator-=(const Matrix & matrix);
		Matrix operator-(const Matrix & matrix);
		Matrix operator*(const Matrix & matrix);

		// other operations available on matrices
		Matrix Transpose();
		Matrix & SwapRows(int r1, int r2);
		Matrix & SwapCols(int c1, int c2);
		Matrix & MultRow(int row, double scalar);
		Matrix & MultCol(int col, double scalar);
		Matrix & AddMultRow(int from, int to, double scalar = 1);
		Matrix & AddMultCol(int from, int to, double scalar = 1);
		Matrix RemoveRow(int row);
		Matrix RemoveCol(int col);
		double Determinant();
		Matrix Reverse();

		// stream
		friend std::istream & operator>> (std::istream &is, Matrix &matrix);
		friend std::ostream & operator<< (std::ostream &os, Matrix &matrix);
	private:
		void InitMatrix();
		void DeleteMatrix();
		bool IsWithinRange(int x, int min, int max);
		void IndexCheck(int i, int min, int max);
		void ScalarCheck(double scalar);
	};
}
