#include <iostream>
#include "matrix.h"
#include <exception>

int main() {
	using namespace eval;
	using namespace std;
	
	Matrix A = Matrix(3);
	cout << "Diagonal matrix A:\n" << A << endl;

	Matrix B = Matrix(2, 3);
	cout << "Matrix B filled with 0s:\n" << B << endl;

	Matrix C = A + A + A;
	cout << "Matrix C = A + A + A:\n" << C << endl;

	Matrix D = A * 3;
	cout << "Matrix D = A * 3:\n" << D << endl;

	Matrix E = C * D;
	cout << "Matrix E = C * D:\n" << E << endl;

	E.SwapRows(0, 2);
	cout << "Matrix E after swapping rows 0 and 2:\n" << E << endl;

	E.SwapCols(0, 2);
	cout << "Matrix E after swapping cols 0 and 2:\n" << E << endl;

	E.MultRow(0, 1.0 / 3);
	cout << "Matrix E after multiplying 0th row by 1/3:\n" << E << endl;

	E.MultCol(2, 3);
	cout << "Matrix E after multiplying 2nd col by 3:\n" << E << endl;

	E.AddMultCol(0, 0);
	E.AddMultRow(0, 1, 3);
	E.AddMultRow(1, 2, 2);
	cout << "Matrix E after some adding and multiplying:\n" << E << endl;

	Matrix F = E.RemoveRow(0);
	cout << "Matrix F being matrix E with 0th row removed:\n" << F << endl;

	Matrix J = F.Transpose();
	cout << "Matrix F transposed:\n" << J << endl;

	Matrix G = F.RemoveCol(0);
	cout << "Matrix G being matrix F with 0th col removed:\n" << G << endl;
	
	Matrix I = G;
	cout << "Matrix I = G:\n" << I << endl;
	I = I + I;
	cout << "I+I:\n" << I << endl;

	try {
		Matrix H = Matrix(3, 3);
		// 1 2 3 1 3 5 1 2 4
		cout << "Enter 9 numbers: ";
		cin >> H;
		cin.get();
		cout << "Matrix H(3x3) read from cin:\n" << H << endl;
		cout << "det(H) = " << H.Determinant() << endl;
		Matrix rev = H.Reverse();
		cout << "H^(-1):\n" << rev << endl;
	}
	catch (EvalException * e) {
		std::cout << e->what() << std::endl;
	}

	cin.get();
	return 0;
}