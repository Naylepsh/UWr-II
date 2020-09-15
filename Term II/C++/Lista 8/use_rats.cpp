#include "Rational.h"
#include <iostream>
#include <string>

int main() {
	using namespace std;

	Rational r1 = Rational(7);
	cout << "r1 = " << r1 << endl;

	Rational r2 = Rational(1, 2);
	cout << "r2 = " << r2 << endl;

	Rational r3 = Rational(1, 3);
	cout << "r3 = " << r3 << endl;

	Rational r4 = r2 + r3;
	cout << "r4 = " << r2 << "+" << r3 << " = " << r4 << endl;

	Rational r5 = r2 - r3;
	cout << "r5 = " << r2 << "-" << r3 << " = " << r5 << endl;

	Rational r6 = r2 * r3;
	cout << "r6 = " << r2 << "*" << r3 << " = " << r6 << endl;

	Rational r7 = r2 / r3;
	cout << "r7 = " << r2 << "/" << r3 << " = " << r7 << endl;

	Rational r8 = -r2;
	cout << "r8 = " << "-" << r2 << " = " << r8 << endl;

	double x = (double)r8;
	cout << "double x = r8 = " << x << endl;

	int y = (int)r8;
	cout << "int x = r8 = " << y << endl;

	try {
		Rational r9 = Rational(INT_MAX);
		Rational r10 = r9 + r9;
		cout << "r10 = r9 + r9 = INT_MAX + INT_MAX = " << r10 << endl;
	}
	catch (RationalException re) {
		cout << re.what() << endl;
	}

	try {
		Rational r9 = Rational(INT_MAX);
		Rational r10 = r9 * r9;
		cout << "r10 = r9 * r9 = INT_MAX * INT_MAX = " << r10 << endl;
	}
	catch (RationalException re) {
		cout << re.what() << endl;
	}

	try {
		Rational r9 = Rational(1, 0);
		cout << "r9 = 1/0 = " << r9 << endl;
	}
	catch (RationalException re) {
		cout << re.what() << endl;
	}

	Rational r9 = Rational(2, 7);
	cout << r9;
	Rational r10 = Rational(2, -7);
	Rational r11 = Rational(-2, 7);
	Rational r12 = Rational(-2, -7);
	cout << endl << r10 << endl << r11 << endl << r12 << endl;
	Rational r13 = !r10;
	cout << r13 << endl;
	Rational r14 = Rational(49, 7);
	cout << r14 << endl;
	
	cin.get();
	return 0;
}