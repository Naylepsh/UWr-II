#include <iostream>
#include "tabbit.h"
#include <cstdint>


int main() {
	using namespace std;

	tab_bit t(6);
	cout << "t(6) = " << t << endl;
	tab_bit u(45ull);
	cout << "u(45ull) = " << u << endl;
	tab_bit v(t);
	cout << "v(t) = " << v << endl;

	//tab_bit w(tab_bit(8) { 1, 0, 1, 1, 0, 0, 0, 1 }); // not yet implemented -- it can't work, right?

	cout << "Setting 0th, 1st and 2nd bits of v to 1\n";
	v[0] = 1;
	v[1] = v[0];
	v[2] = true;
	cout << "v = " << v << endl;

	// indeksowanie ponad 64 bity dalej dziala dobrze
	tab_bit x(67);
	x[65] = 1;
	x[36] = 1;
	cout << "x = " << x << endl;
	cout << x[36] << endl;

	tab_bit z(t);
	z[1] = 1;
	z[4] = 1;
	cout << "z = " << z << endl;
	cout << "v | z = " << (v | z) << endl;
	cout << "v & z = " << (v & z) << endl;
	cout << "v ^ z = " << (v ^ z) << endl;

	tab_bit y(z);
	cout << "y = " << y << endl;
	y |= v;
	cout << "(y |= v) =" << y << endl;
	y &= v;
	cout << "(y &= v) =" << y << endl;
	y ^= v;
	cout << "(y ^= v) =" << y << endl;
	cout << "!y = " << !y << endl;

	tab_bit b({ 0,1,1,0,1,1,0 });
	cout << "Teraz tab_bit b({0,1,1,0,1,1,0})\n";
	cout << "b = " << b << endl;

	tab_bit a(6);
	a = b;
	cout << "Niech a = b\n";
	cout << "a = " << a << endl;

	bool c = a[3];
	cout << "c = a[3] = " << c << endl;
	bool d = a[2];
	cout << "d = a[2] = " << d << endl;

	cout << "Poprawiamy a:\n";
	cin >> a;
	cout << a << endl;

	cin.get();
	cin.get();
	return 0;
}