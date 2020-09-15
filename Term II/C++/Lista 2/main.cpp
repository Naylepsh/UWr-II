#include <iostream>
#include "Wektor.h"
#include "Punkt.h"
#include "Prosta.h"

int main() {
	using std::cout;
	using std::endl;

	// Testowanie wektorow
	Wektor w1 = Wektor();				// konstruktor domyslny
	Wektor w2 = Wektor(1, 0);			// konstruktor (double, double)
	Wektor w3 = Wektor(Wektor(1, 1));	// konstruktor (Wektor)

	cout << "Wektor() = " << w1.toString() << endl;
	cout << "Wektor(1,0) = " << w2.toString() << endl;
	cout << "Wektor(1,1) = " << w3.toString() << endl;

	Wektor w4 = zloz(w2, w3);

	cout << "Zlozenie " <<  w2.toString() << " i " << w3.toString() << " = " << w4.toString() << endl;
	cout << endl;


	// Testowanie punktow
	Punkt p1 = Punkt();					// konstruktor domyslny
	Punkt p2 = Punkt(5, 6);				// konstruktor (double, double)
	Punkt p3 = Punkt(p2);				// konstruktor (Punk)
	Punkt p4 = Punkt(p2, Wektor(-1,1));	// konstruktor (Punkt, Wektor)

	cout << "Punkt() = " << p1.toString() << endl;
	cout << "Punkt(5,6) = " << p2.toString() << endl;
	cout << "Punkt(Punkt(5,6)) = " << p3.toString() << endl;
	cout << "Punkt(Punkt(5,6), Wektor(-1,1)) = " << p4.toString() << endl;
	cout << endl;

	// Testowanie prostych
	Prosta pr1 = Prosta();							// konstruktor domyslny
	Prosta pr2 = Prosta(Punkt(0, 1), Punkt(4, 9));	// konstruktor (Punkt, Punkt)
	Prosta pr3 = Prosta(Wektor(-3, -2));			// konstruktor (Wektor)
	Prosta pr4 = Prosta(-3, 2, -1);					// konstruktor (double, double, double)
	Prosta pr5 = Prosta(pr4, Wektor(1, -2));		// konstruktor (Prosta, Wektor)

	cout << "Prosta() = " << pr1.toString() << endl;
	cout << "Prosta(Punkt(0, 1), Punkt(4, 9)) = " << pr2.toString() << endl;
	cout << "Prosta(Wektor(-3, -2)) = " << pr3.toString() << endl;
	cout << "Prosta(-3, 2, -1) = " << pr4.toString() << endl;
	cout << "Prosta(Prosta(-3, 2, -1), Wektor(1, -2)) = " << pr5.toString() << endl;

	Prosta pr6 = Prosta(-3, 2, 10);
	cout << "Czy proste (-3,2,10) i (-3,2,-3) sa rownolegle? " << czyRownolegle(pr6, pr5) << endl;
	cout << "Czy proste (5,13,2.5) i (-3,2,-3) sa rownolegle? " << czyRownolegle(Prosta(5, 13, 2.5), pr5) << endl;
	cout << "Czy proste (1.0/3, 2, 10) i (-3,2,-3) sa prostopadle? " << czyProstopadle(Prosta(4, 6, 10), pr5) << endl;

	// Testowanie przypadkow patologicznych
	for (int i = 0; i < 2; i++) {
		try {
			if (i == 0) {
				cout << "Proba utworzenia prostej o wzorze: 0x + 0y + C = 0: ";
				Prosta(0, 0, 7);
			}
			else {
				cout << "Proba utworzenia prostej za pomoca dwoch tych samych punktow: ";
				Prosta(Punkt(4, 4), Punkt(4, 4));
			}
		}
		catch (std::invalid_argument ia) {
			cout << ia.what() << endl;
		}
	}
	

	// Kolejny test prostych
	cout << "Czy Prosta((1,1),(0,0)) == Prosta((0,0),(1,1))?" << endl;
	Prosta pr7 = Prosta(Punkt(1, 1), Punkt(0, 0));
	Prosta pr8 = Prosta(Punkt(0, 0), Punkt(1, 1));
	cout << pr7.toString() << endl;
	cout << pr8.toString() << endl;
	cout << "Czy punkt (0,0) nalezy do prostej (1,2,0)?: ";
	cout << Prosta(1, 2, 0).czyLezyNaProstej(Punkt(0, 0)) << endl;


	std::cin.get();
	return 0;
}