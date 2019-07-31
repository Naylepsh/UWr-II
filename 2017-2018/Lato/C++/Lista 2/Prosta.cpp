#include "Prosta.h"

const double EPSILON = 0.000001;


void Prosta::normalizuj()
{
	// normalizujemy wspolczynniki prostej
	// w formie sqrt(A*A + B*B) = 1

	// wyliczamy mnoznik
	double m;
	if (c < 0)
		m = 1 / (sqrt(a*a + b*b));
	else
		m = (-1) / (sqrt(a*a + b*b));

	// poprawiamy wspolczynniki
	a *= m;
	b *= m;
	c *= m;
}

Prosta::Prosta()
{
	// domyslnie: 1y = 0x + 0
	a = 0;
	b = 1;
	c = 0;
}

Prosta::Prosta(const Punkt & p1, const Punkt & p2)
{
	// obliczamy wspolczynniki na podstawie wspolrzednych dwoch punktow

	// nie mozna obliczyc prostej na podstawie dwoch takich samych punktow
	if (p1.x == p2.x && p1.y == p2.y)
		throw std::invalid_argument("nie mozna jednoznacznie utworzyc prostej");

	a = (p2.y - p1.y) / (p1.x - p2.x);
	b = 1.0;
	c = (-1) * (a*p1.x + b*p1.y);
	normalizuj();
}

Prosta::Prosta(const Wektor & w)
{
	// tworzenie prostej prostopadlej do podanego wektora

	// p1 = (0,0), p2 = (w.dx, w.dy)
	// y = a'x + c' => a' = (y-c)/x = dy/dx
	// y = ax + c (prosta prostopadla do tej wyzej)
	// a = (-1) / a'
	// ale jako ze zapisujemy to w postaci ax + by + c = 0, to bierzemy przeciwny znak
	a = w.dx / w.dy;
	b = 1;
	// c = -ax - by, gdzie b = 1
	c = (-1)*a*w.dx - w.dy;
	normalizuj();
}

Prosta::Prosta(double na, double nb, double nc)
{
	// tworzenie prostej o podanych wspolczynnikach

	// wsp. A i B nie moga byc jednoczesnie rowne zero
	if (na == 0 && nb == 0)
		throw std::invalid_argument("nie mozna jednoznacznie utworzyc prostej");

	a = na;
	b = nb;
	c = nc;
	normalizuj();
}

Prosta::Prosta(const Prosta & p, const Wektor & w)
{
	// tworzenie prostej rownoleglej do podanej prostej
	// przesunietej o podany wektor

	// jesli mamy pionowa prosta
	// to przesuwamy ja tylko o dx
	if (p.b == 0) {
		a = p.a;
		b = p.b;
		c = w.dx;
	}
	else
	{
		a = p.a;
		b = p.b;
		c = (a / b)*w.dx - p.c / b + w.dy;	// wyjasnienie na rysunku w folderze
	}
	normalizuj();
}


Prosta::~Prosta()
{
}

bool Prosta::czyProstopadly(const Wektor & w)
{
	Prosta p = Prosta(w);
	p.normalizuj();
	return czyProstopadle(*this, p);
}

bool Prosta::czyRownolegly(const Wektor & w)
{
	Prosta p = Prosta(w);
	return czyRownolegle(*this, p);
}

bool Prosta::czyLezyNaProstej(const Punkt & p)
{
	if (abs(a*p.x + b * p.y + c) <= EPSILON)
		return true;
	return false;
}

double Prosta::odlegloscOdProstej(const Punkt & p)
{
	double d = abs((a* p.x + b * p.y + c)) / sqrt(a* a + b * b);
	return d;
}

double Prosta::getA() const
{
	return a;
}

double Prosta::getB() const
{
	return b;
}

double Prosta::getC() const
{
	return c;
}

std::string Prosta::toString()
{
	std::ostringstream strs;
	strs << a << "x + " << b << "y + " << c;
	return strs.str();
}

bool czyProstopadle(const Prosta & p1, const Prosta & p2)
{
	// sprawdzamy czy wspolczynnik jednej prostej
	// jest odwrotny i ze znakiem przeciwnym do 
	// wspolczynnika drugiej

	// a = - A / B
	// a1 * a2 == -1
	double product = p1.getA() * p2.getA() / p1.getB() / p2.getB();
	return (product  >= -1 - EPSILON && product <= -1 + EPSILON);
}

bool czyRownolegle(const Prosta & p1, const Prosta & p2)
{
	// sprawdzamy czy wspolczynniki obu prostych sa (mniej wiecej) takie same
	return abs(p1.getA() / p1.getB() - p2.getA() / p2.getB()) <= EPSILON;
}

Punkt & punktPrzecieca(const Prosta & p1, const Prosta & p2)
{
	// punkt przeciecia dwoch prostych istnieje wtw gdy proste
	// sa do siebie nierownolegle
	if (czyRownolegle(p1, p2))
		throw std::invalid_argument("proste sa rownolegle");

	// wyznaczone za pomoca gdzies tam udowodnionego wzoru
	double x = (p1.getA()*p2.getC() - p1.getC()*p2.getB()) / 
		(p1.getA() - p1.getB()*p2.getA());
	double y = (-1)*(p1.getA() * x + p1.getC()) / p1.getB();
	
	return Punkt(x, y);
}
