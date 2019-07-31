#pragma once
#include "Punkt.h"
#include "Wektor.h" // ale wektor.h zawiera sie w punkt.h


// jakos zrob zeby byla niekopiowalna
class Prosta
{
private:
	double a;
	double b;
	double c;
	// ax + by + c = 0

private:
	void normalizuj();

public:
	Prosta();
	Prosta(const Punkt &p1, const Punkt &p2);
	Prosta(const Wektor &w);
	Prosta(double na, double nb, double nc);
	Prosta(const Prosta &p, const Wektor &w);
	Prosta operator= (const Prosta &) = delete;
	~Prosta();
	bool czyProstopadly(const Wektor &w);
	bool czyRownolegly(const Wektor &w);
	bool czyLezyNaProstej(const Punkt &p);
	double odlegloscOdProstej(const Punkt &p);

	double getA() const;
	double getB() const;
	double getC() const;

	std::string toString();
};

bool czyProstopadle(const Prosta &p1, const Prosta &p2);
bool czyRownolegle(const Prosta &p1, const Prosta &p2);
Punkt & punktPrzecieca(const Prosta &p1, const Prosta &p2);

