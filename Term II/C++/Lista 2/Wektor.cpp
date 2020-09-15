#include "Wektor.h"


Wektor::Wektor(double delta_x, double delta_y) : dx(delta_x), dy(delta_y)
{
}

Wektor::Wektor(const Wektor & w) : dx(w.dx), dy(w.dy)
{
}


Wektor::~Wektor()
{
}

std::string Wektor::toString()
{
	std::ostringstream strs;
	strs << "[" << dx << ", " << dy << "]";
	return strs.str();
}

Wektor zloz(const Wektor w1, const Wektor w2)
{
	// zwykle dodanie skladowych wektorow
	return Wektor(w1.dx + w2.dx, w1.dy + w2.dy);
}
