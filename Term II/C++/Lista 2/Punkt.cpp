#include "Punkt.h"


Punkt::Punkt(double nx, double ny) : x(nx), y(ny)
{
}

Punkt::Punkt(const Punkt & p, const Wektor & w) : x(p.x + w.dx), y(p.y + w.dy)
{
}

Punkt::Punkt(const Punkt & p) : x(p.x), y(p.y)
{
}


Punkt::~Punkt()
{
}

std::string Punkt::toString()
{
	std::ostringstream strs;
	strs << "(" << x << ", " << y << ")";
	return strs.str();
}
