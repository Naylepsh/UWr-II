#pragma once
#include "Wektor.h"
#include <sstream>
#include <string>
class Punkt
{
public:
	const double x = 0;
	const double y = 0;
public:
	Punkt() = default;
	Punkt(double nx, double ny);
	Punkt(const Punkt &p, const Wektor &w1);
	Punkt(const Punkt &p);
	Punkt operator= (const Punkt &) = delete;
	~Punkt();

	std::string toString();
};

