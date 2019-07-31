#pragma once
#include <sstream>
#include <string>
class Wektor
{
public:
	const double dx = 0;
	const double dy = 0;
public:
	Wektor() = default;
	Wektor(double delta_x, double delta_y);
	Wektor(const Wektor &w);
	~Wektor();
	Wektor & operator= (const Wektor &) = delete;

	std::string toString();
};

// funkcja skladajaca dwa wektory
Wektor zloz(const Wektor w1, const Wektor w2);

