#pragma once
#define _USE_MATH_DEFINES
#include "Expression.h"
#include <cmath>
#include <math.h>

class Constant :
	public Expression
{
protected:
	std::string symbol;
	const double value;
public:
	Constant(std::string symbol, double value);
	~Constant();
	double eval();
	std::string toString();
};


class E :
	public Constant
{
public:
	E() : Constant("e", exp(1)) {};
	~E() {};
};


class Fi :
	public Constant
{
public:
	Fi() : Constant("fi", (1 + sqrt(5)) / 2) {};
	~Fi() {};
};


class Pi :
	public Constant
{
public:
	Pi() : Constant("pi", M_PI) {};
	~Pi() {};
};
