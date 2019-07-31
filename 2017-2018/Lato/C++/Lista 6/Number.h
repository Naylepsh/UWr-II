#pragma once
#include "Expression.h"
class Number :
	public Expression
{
private:
	double number;
public:
	Number(double number = 0);
	~Number();
	double eval();
	std::string toString();
};

