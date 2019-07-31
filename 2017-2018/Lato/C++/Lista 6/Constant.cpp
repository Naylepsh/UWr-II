#include "Constant.h"



Constant::Constant(std::string symbol, double value) : value(value)
{
	this->symbol = symbol;
}

Constant::~Constant()
{
}

double Constant::eval()
{
	return value;
}

std::string Constant::toString()
{
	return symbol;
}
