#include "Number.h"
#include <sstream>



Number::Number(double number)
{
	this->number = number;
}

Number::~Number()
{
}

double Number::eval()
{
	return number;
}

std::string Number::toString()
{
	std::ostringstream stream;
	stream << number;
	return stream.str();
}
