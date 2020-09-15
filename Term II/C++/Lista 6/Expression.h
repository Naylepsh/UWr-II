#pragma once
#include <iostream>

class Expression {
public:
	virtual double eval() = 0;
	virtual std::string toString() = 0;
};