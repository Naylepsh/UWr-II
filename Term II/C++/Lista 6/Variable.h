#pragma once
#include "Expression.h"
#include <vector>


class Variable :
	public Expression
{
private:
	std::string variable;
	static std::vector <std::pair<std::string, double>> valuation;
public:
	Variable(std::string var = "");
	~Variable();
	double eval();
	std::string toString();

	int find_valuation(std::string variable);
	void add_valuation(std::string variable, double value);
	void remove_valuation(std::string variable);
	void change_valuation(std::string variable, double value);
};

