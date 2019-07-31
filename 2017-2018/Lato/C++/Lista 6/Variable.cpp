#include "Variable.h"


std::vector <std::pair<std::string, double>> Variable::valuation;


Variable::Variable(std::string var)
{
	this->variable = var;
}

Variable::~Variable()
{
}

double Variable::eval()
{
	int i = find_valuation(variable);
	return valuation[i].second;
}

std::string Variable::toString()
{
	return variable;
}

int Variable::find_valuation(std::string variable)
{
	int i = -1;
	for (auto pair : valuation) {
		++i;
		if (pair.first == variable)
			return i;
	}
	throw std::invalid_argument("No variable present in valuation");
}

void Variable::add_valuation(std::string variable, double value)
{
	valuation.insert(valuation.begin(), std::make_pair(variable, value));
}

void Variable::remove_valuation(std::string variable)
{
	valuation.erase(valuation.begin() + find_valuation(variable));
}

void Variable::change_valuation(std::string variable, double value)
{
	valuation[find_valuation(variable)] = std::make_pair(variable, value);
}
