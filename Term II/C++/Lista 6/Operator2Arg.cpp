#include "Operator2Arg.h"



Add::Add(Expression * expr1, Expression * expr2)
{
	arg1 = expr1;
	arg2 = expr2;
}

Add::~Add()
{
	delete arg1;
	delete arg2;
}

double Add::eval()
{
	return arg1->eval() + arg2->eval();
}

std::string Add::toString()
{
	return "(" + arg1->toString() + "+" + arg2->toString() + ")";
}



Divide::Divide(Expression * expr1, Expression * expr2)
{
	arg1 = expr1;
	arg2 = expr2;
}


Divide::~Divide()
{
	delete arg1;
	delete arg2;
}

double Divide::eval()
{
	return arg1->eval() / arg2->eval();
}

std::string Divide::toString()
{
	return "(" + arg1->toString() + "/" + arg2->toString() + ")";
}



Log::Log(Expression * expr1, Expression * expr2)
{
	arg1 = expr1;
	arg2 = expr2;
}

Log::~Log()
{
	delete arg1;
	delete arg2;
}

double Log::eval()
{
	return log(arg1->eval()) / log(arg2->eval());
}

std::string Log::toString()
{
	return "log" + arg2->toString() + "(" + arg1->toString() + ")";
}



Mod::Mod(Expression * expr1, Expression * expr2)
{
	arg1 = expr1;
	arg2 = expr2;
}


Mod::~Mod()
{
	delete arg1;
	delete arg2;
}

double Mod::eval()
{
	return fmod(arg1->eval(), arg2->eval());
}

std::string Mod::toString()
{
	return "(" + arg1->toString() + "%" + arg2->toString() + ")";
}



Multiply::Multiply(Expression * expr1, Expression * expr2)
{
	arg1 = expr1;
	arg2 = expr2;
}

Multiply::~Multiply()
{
	delete arg1;
	delete arg2;
}

double Multiply::eval()
{
	return arg1->eval() * arg2->eval();
}

std::string Multiply::toString()
{
	return "(" + arg1->toString() + "*" + arg2->toString() + ")";
}



Pow::Pow(Expression * expr1, Expression * expr2)
{
	arg1 = expr1;
	arg2 = expr2;
}


Pow::~Pow()
{
	delete arg1;
	delete arg2;
}

double Pow::eval()
{
	return pow(arg1->eval(), arg2->eval());
}

std::string Pow::toString()
{
	return "(" + arg1->toString() + ")^(" + arg2->toString() + ")";
}



Subtract::Subtract(Expression * expr1, Expression * expr2)
{
	arg1 = expr1;
	arg2 = expr2;
}


Subtract::~Subtract()
{
	delete arg1;
	delete arg2;
}

double Subtract::eval()
{
	return arg1->eval() - arg2->eval();
}

std::string Subtract::toString()
{
	return "(" + arg1->toString() + "-" + arg2->toString() + ")";
}
