#include "Operator1Arg.h"
#include "Constant.h"



Abs::Abs(Expression * expr)
{
	arg1 = expr;
}

Abs::~Abs()
{
	delete arg1;
}

double Abs::eval()
{
	return abs(arg1->eval());
}

std::string Abs::toString()
{
	return "|" + arg1->toString() + "|";
}



Cos::Cos(Expression * expr)
{
	arg1 = expr;
}

Cos::~Cos()
{
	delete arg1;
}

double Cos::eval()
{
	return cos((*arg1).eval());
}

std::string Cos::toString()
{
	return "cos(" + (*arg1).toString() + ")";
}



Exp::Exp(Expression * expr)
{
	arg1 = expr;
}

Exp::~Exp()
{
	delete arg1;
}

double Exp::eval()
{
	return exp(arg1->eval());
}

std::string Exp::toString()
{
	return E().toString() + "^(" + arg1->toString() + ")";
}



Inverse::Inverse(Expression * expr)
{
	arg1 = expr;
}

Inverse::~Inverse()
{
	delete arg1;
}

double Inverse::eval()
{
	return 1 / arg1->eval();
}

std::string Inverse::toString()
{
	return "1/(" + arg1->toString() + ")";
}



Ln::Ln(Expression * expr)
{
	arg1 = expr;
}

Ln::~Ln()
{
	delete arg1;
}

double Ln::eval()
{
	return log(arg1->eval());
}

std::string Ln::toString()
{
	return "log(" + arg1->toString() + ")";
}



Opposite::Opposite(Expression * expr)
{
	arg1 = expr;
}

Opposite::~Opposite()
{
	delete arg1;
}

double Opposite::eval()
{
	return (-1) * arg1->eval();
}

std::string Opposite::toString()
{
	return "-(" + arg1->toString() + ")";
}



Sin::Sin(Expression * expr)
{
	arg1 = expr;
}

Sin::~Sin()
{
	delete arg1;
}

double Sin::eval()
{
	return sin(arg1->eval());
}

std::string Sin::toString()
{
	return "sin(" + arg1->toString() + ")";
}
