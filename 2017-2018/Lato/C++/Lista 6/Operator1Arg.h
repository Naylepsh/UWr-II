#pragma once
#include "Expression.h"

class Operator1Arg :
	public Expression
{
public:
	Expression * arg1;
public:
	virtual double eval() = 0;
	virtual std::string toString() = 0;
};


class Abs :
	public Operator1Arg
{
public:
	Abs(Expression * expr);
	~Abs();
	double eval();
	std::string toString();
};


class Cos :
	public Operator1Arg
{
public:
	Cos(Expression * arg1);
	~Cos();
	double eval();
	std::string toString();
};


class Exp :
	public Operator1Arg
{
public:
	Exp(Expression * arg1);
	~Exp();
	double eval();
	std::string toString();
};


class Inverse :
	public Operator1Arg
{
public:
	Inverse(Expression * expr);
	~Inverse();
	double eval();
	std::string toString();
};


class Ln :
	public Operator1Arg
{
public:
	Ln(Expression * expr);
	~Ln();
	double eval();
	std::string toString();
};


class Opposite :
	public Operator1Arg
{
public:
	Opposite(Expression * expr);
	~Opposite();
	double eval();
	std::string toString();
};


class Sin :
	public Operator1Arg
{
public:
	Sin(Expression * arg1);
	~Sin();
	double eval();
	std::string toString();
};

