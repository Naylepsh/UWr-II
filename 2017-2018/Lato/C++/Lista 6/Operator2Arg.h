#pragma once
#include "Operator1Arg.h"
class Operator2Arg :
	public Operator1Arg
{
public:
	Expression * arg2;
public:
	virtual double eval() = 0;
	virtual std::string toString() = 0;
};


class Add :
	public Operator2Arg
{
public:
	Add(Expression * expr1, Expression * expr2);
	~Add();
	double eval();
	std::string toString();
};


class Divide :
	public Operator2Arg
{
public:
	Divide(Expression * expr1, Expression * expr2);
	~Divide();
	double eval();
	std::string toString();
};


class Log :
	public Operator2Arg
{
public:
	Log(Expression * expr1, Expression * expr2);
	~Log();
	double eval();
	std::string toString();
};


class Mod :
	public Operator2Arg
{
public:
	Mod(Expression * expr1, Expression * expr2);
	~Mod();
	double eval();
	std::string toString();
};


class Multiply :
	public Operator2Arg
{
public:
	Multiply(Expression * expr1, Expression * expr2);
	~Multiply();
	double eval();
	std::string toString();
};


class Pow :
	public Operator2Arg
{
public:
	Pow(Expression * expr1, Expression * expr2);
	~Pow();
	double eval();
	std::string toString();
};


class Subtract :
	public Operator2Arg
{
public:
	Subtract(Expression * expr1, Expression * expr2);
	~Subtract();
	double eval();
	std::string toString();
};

