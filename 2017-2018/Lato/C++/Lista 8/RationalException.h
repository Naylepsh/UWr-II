#pragma once
#include <exception>
#include <iostream>
class RationalException :
	public std::exception
{
private:
	const char * msg;
public:
	RationalException(const char * msg);
	~RationalException();
	const char * what() const;
};


class DivisionBy0 :
	public RationalException {
public:
	DivisionBy0(const char * msg = "Cannot divide by 0");
	~DivisionBy0();
};


class OutOfRange :
	public RationalException {
public:
	OutOfRange(const char * msg = "Integer range exceeded");
	~OutOfRange();
};

