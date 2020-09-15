#include "RationalException.h"



RationalException::RationalException(const char * msg) : msg(msg)
{
}

RationalException::~RationalException()
{
}

const char * RationalException::what() const
{
	return msg;
}

DivisionBy0::DivisionBy0(const char * msg) : RationalException(msg)
{
}

DivisionBy0::~DivisionBy0()
{
}

OutOfRange::OutOfRange(const char * msg) : RationalException(msg)
{
}

OutOfRange::~OutOfRange()
{
}
