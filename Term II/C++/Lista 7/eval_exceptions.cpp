#include "eval_exceptions.h"



EvalException::EvalException(const char * error)
{
	this->error = error;
}

EvalException::EvalException(EvalException & ee)
{
	error = ee.error;
}

EvalException & EvalException::operator=(EvalException & ee)
{
	error = ee.error;
	return *this;
}


EvalException::~EvalException()
{
}

const char * EvalException::what() const throw()
{
	return error;
}

InvalidScalar::InvalidScalar() : EvalException("Invalid scalar - Value must be non-zero")
{
}

InvalidScalar::InvalidScalar(InvalidScalar & is) : EvalException(is)
{
}

InvalidScalar::~InvalidScalar()
{
}

MatrixIndexOutOfRange::MatrixIndexOutOfRange() : EvalException("Index is out of matrix' range")
{
}

MatrixIndexOutOfRange::MatrixIndexOutOfRange(MatrixIndexOutOfRange & mioor) : EvalException(mioor)
{
}

MatrixIndexOutOfRange::~MatrixIndexOutOfRange()
{
}

InvalidMatrixSize::InvalidMatrixSize() : EvalException("Matrices have different sizes")
{
}

InvalidMatrixSize::InvalidMatrixSize(InvalidMatrixSize & ims) : EvalException(ims)
{
}

InvalidMatrixSize::~InvalidMatrixSize()
{
}

IrreversibleMatrix::IrreversibleMatrix() : EvalException("Irreversible matrix")
{
}

IrreversibleMatrix::IrreversibleMatrix(IrreversibleMatrix & im) : EvalException(im)
{
}

IrreversibleMatrix::~IrreversibleMatrix()
{
}
