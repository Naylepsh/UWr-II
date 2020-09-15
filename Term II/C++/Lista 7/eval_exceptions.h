#pragma once
#include <exception>
#include <iostream>
class EvalException :
	public std::exception
{
private:
	const char * error;
public:
	EvalException(const char * error = "Encountered error");
	EvalException(EvalException & ee);
	EvalException & operator=(EvalException & ee);
	~EvalException() = 0;
	const char * what() const throw();
};

class InvalidScalar :
	public EvalException {
public:
	InvalidScalar();
	InvalidScalar(InvalidScalar & is);
	~InvalidScalar();
};

class MatrixIndexOutOfRange :
	public EvalException {
public:
	MatrixIndexOutOfRange();
	MatrixIndexOutOfRange(MatrixIndexOutOfRange & mioor);
	~MatrixIndexOutOfRange();
};

class InvalidMatrixSize :
	public EvalException {
public:
	InvalidMatrixSize();
	InvalidMatrixSize(InvalidMatrixSize & ims);
	~InvalidMatrixSize();
};

class IrreversibleMatrix :
	public EvalException {
public:
	IrreversibleMatrix();
	IrreversibleMatrix(IrreversibleMatrix & im);
	~IrreversibleMatrix();
};