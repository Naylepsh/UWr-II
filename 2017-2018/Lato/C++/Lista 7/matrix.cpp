#include "matrix.h"



eval::Matrix::Matrix(const int rows)
{
	// Diagonal matrix
	if (rows < 1) {
		throw new InvalidMatrixSize();
	}

	this->rows = rows;
	this->cols = rows;
	this->matrix = new double*[rows];
	InitMatrix();

	// fill arrays with appropiate values
	for (int i = 0; i < rows; i++) {
		for (int j = 0; j < cols; j++) {
			if (i == j)
				matrix[i][j] = 1;
			else
				matrix[i][j] = 0;
		}
	}
}

eval::Matrix::Matrix(int rows, int cols)
{
	// matrix filled with 0s
	if (rows < 1 || cols < 1) {
		throw new InvalidMatrixSize();
	}

	this->rows = rows;
	this->cols = cols;
	this->matrix = new double*[rows];
	InitMatrix();

	// fill arrays with appropiate values
	for (int i = 0; i < rows; i++) {
		for (int j = 0; j < cols; j++) {
			matrix[i][j] = 0;
		}
	}
}

eval::Matrix::Matrix(Matrix & matrix)
{
	this->rows = matrix.rows;
	this->cols = matrix.cols;
	this->matrix = new double*[rows];
	InitMatrix();

	// fill arrays with appropiate values
	for (int i = 0; i < rows; i++) {
		for (int j = 0; j < cols; j++) {
			this->matrix[i][j] = matrix.matrix[i][j];
		}
	}
}

eval::Matrix::Matrix(Matrix && matrix)
{
	this->matrix = matrix.matrix;
	this->rows = matrix.rows;
	this->cols = matrix.cols;
	matrix.matrix = nullptr;
	matrix.rows = 0;
	matrix.cols = 0;
}

eval::Matrix::~Matrix()
{
	DeleteMatrix();
}

eval::Matrix & eval::Matrix::operator=(const Matrix & matrix)
{
	this->rows = matrix.rows;
	this->cols = matrix.cols;
	DeleteMatrix();
	this->matrix = new double*[rows];
	InitMatrix();

	// fill arrays with appropiate values
	for (int i = 0; i < rows; i++) {
		for (int j = 0; j < cols; j++) {
			this->matrix[i][j] = matrix.matrix[i][j];
		}
	}

	return *this;
}


eval::Matrix & eval::Matrix::operator=(Matrix && matrix)
{
	if (this == &matrix)
		return *this;
	this->DeleteMatrix();
	this->matrix = matrix.matrix;
	this->rows = matrix.rows;
	this->cols = matrix.cols;
	matrix.matrix = nullptr;
	matrix.rows = 0;
	matrix.cols = 0;

	return *this;
}

eval::Matrix & eval::Matrix::operator*=(double scalar)
{
	for (int i = 0; i < rows; i++) {
		for (int j = 0; j < cols; j++)
			matrix[i][j] *= scalar;
	}

	return *this;
}

eval::Matrix eval::Matrix::operator*(double scalar)
{
	Matrix temp = Matrix(*this);
	return temp *= scalar;
}

eval::Matrix & eval::Matrix::operator+=(const Matrix & matrix)
{
	if (rows != matrix.rows || cols != matrix.cols) {
		throw new InvalidMatrixSize();
	}

	for (int i = 0; i < rows; i++) {
		for (int j = 0; j < cols; j++)
			this->matrix[i][j] += matrix.matrix[i][j];
	}
	
	return *this;
}

eval::Matrix eval::Matrix::operator+(const Matrix & matrix)
{
	Matrix temp = Matrix(*this);
	return temp += matrix;
}

eval::Matrix & eval::Matrix::operator-=(const Matrix & matrix)
{
	if (rows != matrix.rows || cols != matrix.cols) {
		throw new InvalidMatrixSize();
	}

	for (int i = 0; i < rows; i++) {
		for (int j = 0; j < cols; j++)
			this->matrix[i][j] -= matrix.matrix[i][j];
	}

	return *this;
}

eval::Matrix eval::Matrix::operator-(const Matrix & matrix)
{
	Matrix temp = Matrix(*this);
	return temp -= matrix;
}

eval::Matrix eval::Matrix::operator*(const Matrix & matrix)
{
	if (rows != matrix.cols) {
		throw new InvalidMatrixSize();
	}

	Matrix temp = Matrix(rows, matrix.cols);

	for (int i = 0; i < temp.rows; i++) {
		for (int j = 0; j < temp.cols; j++) {
			for (int k = 0; k < cols; k++) {
				temp.matrix[i][j] += this->matrix[i][k] * matrix.matrix[k][j];
			}
		}
	}

	return temp;
}

eval::Matrix eval::Matrix::Transpose()
{
	Matrix temp = Matrix(cols, rows);
	for (int i = 0; i < rows; i++) {
		for (int j = 0; j < cols; j++) {
			temp.matrix[j][i] = matrix[i][j];
		}
	}
	return temp;
}

eval::Matrix & eval::Matrix::SwapRows(int r1, int r2)
{
	IndexCheck(r1, 0, rows);
	IndexCheck(r2, 0, rows);

	double * t = matrix[r1];
	matrix[r1] = matrix[r2];
	matrix[r2] = t;

	return *this;
}

eval::Matrix & eval::Matrix::SwapCols(int c1, int c2)
{
	IndexCheck(c1, 0, cols);
	IndexCheck(c2, 0, cols);

	for (int i = 0; i < rows; i++) {
		double t = matrix[i][c1];
		matrix[i][c1] = matrix[i][c2];
		matrix[i][c2] = t;
	}

	return *this;
}

eval::Matrix & eval::Matrix::MultRow(int row, double scalar)
{
	//ScalarCheck(scalar);
	IndexCheck(row, 0, rows);

	for (int i = 0; i < cols; i++) {
		matrix[row][i] *= scalar;
	}

	return *this;
}

eval::Matrix & eval::Matrix::MultCol(int col, double scalar)
{
	ScalarCheck(scalar);
	IndexCheck(col, 0, cols);

	for (int i = 0; i < rows; i++) {
		matrix[i][col] *= scalar;
	}

	return *this;
}

eval::Matrix & eval::Matrix::AddMultRow(int from, int to, double scalar)
{
	//ScalarCheck(scalar);
	IndexCheck(from, 0, rows);
	IndexCheck(to, 0, rows);

	for (int i = 0; i < cols; i++) {
		matrix[to][i] += scalar * matrix[from][i];
	}

	return *this;
}

eval::Matrix & eval::Matrix::AddMultCol(int from, int to, double scalar)
{
	ScalarCheck(scalar);
	IndexCheck(from, 0, cols);
	IndexCheck(to, 0, cols);

	for (int i = 0; i < rows; i++) {
		matrix[i][to] += scalar * matrix[i][from];
	}

	return *this;
}

eval::Matrix eval::Matrix::RemoveRow(int row)
{
	IndexCheck(row, 0, rows);

	Matrix temp = Matrix(rows - 1, cols);
	temp.InitMatrix();
	int temp_i = 0;
	for (int i = 0; i < rows; i++) {
		if (i != row) {
			for (int j = 0; j < cols; j++) {
				temp.matrix[temp_i][j] = matrix[i][j];
			}
			++temp_i;
		}
	}

	return temp;
}

eval::Matrix eval::Matrix::RemoveCol(int col)
{
	IndexCheck(col, 0, cols);

	Matrix temp = Matrix(rows, cols - 1);
	temp.InitMatrix();
	int temp_j = 0;
	for (int i = 0; i < rows; i++) {
		for (int j = 0; j < cols; j++) {
			if (j != col) {
				temp.matrix[i][temp_j] = matrix[i][j];
				++temp_j;
			}
		}
		temp_j = 0;
	}

	return temp;
}

double eval::Matrix::Determinant()
{
	if (rows != cols) {
		throw new InvalidMatrixSize();
	}

	Matrix temp = Matrix(*this);
	double det = 1;
	while (temp.rows > 1) {
		// check whether top left corner value = 0
		if (temp.matrix[0][0] == 0) {
			// if it is - search for non-zero value and swap
			int i = 1;
			for (i; i < rows; i++) {
				if (temp.matrix[i][0] != 0) {
					temp.SwapRows(0, i);
					break;
				}
			}
			// if there's no non-zero value in that column -- det = 0
			if (i == rows) {
				return 0;
			}
		}
		// zero-out the first column of each row except the 0th;
		for (int i = 1; i < temp.rows; i++) {
			// only do calculation when 0th value of i-th row is non-zero
			if (temp.matrix[i][0] != 0) {
				double scalar = (-1) * temp.matrix[i][0] / temp.matrix[0][0];
				temp.AddMultRow(0, i, scalar);
			}
		}
		// update det
		det *= temp.matrix[0][0];
		// remove the 0th row and column
		temp = temp.RemoveCol(0).RemoveRow(0);
	}
	return det * temp.matrix[0][0];
}

eval::Matrix eval::Matrix::Reverse()
{
	if (cols != rows) {
		throw new InvalidMatrixSize();
	}
	if (Determinant() == 0) {
		throw new IrreversibleMatrix();
	}

	Matrix temp = Matrix(*this);
	Matrix reversed = Matrix(rows);

	// turn matrix into upper-triangular form
	bool skip = false;
	for (int i = 0; i < temp.rows; i++) {
		if (temp.matrix[i][i] == 0) {
			int j = i + 1;
			for (j; j < rows; j++) {
				if (temp.matrix[j][i] != 0) {
					temp.SwapRows(0, j);
					reversed.SwapRows(0, j);
					break;
				}
			}
			// if there's no non-zero value in that column then move to the next one
			if (j == rows) {
				skip = true;
			}
		}
		if (!(skip)) {
			// turn Ai,i val to 1
			double scalar = 1.0 / temp.matrix[i][i];
			temp.MultRow(i, scalar);
			reversed.MultRow(i, scalar);

			// zero-out the lower rows
			for (int j = i + 1; j < rows; j++) {
				if (temp.matrix[j][i] != 0) {
					scalar = (-1) * temp.matrix[j][i] / temp.matrix[i][i];
					temp.AddMultRow(i, j, scalar);
					reversed.AddMultRow(i, j, scalar);
				}
			}
		}
	}
	// turn matrix into diagonal form
	double scalar;
	for (int i = rows - 1; i >= 0; i--) {
		for (int j = i - 1; j >= 0; j--) {
			if (temp.matrix[j][i] != 0) {
				scalar = (-1) * temp.matrix[j][i] / temp.matrix[i][i];
				temp.AddMultRow(i, j, scalar);
				reversed.AddMultRow(i, j, scalar);
			}
		}
	}
	return reversed;
}

void eval::Matrix::InitMatrix()
{
	
	for (int i = 0; i < rows; i++) {
		matrix[i] = new double[cols];
	}
}

void eval::Matrix::DeleteMatrix()
{
	// delete each row
	for (int i = 0; i < rows; i++) {
		delete[] matrix[i];
	}

	// delete pointer to former matrix
	if (matrix != nullptr) {
		delete[] matrix;
	}
}

bool eval::Matrix::IsWithinRange(int x, int min, int max)
{
	return (min <= x && x < max);
}

void eval::Matrix::IndexCheck(int i, int min, int max)
{
	if (!(IsWithinRange(i, min, max))) {
		throw new MatrixIndexOutOfRange();
	}
}

void eval::Matrix::ScalarCheck(double scalar)
{
	if (scalar == 0) {
		throw new InvalidScalar();
	}
}

std::istream & eval::operator>>(std::istream & is, Matrix & matrix)
{
	for (int i = 0; i < matrix.rows; i++) {
		for (int j = 0; j < matrix.cols; j++) {
			is >> matrix.matrix[i][j];
		}
	}

	return is;
}

std::ostream & eval::operator<<(std::ostream & os, Matrix & matrix)
{
	for (int i = 0; i < matrix.rows; i++) {
		for (int j = 0; j < matrix.cols; j++) {
			os << matrix.matrix[i][j] << " ";
		}
		os << "\n";
	}

	return os;
}
