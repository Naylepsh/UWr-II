#pragma once
#include <iostream>
#include "RationalException.h"
#include <climits>
#include <vector>
class Rational
{
private:
	int numerator;
	int denominator;
public:
	Rational(int num, int denom=1);
	~Rational();

	// arith. operators
	Rational operator+(const Rational & rat);
	Rational operator-(const Rational & rat);
	Rational operator*(const Rational & rat);
	Rational operator/(const Rational & rat);
	Rational operator-();
	Rational operator!();

	// streams
	friend std::ostream & operator<<(std::ostream & os, const Rational & rat);

	// casting
	explicit operator double() const;
	explicit operator int() const;

	// getters
	int GetNum() const;
	int GetDenom() const;
};

int gcd(int a, int b);
int lcm(int a, int b);
int sgn(int x);

// lots of helpers
static void AdditionCheck(int a, int b);
static void SubstractionCheck(int a, int b);
static void MultiplicationCheck(int a, int b);
static std::string DecimalForm(int x, int y);
static std::string NormalForm(int x, int y);
static bool IsIn(int x, const std::vector<std::pair<int, int>> v);
static bool IsIn(int x, int y, const std::vector<std::pair<int, int>> v);
static int GetIndex(int x, const std::vector<std::pair<int,int>> v);
static int GetIndex(int x, int y, std::vector<std::pair<int, int>> v);
