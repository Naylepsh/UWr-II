#include "Rational.h"
#include <climits>
#include <string>
#include <sstream>

Rational::Rational(int num, int denom)
{
	if (denom == 0)
		throw DivisionBy0();

	if (num == 0) {
		numerator = 0;
		denominator = 1;
	}
	else {
		int div = gcd(num, denom);
		numerator = num / div;
		denominator = denom / div;
		if (sgn(denominator) == -1) {
			numerator *= -1;
			denominator *= -1;
		}
			
	}
}

Rational::~Rational()
{
}

Rational Rational::operator+(const Rational & rat)
{
	int new_denom = lcm(denominator, rat.denominator);
	MultiplicationCheck(numerator, new_denom);
	int n1 = numerator * new_denom / denominator;
	MultiplicationCheck(rat.numerator, new_denom);
	int n2 = rat.numerator * new_denom / rat.denominator;
	AdditionCheck(n1, n2);
	
	return Rational(n1 + n2, new_denom);
}

Rational Rational::operator-(const Rational & rat)
{
	int new_denom = lcm(denominator, rat.denominator);
	MultiplicationCheck(numerator, new_denom);
	int n1 = numerator * new_denom / denominator;
	MultiplicationCheck(rat.numerator, new_denom);
	int n2 = rat.numerator * new_denom / rat.denominator;
	AdditionCheck(n1, -1 * n2);

	return Rational(n1 - n2, new_denom);
}

Rational Rational::operator*(const Rational & rat)
{
	MultiplicationCheck(numerator, rat.numerator);
	int new_num = numerator * rat.numerator;
	MultiplicationCheck(denominator, rat.denominator);
	int new_denom = denominator * rat.denominator;

	return Rational(new_num, new_denom);
}

Rational Rational::operator/(const Rational & rat)
{
	if (rat.numerator == 0)
		throw DivisionBy0();

	MultiplicationCheck(numerator, rat.denominator);
	int new_num = numerator * rat.denominator;
	MultiplicationCheck(denominator, rat.numerator);
	int new_denom = denominator * rat.numerator;

	return Rational(new_num, new_denom);
}

Rational Rational::operator-()
{
	MultiplicationCheck(numerator, -1);

	return Rational(numerator * -1, denominator);
}

Rational Rational::operator!()
{
	if (numerator == 0)
		throw DivisionBy0();

	return Rational(denominator, numerator);
}

Rational::operator double() const
{
	return (double)numerator / denominator;
}

Rational::operator int() const
{
	return numerator / denominator;
}

int Rational::GetNum() const
{
	return numerator;
}

int Rational::GetDenom() const
{
	return denominator;
}

std::ostream & operator<<(std::ostream & os, const Rational & rat)
{
	os << DecimalForm(rat.numerator, rat.denominator);
	//os << NormalForm(rat.numerator, rat.denominator);
	return os;
}

int gcd(int a, int b)
{
	if (b == 0) return a;
	a %= b;
	return gcd(b, a);
}

int lcm(int a, int b)
{
	int temp = gcd(a, b);
	if (temp == 0)
		return 0;

	MultiplicationCheck(a / temp, b);
	int x = abs(a / temp * b);

	return abs(a*b) / temp;
}

int sgn(int x)
{
	if (x < 0)
		return -1;
	if (x > 0)
		return 1;
	return 0;
}

void AdditionCheck(int a, int b)
{
	long long c = (long long)a + b;
	if (!(INT_MIN <= c && c <= INT_MAX))
		throw OutOfRange();
}

void SubstractionCheck(int a, int b)
{
	long long c = (long long)a - b;
	if (!(INT_MIN <= c && c <= INT_MAX))
		throw OutOfRange();
}

void MultiplicationCheck(int a, int b)
{
	long long c = (long long)a * b;
	if (!(INT_MIN <= c && c <= INT_MAX))
		throw OutOfRange();
}

std::string DecimalForm(int x, int y)
{
	std::stringstream ss;
	auto divisors = std::vector<std::pair<int, int>>();

	// adding sign if needed
	if (sgn(x) != sgn(y) && sgn(x) != 0 && sgn(y) != 0)
		ss << "-";

	// number before decimal point
	ss << std::to_string(abs(x / y));
	x %= y;
	
	// adding decimal point if needed
	if (x != 0)
		ss << ".";
	else
		return ss.str();

	// digits after decimal point
	int modulo = -1;
	int val;
	bool periodic_exists = false;
	while (x%y != 0) {
		x *= 10;
		modulo = x % y;
		if (IsIn(modulo, abs(x/y), divisors)) {
			periodic_exists = true;
			val = abs(x / y);
			break;
		}
			
		divisors.push_back(std::make_pair(modulo, abs(x / y)));
		x = modulo;
	}

	// turning it all into string
	if (modulo != -1) {
		int i;
		if (periodic_exists) {
			i = GetIndex(modulo, val, divisors) - periodic_exists;
		}
		else {
			i = GetIndex(modulo, divisors) - periodic_exists;
		}

		// adding all digits before periodic number
		for (int j = 0; j <= i; j++) {
			ss << std::to_string(divisors[j].second);
		}

		// adding all digits of periodic number
		if (i != divisors.size() - 1) {
			ss << "(";
			for (int j = i + 1; j < divisors.size(); j++) {
				ss << std::to_string(divisors[j].second);
			}
			ss << ")";
		}
	}

	return ss.str();
}

std::string NormalForm(int x, int y)
{
	return std::to_string(x) + '/' + std::to_string(y);
}

bool IsIn(int x, const std::vector<std::pair<int, int>> v)
{
	for (int i = 0; i < v.size(); i++) {
		if (v[i].first == x)
			return true;
	}
	return false;
}

bool IsIn(int x, int y, const std::vector<std::pair<int, int>> v)
{
	for (int i = 0; i < v.size(); i++) {
		if (v[i].first == x && v[i].second == y)
			return true;
	}
	return false;
}

int GetIndex(int x, const std::vector<std::pair<int, int>> v)
{
	for (int i = 0; i < v.size(); i++) {
		if (v[i].first == x)
			return i;
	}
	return -1;
}

int GetIndex(int x, int y, const std::vector<std::pair<int, int>> v)
{
	for (int i = 0; i < v.size(); i++) {
		if (v[i].first == x && v[i].second == y)
			return i;
	}
	return -1;
}
