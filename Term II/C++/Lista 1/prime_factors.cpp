#include <iostream>
#include <vector>
#include <cstdint>
#include <string>


std::string MAX_LL = std::to_string(LLONG_MAX);
std::string MIN_LL = std::to_string(LLONG_MIN);


int64_t convertToInt64(std::string s);
bool isPrime(int64_t n);
std::vector<int64_t> getFactors(int64_t n);
void printFactors(int64_t n);
void printManual();


int main(int argc, char *args[]) {
	if (argc == 1) {
		printManual();
	}
	else {
		// convert char* args to strings
		std::vector<std::string> arguments;
		for (int i = 1; i < argc; i++)
			arguments.push_back(std::string(args[i]));

		// convert strings to int64_ts
		std::vector<int64_t> nums;
		for (std::string n : arguments)
			nums.push_back(convertToInt64(n));

		// print prime factors of all arguments
		for (int64_t n : nums)
			printFactors(n);
	}

	//std::cin.get();
	return 0;
}


void printManual() {
	using std::cerr;
	cerr << "---How to use---\n";
	cerr << "Enter a sequence of numbers seperated by <space>\n";
	cerr << "Each number and its factorials will be printed in the same line\n";
}


int64_t convertToInt64(std::string s) {
	// if first char is not a digit nor '-' sign throw an exception
	if ((s[0] < '0' || s[0] > '9') && s[0] != '-')
		throw std::invalid_argument("Argument is not a number.");
	// any any other char is a not a digit then throw an exception
	for (int i = 1; i < s.size(); i++) {
		if (s[i] < '0' || s[i] > '9')
			throw std::invalid_argument("Argument is not a number.");
	}
	// if number is out of range then throw an exception
	if (s[0] == '-') {
		if (s.length() > MIN_LL.length() ||
			(s.length() == MIN_LL.length() && s < MIN_LL))
			throw std::invalid_argument("Argument out of range");
	}	
	//if (s[0] != '-')
	else {
		if (s.length() > MAX_LL.length() ||
			(s.length() == MAX_LL.length() && s > MAX_LL))
			throw std::invalid_argument("Argument out of range");
	}
	return std::stoll(s);
}


void printFactors(int64_t n) {
	using std::cout;
	std::vector<int64_t> factors = getFactors(n);

	// printing all the prime factors
	cout << n << " = ";
	for (int i = 0; i < factors.size(); i++) {
		if (i != 0)
			cout << '*';
		cout << factors[i];
	}
	cout << std::endl;
}


std::vector<int64_t> getFactors(int64_t n) {
	std::vector<int64_t> factors;

	// -1 is a kind of special case
	if (n == -1) {
		factors.push_back(-1);
	}
	else {
		// sign correction
		// and bottom limit value handling
		if (n < 0) {
			factors.push_back(-1);
			// special case since absolute value of (-1)*2^64 is past the upper limit
			if (n == LLONG_MIN) {
				if (n % 2 == 0) {
					n /= 2;
					factors.push_back(2);
				}
			}
			n = abs(n);
		}

		if (isPrime(n))
			factors.push_back(n);
		else {
			// standard prime factors finding
			while (n % 2 == 0) {
				n /= 2;
				factors.push_back(2);
			}
			int i = 3;
			while (n > 1) {
				while (n % i == 0) {
					n /= i;
					factors.push_back(i);
				}
				i++;
			}
		}
	}
	return factors;
}


bool isPrime(int64_t n) {
	n = abs(n);
	for (int i = 2; i <= sqrt(n); i++) {
		if (n % i == 0)
			return false;
	}
	return true;
}
