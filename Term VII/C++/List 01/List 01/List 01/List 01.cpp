// List 01.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include <iostream>
#include <set>
#include <math.h>

using namespace std;

// To allow trigraphs in visual studio do the following:
// Project -> <Project-Name> Properties -> C/C++ -> Command Line
// Then, in Additional Options include /Zc:trigraphs
void displayTrigraph() {
    cout << "??-" << endl;
}

void displayRawString() {
    cout <<
        R"(Instytut Informatyki Uniwersytetu Wrocławskiego
Fryderyka Joliot - Curie 15, 
50-383 Wrocław
C:\Program Files
(")" "()" << endl;
}

void aliasAndForEach() {
    using SetAlias = set <string>;
    SetAlias xs = { "foo", "bar", "baz" };
    for (auto x : xs) {
        cout << x << " ";
    }
    cout << endl;
}

enum class Name : uint16_t { John, Jane, Chad, Stacy, Albert };

string nameToString(Name name) {
    switch (name) {
    case Name::Albert: return "Albert";
    case Name::Chad: return "Chad";
    case Name::Jane: return "Jane";
    case Name::John: return "John";
    case Name::Stacy: return "Stacy";
	default: return "";
    }
}

void greet(string message, Name name) {
    cout << nameToString(name) << ", " << message << endl;
}

auto lucasNumber(uint32_t n) {
    if (n == 0) return 2;
    if (n == 1) return 1;
    return lucasNumber(n - 1) + lucasNumber(n - 2);
}

bool areCloseEnough(const double x, const double y, double epsilon = 0.0001) {
    if (x == y) return true;
    if (-epsilon < x && x < y) return true;
    if (y < x && x < epsilon) return true;
    return false;
}

void solveQuadraticEquation(const double a, const double b, const double c) {
    const double delta = b * b - 4 * a * c;
    if (areCloseEnough(delta, 0)) {
        cout << "x0 = " << -b / (2.0 * a);
    }
    else if (delta > 0) {
        const double squaredDelta = sqrt(delta);
        const double x1 = (-b + squaredDelta) / (2.0 * a);
        const double x2 = (-b - squaredDelta) / (2.0 * a);
        cout << "x1 = " << x1 << ", x2 = " << x2;
    }
    else {
        cout << "No real number solution";
    }
    cout << endl;
}

void br() {
    cout << endl;
}

int main()
{
    // Ex 1
    displayTrigraph();
    br();
    // Ex 2
    displayRawString();
    br();
    // Ex 3
    aliasAndForEach();
    br();
    // Ex 4
    greet("so glad to see you!", Name::Chad);
    br();
    // Ex 5
    cout << lucasNumber(8) << endl;
    br();
    // Ex 6
    solveQuadraticEquation(1, 2, 1);
    br();
}

