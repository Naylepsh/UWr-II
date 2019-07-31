#include <iostream>
#include <string>

#include "Constant.h"
#include "Number.h"
#include "Variable.h"
#include "Operator1Arg.h"
#include "Operator2Arg.h"

int main() {
	using namespace std;

	// very basic tests
	Number number = Number(7.5);
	cout << "Number = " << number.toString() << endl;

	Expression * pi = new Pi();
	cout << pi->toString() << " = " << pi->eval() << endl;

	Expression * e = new E();
	cout << e->toString() << " = " << e->eval() << endl;

	Expression * fi = new Fi();
	cout << fi->toString() << " = " << fi->eval() << endl;

	Expression * sin1 = new Sin(new Number(0));
	cout << sin1->toString() << " = " << sin1->eval() << endl;
	Expression * sin2 = new Sin(pi);
	cout << sin2->toString() << " = " <<  sin2->eval() << endl;
	Expression * sin3 = new Sin(new Divide(pi, new Number(4)));
	cout << sin3->toString() << " = " << sin3->eval() << endl;

	Expression * abs1 = new Abs(sin3);
	cout << abs1->toString() << " = " << abs1->eval() << endl;

	Expression * sum1 = new Add(sin3, abs1);
	cout << sum1->toString() << " = " << sum1->eval() << endl;

	Expression * sub1 = new Subtract(sin3, abs1);
	cout << sub1->toString() << " = " << sub1->eval() << endl;

	Expression * mult1 = new Multiply(new Number(5), new Number(6));
	cout << mult1->toString() << " = " << mult1->eval() << endl;

	Expression * div1 = new Divide(new Number(10), new Number(2));
	cout << div1->toString() << " = " << div1->eval() << endl;

	Expression * log1 = new Log(new Number(16), new Number(2));
	cout << log1->toString() << " = " << log1->eval() << endl;

	Expression * exp = new Exp(new Number(1));
	cout << exp->toString() << " = " << exp->eval() << endl;

	Expression * inv = new Inverse(new Number(2));
	cout << inv->toString() << " = " << inv->eval() << endl;

	Expression * ln = new Ln(new Number(1));
	cout << ln->toString() << " = " << ln->eval() << endl;

	Expression * opp = new Opposite(new Number(2));
	cout << opp->toString() << " = " << opp->eval() << endl;

	Expression * mod = new Mod(new Number(6), new Number(4.5));
	cout << mod->toString() << " = " << mod->eval() << endl;

	/******** more 'advanced' tests ********/
	cout << endl;
	// (((x-1)*x)/2)
	Expression * expr1 = 
		new Divide(
			new Multiply(
				new Subtract(
					new Variable("x"),
					new Number(1)),
				new Variable("x")),
			new Number(2));
	cout << expr1->toString() << endl;

	// 2 +((x * 7) - ((y * 3 + 5)))
	Expression * expr2 =
		new Add(
			new Number(2),
			new Subtract(
				new Multiply(
					new Variable("x"),
					new Number(7)),
				new Add(
					new Multiply(
						new Variable("y"),
						new Number(3)),
					new Number(5))));
	cout << expr2->toString() << endl;

	// (3 + 5) / (2 + x * 7)
	Expression * expr3 =
		new Divide(
			new Add(
				new Number(3),
				new Number(5)),
			new Add(
				new Number(2),
				new Multiply(
					new Variable("x"),
					new Number(7))));
	cout << expr3->toString() << endl;

	//cos((x + 1)*x) / e ^ x ^ 2
	Expression * expr4 =
		new Divide(
			new Cos(
				new Multiply(
					new Add(
						new Variable("x"),
						new Number(1)),
					new Variable("x"))),
			new Pow(
				new E(),
				new Pow(
					new Variable("x"),
					new Number(2))));
	cout << expr4->toString() << endl;

	//add_valuation("x", 0);
	//add_valuation("y", 1);
	Variable * for_valuation = new Variable("doesn't matter");
	for_valuation->add_valuation("x", 0);
	for_valuation->add_valuation("y", 1);
	for (double x = 0; x <= 1; x += 0.1) {
		//change_valuation("x", x);
		for_valuation->change_valuation("x", x);
		cout << "x = " << x << ": " << expr1->eval() << " " << expr2->eval()
			<< " " << expr3->eval() << " " << expr3->eval() << endl;
	}

	delete for_valuation;
	cin.get();
	return 0;
}