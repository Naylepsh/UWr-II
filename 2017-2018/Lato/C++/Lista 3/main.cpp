#include <iostream>
#include "Stack.h"

void initStackMenu();
void stackOptions();

int main() {
	using std::cout;
	using std::endl;
	using std::cin;

	// initial stack creation
	Stack * stack;
	initStackMenu();
	char option;
	cin >> option;
	switch (option) {
	case '1':	// stack with a set capacity
	{
		cout << "Enter max capacity: ";
		int cap;
		cin >> cap;
		if (cap <= 0) {
			std::cerr << "Invalid argument. Max capacity set to 1\n";
			cap = 1;
		}
		stack = new Stack(cap);
		break;
	}
	case '2':	// default stack
	{
		stack = new Stack();
		break;
	}
	case '3':	// stack with pre-set strings -- totally unnecessary mumbo-jumbo
	{
		Stack temp, temp2;
		temp = Stack(Stack(Stack({ "one", "two", "three" })));
		temp2 = temp;
		stack = new Stack(temp2);
		break;
	}
	default:
	{
		std::cerr << "Invalid argument. Creating default stack\n";
		stack = new Stack();
		break;
	}
	}

	// operate on the stack
	stackOptions();
	cin >> option;
	while (option != '0') {
		try {
			switch (option) {
			case '1':
			{
				cout << "Enter a string: ";
				std::string s;
				cin >> s;
				stack->add(s);
				break;
			}
			case '2':
			{
				cout << "Removed '" << stack->pop() << "' from the stack\n";
				break;
			}
			case '3':
			{
				cout << "Current top value: " << stack->check() << endl;
				break;
			}
			case '4':
			{
				cout << "Current size of the stack: " << stack->size() << endl;
				break;
			}
			case '5':
			{
				cout << "This stack's capacity: " << stack->cap() << endl;
				break;
			}
			case '6':	// showing all string from the stack
			{
				// make a copy of that stack and pop until it's empty
				Stack * temp = new Stack(*stack);
				for (int i = 0; i < stack->size(); i++)
					cout << "\t" << temp->pop() << endl;
				delete temp;
				break;
			}
			default:
				cout << "Invalid argument. Try again.\n";
			}
		}
		catch (std::out_of_range oor) {
			std::cerr << "Operation canceled. " << oor.what() << endl;
		}
		stackOptions();
		cin >> option;
	}

	delete stack;
	return 0;
}

void initStackMenu()
{
	using std::cout;
	using std::endl;

	cout << "Press <1> to make a stack with set CAPACITY.\n";
	cout << "Press <2> to make a default stack.\n";
	cout << "Press <3> to make a stack with already set strings in it.\n";
}

void stackOptions()
{
	using std::cout;

	cout << "Press <1> to add to the stack.\n";
	cout << "Press <2> to remove the top value on this stack.\n";
	cout << "Press <3> to check the top value on this stack.\n";
	cout << "Press <4> to check the current size of this stack.\n";
	cout << "Press <5> to check the capacity of this stack.\n";
	cout << "Press <6> to check all values in this stack.\n";
	cout << "Press <0> to exit\n";
}

