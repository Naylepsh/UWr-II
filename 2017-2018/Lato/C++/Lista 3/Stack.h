#pragma once
#include <string>
class Stack
{
private:
	std::string * stack;
	int capacity;
	int how_many = 0;
public:
	Stack();
	Stack(int capacity);
	Stack(std::initializer_list<std::string> il);
	Stack(const Stack & stack);
	Stack(Stack && stack);
	~Stack();

	void add(std::string s);
	std::string pop();
	std::string check();
	int size();
	int cap();

	Stack & operator=(const Stack & stack);
	Stack & operator=(Stack && stack);
};

