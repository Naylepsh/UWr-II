#include "Stack.h"
#include <iostream>

Stack::Stack() : Stack(1)
{
}

Stack::Stack(int capacity)
{
	stack = new std::string[capacity];
	this->capacity = capacity;
}

Stack::Stack(std::initializer_list<std::string> il)
{
	std::cout << "Using (initializer_list) constructor.\n";
	capacity = il.size();
	stack = new std::string[capacity];
	for (auto s : il)
		add(s);
}

Stack::Stack(const Stack & stack)
{
	std::cout << "Using copy constructor\n";
	capacity = stack.capacity;
	how_many = stack.how_many;
	this->stack = new std::string[capacity];
	for (int i = 0; i < stack.how_many; i++)
		this->stack[i] = stack.stack[i];
}

/* copying stack in stack-like way
Stack::Stack(Stack * stack)
{
	// init new stack
	capacity = stack->capacity;
	how_many = 0;
	this->stack = new std::string[capacity];

	// create a temporary array to hold items
	const int Size = stack->size();
	std::string * temp = new std::string[Size];

	// move all items one by one to temporary array
	// (they will be in reversed order
	for (int i = 0; i < Size; i++) {
		temp[i] = stack->pop();
	}
	// now, move items back to the original stack
	// and to new stack
	for (int i = Size - 1; i >= 0; i--) {
		stack->add(temp[i]);
		add(temp[i]);
	}
	
	delete[] temp;
}
*/

Stack::Stack(Stack && stack)
{
	std::cout << "Using (Stack &&) constructor.\n";
	this->stack = std::move(stack.stack);
	capacity = stack.capacity;
	how_many = stack.how_many;
	stack.how_many = 0;
	stack.stack = nullptr;
}


Stack::~Stack()
{
	delete[] stack;
}

void Stack::add(std::string s)
{
	if (how_many == capacity)
		throw std::out_of_range("Stack is full.");
	else
		stack[how_many++] = s;
}

std::string Stack::pop()
{
	if (how_many == 0)
		throw std::out_of_range("Stack is empty.");
	return stack[--how_many];
}

std::string Stack::check()
{
	if (how_many == 0)
		throw std::out_of_range("Stack is empty.");
	return stack[how_many - 1];
}

int Stack::size()
{
	return how_many;
}

int Stack::cap()
{
	return capacity;
}

Stack & Stack::operator=(const Stack & stack)
{
	std::cout << "Using operator=(const Stack &)\n";
	capacity = stack.capacity;
	how_many = stack.how_many;
	delete[] this->stack;
	this->stack = new std::string[capacity];
	for (int i = 0; i < stack.how_many; i++)
		this->stack[i] = stack.stack[i];
	return *this;
}

Stack & Stack::operator=(Stack && stack)
{
	std::cout << "Using operator=(Stack &&)\n";
	capacity = stack.capacity;
	how_many = stack.how_many;
	delete[] this->stack;
	this->stack = stack.stack;
	stack.stack = nullptr;
	stack.how_many = 0;
	return *this;
}

