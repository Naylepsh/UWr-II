#pragma once
#include "DateHour.h"
#include <string>
#include <iostream>
class Event
{
private:
	DateHour date;
	std::string eventName;
public:
	Event();
	Event(DateHour & date, std::string eventName);
	Event(DateHour && date, std::string eventName);
	~Event();

	// operators
	bool operator<(const Event & e);
	bool operator<=(const Event & e);
	bool operator==(const Event & e);
	bool operator>(const Event & e);
	bool operator>=(const Event & e);
	friend std::ostream & operator<<(std::ostream & os, const Event & e);
};
