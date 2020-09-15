#include "Event.h"



Event::Event()
{
	date = DateHour();
	eventName = "N/A";
}

Event::Event(DateHour & date, std::string eventName)
{
	this->date = date;
	this->eventName = eventName;
}

Event::Event(DateHour && date, std::string eventName)
{
	this->date = date;
	this->eventName = eventName;
}


Event::~Event()
{
}

bool Event::operator<(const Event & e)
{
	return date < e.date;
}

bool Event::operator<=(const Event & e)
{
	return date < e.date || date == e.date;
}

bool Event::operator==(const Event & e)
{
	return date == e.date;
}

bool Event::operator>(const Event & e)
{
	return e.date < date;
}

bool Event::operator>=(const Event & e)
{
	return e.date < date || e.date == date;
}

std::ostream & operator<<(std::ostream & os, const Event & e)
{
	return os << e.eventName << " at: " << e.date;
}
