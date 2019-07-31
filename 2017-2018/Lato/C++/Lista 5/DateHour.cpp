#include "DateHour.h"
#include <chrono>
#include <iostream>


bool DateHour::isHourCorrect(int hour, int minute, int second)
{
	return (0 <= hour && hour < 24 &&
		0 <= minute && minute < 60 &&
		0 <= second && second < 60);
}

DateHour::DateHour() : Date()
{
	using namespace std::chrono;

	system_clock::time_point today = system_clock::now();
	time_t tt = system_clock::to_time_t(today);

	second = tt % 60;
	minute = (tt / 60) % 60;
	hour = (tt / 60 / 60) % 24;
}

DateHour::DateHour(int day = 1, int month = 1, int year = 0, int hour = 0, int minute = 0, int second = 0) :Date(day, month, year)
{
	if (isHourCorrect(hour, minute, second)) {
		this->hour = hour;
		this->minute = minute;
		this->second = second;
	}
	else
		throw std::invalid_argument("Invalid hour");
}


DateHour::~DateHour()
{
}

DateHour & DateHour::operator++()
{
	++second;
	if (second >= 60) {
		second = 0;
		++minute;
	}
	if (minute >= 60) {
		minute = 0;
		++hour;
	}
	if (hour >= 24) {
		hour = 0;
		Date::operator++();
	}

	return *this;
}

DateHour & DateHour::operator+=(int seconds)
{
	for (int i = 0; i < seconds; i++) {
		operator++();
	}
	
	return *this;
}

DateHour & DateHour::operator--()
{
	--second;
	if (second < 0) {
		second = 59;
		--minute;
	}
	if (minute < 0) {
		minute = 59;
		--hour;
	}
	if (hour < 0) {
		hour = 23;
		Date::operator--();
	}
	
	return *this;
}

DateHour & DateHour::operator-=(int seconds)
{
	for (int i = 0; i < seconds; i++) {
		operator--();
	}
	return *this;
}

std::ostream & operator<<(std::ostream & os, const DateHour dh)
{
	return os << dh.getHour() << ":" << dh.getMinute() << ":" << dh.getSecond()
		<< " " << dh.getDay() << "." << dh.getMonth() << "." << dh.getYear();
}

long long operator-(const DateHour & dh1, const DateHour & dh2)
{
	// Find later date and substract accordingly
	if (dh1 < dh2)
		return timeDifference(dh2, dh1);
	return timeDifference(dh1, dh2);
}

bool operator<(const DateHour & dh1, const DateHour & dh2)
{
	return timeDifference(dh1, dh2) < 0;
}

bool operator==(const DateHour & dh1, const DateHour & dh2)
{
	return timeDifference(dh1, dh2) == 0;
}

long long timeDifference(const DateHour & dh1, const DateHour & dh2)
{
	int days = Date(dh1.getDay(), dh1.getMonth(), dh1.getYear()) -
		Date(dh2.getDay(), dh2.getMonth(), dh2.getYear());
	if (days == 0) {
		return dh2.getHour() * 60 * 60 + dh2.getMinute() * 60 + dh2.getSecond() -
			dh1.getHour() * 60 * 60 - dh1.getMinute() * 60 - dh1.getSecond();
	}
	else {
		return (days - 1) * 24 * 60 * 60 // all the days in seconds
			+ 24 * 60 * 60 - dh1.getHour() * 60 * 60 - dh1.getMinute() * 60 - dh1.getSecond() // remaining seconds to the end of a day
			+ dh2.getHour() * 60 * 60 + dh2.getMinute() * 60 + dh2.getSecond(); // seconds of the second date since the beginning of a day
	}
}

