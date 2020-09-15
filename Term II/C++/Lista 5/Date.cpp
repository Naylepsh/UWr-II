#include "Date.h"
#include <chrono>
#include <ctime>
#include <time.h>


bool Date::isDateCorrect(int day, int month, int year)
{
	int index = isLeapYear(year);
	return (0 < month && month <= 12 && 
		0 < day && day <= days_in_months[index][month]);
}

int Date::daysPassedSince0To(const Date & date)
{
	int leap_years = date.year / 4 - date.year / 100 + date.year / 400;
	int days_till_date_year = (date.year - 1) * 365 + leap_years;
	int days = days_till_date_year + days_since_beginning_of_a_year[isLeapYear(date.year)][date.month - 1] + date.day;
	return days;
}

bool Date::isLeapYear(int year)
{
	return ((year % 4 == 0 && year % 100 != 0) || year % 400 == 0);
}

Date::Date()
{
	// get current time
	using namespace std::chrono;
	system_clock::time_point today = system_clock::now();
	time_t tt = system_clock::to_time_t(today);

	// set date to 01.01.1970
	this->day = 1;
	this->month = 1;
	this->year = 1970;

	// increment date by the number of days passed since 1970
	int days = (tt / 60 / 60 / 24);
	for (int i = 0; i < days; i++) {
		operator++();
	}
}

Date::Date(int day, int month, int year)
{
	if (isDateCorrect(day, month, year)) {
		this->day = day;
		this->month = month;
		this->year = year;
	}
	else
		throw std::invalid_argument("Invalid date");
}


Date::~Date()
{
}


int Date::operator-(const Date & date)
{
	return daysPassedSince0To(*this) - daysPassedSince0To(date);
}

Date & Date::operator++()
{
	++day;
	if (day > days_in_months[isLeapYear(year)][month]) {
		day = 1;
		++month;
	}
	if (month > 12) {
		month = 1;
		++year;
	}
	return *this;
}

Date & Date::operator+=(int days)
{
	for (int i = 0; i < days; i++)
		operator++();
	return *this;
}

Date & Date::operator--()
{
	--day;
	if (day < 1) {
		--month;
		day = days_in_months[isLeapYear(year)][month];
	}
	if (month < 0) {
		month = 12;
		--year;
	}
	return *this;
}

Date & Date::operator-=(int days)
{
	for (int i = 0; i < days; i++)
		operator--();
	return *this;
}

std::ostream & operator<<(std::ostream & os, const Date & d)
{
	return os << d.getDay() << "." << d.getMonth() << "." << d.getYear();
}
