#pragma once
#include <iostream>
static const int days_in_months[2][13] = {
	{ 0,31,28,31,30,31,30,31,31,30,31,30,31 }, // lata zwyk³e
	{ 0,31,29,31,30,31,30,31,31,30,31,30,31 } }; // lata przestêpne

static const int days_since_beginning_of_a_year[2][13] = {
	{ 0,31,59,90,120,151,181,212,243,273,304,334,365 }, // lata zwyk³e
	{ 0,31,60,91,121,152,182,213,244,274,305,335,366 } // lata przestêpne
	};

class Date
{
private:
	int day;
	int month;
	int year;
private:
	static bool isDateCorrect(int day, int month, int year);
	int daysPassedSince0To(const Date & date);
protected:
	static bool isLeapYear(int year);
public:
	Date();
	Date(int day, int month, int year);
	~Date();

	// operators
	virtual int operator-(const Date & date);
	Date & operator++();
	Date & operator+=(int days);
	Date & operator--();
	Date & operator-=(int days);
	friend std::ostream & operator<<(std::ostream & os, const Date & d);

	// getters
	int getDay() const { return day; }
	int getMonth() const { return month; }
	int getYear() const { return year; }
};
