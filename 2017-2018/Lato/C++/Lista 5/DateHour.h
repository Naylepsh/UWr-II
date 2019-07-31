#pragma once
#include "Date.h"
class DateHour : public Date
{
protected:
	int second;
	int minute;
	int hour;
private:
	bool isHourCorrect(int hour, int minute, int second);
public:
	DateHour();
	DateHour(int day, int month, int year, int hour, int minute, int second);
	~DateHour();

	// operators
	DateHour & operator++();
	DateHour & operator+=(int seconds);
	DateHour & operator--();
	DateHour & operator-=(int seconds);
	friend std::ostream & operator<<(std::ostream & os, const DateHour dh);

	// getters
	int getSecond() const { return second; }
	int getMinute() const { return minute; }
	int getHour() const { return hour; }
};

// operators
long long operator-(const DateHour & dh1, const DateHour & dh2);
bool operator<(const DateHour & dh1, const DateHour & dh2);
bool operator==(const DateHour & dh1, const DateHour & dh2);

// helpers
static long long timeDifference(const DateHour & dh1, const DateHour & dh2);