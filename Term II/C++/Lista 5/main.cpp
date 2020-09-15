#include <iostream>
#include "Date.h"
#include "DateHour.h"
#include "Event.h"
#include <chrono>
#include <ctime>
#include <vector>
#include <algorithm>


int main() {
	using namespace std;

	Date date1 = Date(24, 3, 2018);
	cout << "Starting date: " << date1 << endl;
	//Date date2 = Date(29, 2, 2018); // invalid date
	++date1;
	cout << "Increment date once: " << date1 << endl;
	date1 += 70;
	cout << "Date after 70 days: " << date1 << endl;
	cout << "Number of days between: " << date1 << " and " 
		<< Date(24, 3, 2018) << ": " << date1 - Date(24, 3, 2018) << endl;
	cout << "Number of days between 1.1.2018 and 31.12.2017: " << Date(1, 1, 2018) - Date(31, 12, 2017) << endl;
	cout << "Number of days between 1.1.2017 and 1.1.2016: " << Date(1, 1, 2017) - Date(1, 1, 2016) << endl;
	cout << "Number of days between 1.1.2016 and 31.12.2014: " << Date(1, 1, 2016) - Date(31, 12, 2014) << endl;
	cout << "Number of days between 1.1.2015 and 31.12.2013: " << Date(1, 1, 2015) - Date(31, 12, 2013) << endl;
	date1 -= 71;
	cout << "Date 71 days before: " << date1 << endl;

	Date date2 = Date();
	cout << "Current date: " << date2 << endl;

	DateHour dh1 = DateHour();
	cout << "Current date and time: " << dh1 << endl;

	DateHour dh2 = DateHour(25, 3, 2018, 9, 4, 0);
	DateHour dh3 = DateHour(25, 4, 2018, 9, 4, 59);
	cout << "dh2: " << dh2 << endl;
	cout << "dh3: " << dh3 << endl;
	cout << "Difference in time between dh3 and dh2 (in seconds): " << dh3 - dh2 << endl;
	cout << "Difference in time between dh2 and dh3 (in seconds): " << dh2 - dh3 << endl;
	DateHour dh4 = DateHour(25, 3, 2018, 9, 5, 59);
	cout << "dh4: " << dh4 << endl;
	cout << "Difference in time between dh4 and dh2 (in seconds): " << dh4 - dh2 << endl;
	cout << "Difference in time between some day and a day after that (in seconds): " << DateHour(25, 3, 2018, 9, 4, 0)
		- DateHour(26, 3, 2018, 9, 4, 0) << endl;

	std::vector<Event> events = std::vector<Event>();
	events.push_back(Event());
	events.push_back(Event(DateHour(), "Doing absolutely nothing"));
	events.push_back(Event(dh2, "Doing absolutely nothing but few days ago"));
	events.push_back(Event(DateHour(1, 1, 1979, 0, 0, 0), "Doing nothing but long time ago"));
	std::sort(events.begin(), events.end());
	for (auto e : events) {
		cout << e << endl;
	}


	cin.get();
	return 0;
}