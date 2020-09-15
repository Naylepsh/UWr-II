using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;

namespace Ex_8
{
    class Program
    {
        static void Main(string[] args)
        {
            var cultures = new List<string>() { "en", "de", "fr", "ru", "ar", "cs", "pl" };

            foreach (var cultureInfo in cultures.Select(culture => new CultureInfo(culture)))
            {
                Console.WriteLine(cultureInfo.DisplayName);
                DisplayMonths(cultureInfo);
                DisplayDaysOfThWeek(cultureInfo);
                DisplayCurrentDate(cultureInfo);
                Console.WriteLine();
            }

            Console.ReadKey();
        }

        static void DisplayMonths(CultureInfo culture)
        {
            var dateFormat = culture.DateTimeFormat;
            var months = dateFormat.MonthNames.Zip(
                dateFormat.AbbreviatedMonthNames, (full, shrt) => $"{full} ({shrt})");

            Console.WriteLine($"Months: {string.Join(", ", months)}");
        }

        static void DisplayDaysOfThWeek(CultureInfo culture)
        {
            var days = culture.DateTimeFormat.DayNames.Zip(
                culture.DateTimeFormat.ShortestDayNames, (full, shrt) => $"{full} ({shrt})");

            Console.WriteLine($"Days: {string.Join(", ", days)}");
        }

        static void DisplayCurrentDate(CultureInfo culture)
        {
            var date = System.DateTime.Now;
            var day = date.Day;
            var month = culture.DateTimeFormat.GetMonthName(date.Month);
            var year = date.Year;

            Console.WriteLine($"Current date: {day} {month} {year}");
        }
    }

}
