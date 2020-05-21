using System;
using System.Collections.Generic;
using System.Linq;

namespace Ex_3
{
    // ExampleDb wygenerowana za pomoca polecenia:
    // sqlmetal /server:.\sqlexpress /database:example /language:cs /code:ExampleDb.cs
    internal class Program
    {
        private static List<Data> data = new List<Data>
        {
            new Data
            {
                Imie = "John",
                Nazwisko = "Doe",
                DataUrodzenia = DateTime.Now,
                Miejscowosc = "New York"
            },
            new Data
            {
                Imie = "Jane",
                Nazwisko = "Doe",
                DataUrodzenia = DateTime.Now,
                Miejscowosc = "Los Angeles"
            },
            new Data
            {
                Imie = "John",
                Nazwisko = "Smith",
                DataUrodzenia = DateTime.Now,
                Miejscowosc = "Chicago"
            }
        };

        private static void Main(string[] args)
        {
            try
            {
                using (var context = new Example(@"server=.\sqlexpress;database=example;integrated security=true"))
                {
                    Console.WriteLine("Remove All");
                    Clean(context);

                    Console.WriteLine("Populate");
                    Populate(context);

                    Console.WriteLine("Display All");
                    DisplayAllStudents(context);

                    Console.WriteLine("Turn all Johns into James");
                    JamesifyJohnes(context);

                    Console.WriteLine("Display All");
                    DisplayAllStudents(context);

                    Console.WriteLine("Remove All");
                    Clean(context);
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex.Message);
            }
        }

        private static void DisplayAllStudents(Example context)
        {
            foreach (var student in context.Student)
            {
                Console.WriteLine(
                    string.Format("{0}, {1}, {2}, {3}",
                    student.Imie,
                    student.Nazwisko,
                    student.DataUrodzenia,
                    student.Miejscowosc));
            }
        }

        private static void Populate(Example context)
        {
            int i = 1;
            foreach (var item in data)
            {
                Miejscowosc miejscowosc = new Miejscowosc
                {
                    Nazwa = item.Miejscowosc
                };
                Student student = new Student
                {
                    StudentId = i,
                    Imie = item.Imie,
                    Nazwisko = item.Nazwisko,
                    DataUrodzenia = item.DataUrodzenia
                };
                miejscowosc.Student.Add(student);
                context.Student.InsertOnSubmit(student);
                context.SubmitChanges();
                i++;
            }
        }

        private static void JamesifyJohnes(Example context)
        {
            // nie mozna uzyc porowniania do == "John", gdyz SQL Server nie obsluguje porownan na typach text
            var johns = context.Student.Where(student => student.Imie.StartsWith("John"));

            foreach (var john in johns)
            {
                john.Imie = "James";
                context.SubmitChanges();
            }
        }

        private static void Clean(Example context)
        {
            context.ExecuteCommand("DELETE FROM STUDENT");
            context.ExecuteCommand("DELETE FROM MIEJSCOWOSC");
        }
    }

    class Data
    {
        public string Imie { get; set; }
        public string Nazwisko { get; set; }
        public DateTime DataUrodzenia { get; set; }
        public string Miejscowosc { get; set; }
    }
}