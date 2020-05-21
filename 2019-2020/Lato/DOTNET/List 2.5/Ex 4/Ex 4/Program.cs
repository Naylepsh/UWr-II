using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex_4
{
    class Program
    {
        private class Data
        {
            public string Name { get; set; }
            public string Surname { get; set; }
            public DateTime BirthDate { get; set; }
            public string Town { get; set; }
        }

        private static List<Data> data = new List<Data>
        {
            new Data
            {
                Name = "John",
                Surname = "Doe",
                BirthDate = DateTime.Now,
                Town = "New York"
            },
            new Data
            {
                Name = "Jane",
                Surname = "Doe",
                BirthDate = DateTime.Now,
                Town = "Los Angeles"
            },
            new Data
            {
                Name = "John",
                Surname = "Smith",
                BirthDate = DateTime.Now,
                Town = "Chicago"
            }
        };


        static void Main(string[] args)
        {
            try
            {
                using (var context = new TownStudentContext())
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

            Console.ReadKey();
        }

        private static void DisplayAllStudents(TownStudentContext context)
        {
            foreach (var student in context.Students)
            {
                Console.WriteLine(
                    string.Format("{0}, {1}, {2}, {3}",
                    student.Name,
                    student.Surname,
                    student.BirthDate,
                    student.Town.Name));
            }
        }

        private static void Populate(TownStudentContext context)
        {
            foreach (var item in data)
            {
                Town town = new Town
                {
                    Name = item.Town
                };
                Student student = new Student
                {
                    Name = item.Name,
                    Surname = item.Surname,
                    BirthDate = item.BirthDate
                };
                student.Town = town;
                context.Towns.Add(town);
                context.Students.Add(student);
                context.SaveChanges();
            }
        }

        private static void JamesifyJohnes(TownStudentContext context)
        {
            var johns = context.Students.Where(student => student.Name.StartsWith("John"));

            foreach (var john in johns)
            {
                john.Name = "James";
            }

            context.SaveChanges();
        }

        private static void Clean(TownStudentContext context)
        {
            context.Database.ExecuteSqlCommand("DELETE FROM STUDENTS");
            context.Database.ExecuteSqlCommand("DELETE FROM TOWNS");
        }
    }

    public class TownStudentContext : DbContext
    {
        public TownStudentContext() : base("cs")
        {

        }

        public DbSet<Town> Towns { get; set; }
        public DbSet<Student> Students { get; set; }
    }

    public class Student
    {
        public int ID { get; set; }
        public string Name { get; set; }
        public string Surname { get; set; }
        public DateTime BirthDate { get; set; }
        public Town Town { get; set; }
    }

    public class Town
    {
        public int ID { get; set; }
        public string Name { get; set; }
        public ICollection<Student> Students { get; set; }
    }
}
