using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml;
using System.Xml.Linq;
using System.Xml.Schema;
using System.Xml.Serialization;

namespace XML
{
    internal class Program
    {
        public static string ns = "MyData";

        private static void Main(string[] args)
        {
            string pathToValidXml = "../../data.xml";

            // 2.4.2 + 2.4.3
            Console.WriteLine("Reading valid file...");
            Validator(pathToValidXml);
            Console.WriteLine("File read.");

            Console.WriteLine("Reading invalid file...");
            Validator("../../InvalidData.xml");
            Console.WriteLine("File read.");

            // 2.4.4
            Console.WriteLine("Writing students...");
            WriteStudents(pathToValidXml);
            Console.WriteLine("Writing finished.");

            // 2.4.5
            Console.WriteLine("Writing students...");
            WriteStudentsUsingDOM(pathToValidXml);
            Console.WriteLine("Writing finished.");

            // 2.4.7
            Console.WriteLine("-----");
            Console.WriteLine("Enter a surname prefix to search by (case insensitive)");
            string letter = Console.ReadLine();
            var students = StudentsWhoseSurnameStartsWith(pathToValidXml, letter.ToUpper());
            if (students.Count > 0)
            {
                foreach (var student in students)
                {
                    Console.WriteLine(student);
                }
            }
            else
            {
                Console.WriteLine("No such students found");
            }
        }

        private static void Validator(string path)
        {
            string pathToXsd = "../../data.xsd";
            XmlSchemaSet schemas = new XmlSchemaSet();
            schemas.Add(ns, pathToXsd);
            XmlReader studentsReader = XmlReader.Create(path);
            var document = XDocument.Load(studentsReader);

            document.Validate(schemas, new ValidationEventHandler(ValidationHandler));

            studentsReader.Close();
        }

        public static void ValidationHandler(object sender, ValidationEventArgs args)
        {
            Console.WriteLine("*Validation Error*");
            Console.WriteLine("\tSeverity: {0}", args.Severity);
            Console.WriteLine("\tInfo:    {0}", args.Message);
        }

        private static Students LoadStudents(string path)
        {
            Students students;
            XmlSerializer serializer = new XmlSerializer(typeof(Students));
            using (var fs = new FileStream(path, FileMode.Open))
            {
                students = (Students)serializer.Deserialize(fs);
            }

            return students;
        }

        private static void WriteStudents(string path)
        {
            var students = LoadStudents(path);
            foreach (var student in students.Student)
            {
                Console.WriteLine(student.Name);
            }
        }

        private static void WriteStudentsUsingDOM(string path)
        {
            XmlDocument document = new XmlDocument();
            document.Load(path);
            document.Save(Console.Out);
        }

        private static List<StudentPersonalData> StudentsWhoseSurnameStartsWith(string path, string surnamePrefix)
        {
            XDocument document = XDocument.Load(path);

            var students =
                document
                .Root
                .Descendants(AttachNamespace("Student"))
                .Where(student => student
                                .Element(AttachNamespace("Surname"))
                                .Value
                                .StartsWith(surnamePrefix.ToString()))
                ;

            List<StudentPersonalData> studentsPersonalData = students.Select(student => new StudentPersonalData
            {
                Name = student.Element(AttachNamespace("Name")).Value,
                Surname = student.Element(AttachNamespace("Surname")).Value,
                Index = student.Element(AttachNamespace("Index")).Value,
                BirthDate = Convert.ToDateTime(student.Element(AttachNamespace("BirthDate")).Value)
            }).ToList();

            return studentsPersonalData;
        }

        private static string AttachNamespace(string name)
        {
            return $"{{{ns}}}{name}";
        }
    }

    public class StudentPersonalData
    {
        public string Name { get; set; }
        public string Surname { get; set; }
        public string Index { get; set; }
        public DateTime BirthDate { get; set; }

        public override string ToString()
        {
            return $"{Name} {Surname} - {Index} - {BirthDate}";
        }
    }
}