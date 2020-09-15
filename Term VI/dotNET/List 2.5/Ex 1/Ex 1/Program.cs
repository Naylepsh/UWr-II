using System;
using System.Data.OleDb;

namespace Ex_1
{
    internal class Program
    {
        private static void Main(string[] args)
        {
            string x = "Provider = Microsoft.ACE.OLEDB.12.0; Data Source = ..\\..\\Book.xlsx; Extended Properties = \"Excel 12.0 XML;\"";

            using (var connection = new OleDbConnection(x))
            {
                try
                {
                    connection.Open();
                    var command = new OleDbCommand("SELECT * FROM [Arkusz1$]", connection);

                    using (var reader = command.ExecuteReader())
                    {
                        Console.WriteLine("Index | Name | Surname | Term");
                        while (reader.Read())
                        {
                            object index = reader["Index"];
                            object name = reader["Name"];
                            object surname = reader["Surname"];
                            object term = reader["Term"];

                            Console.WriteLine(string.Format("({0}, {1}, {2}, {3})", index, name, surname, term));
                        }
                    }
                }
                catch (Exception ex)
                {
                    Console.WriteLine(ex.Message);
                }
            }

            Console.ReadKey();
        }
    }
}