using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Ex_2;
using System.Collections.Generic;
using System.Linq;
using System.Xml;

namespace DataAccessTest
{
    public class DatabaseColumnValueSummer : DataAccessHandler
    {
        private Dictionary<string, List<int>> _data;
        public int Result { get; set; }

        protected override void Close()
        {
            if (_data == null)
            {
                throw new Exception("Database connection is not established");
            }

            _data = null;
        }

        protected override void Open()
        {
            if (_data != null)
            {
                Close();
            }

            _data = new Dictionary<string, List<int>>();
        }

        protected override void ProcessData()
        {
            if (_data == null)
            {
                throw new Exception("Connection to database is not established");
            }

            Result = _data["column1"].Sum();
        }

        protected override void ReadData()
        {
            if (_data == null)
            {
                throw new Exception("Connection to database is not established");
            }

            _data.Add("column1", new List<int>()
            {
                1,2,3,4
            });
        }
    }

    public class XMLLongestNodeNameFinder : DataAccessHandler
    {
        private XmlDocument _document;
        public int Result { get; set; }

        protected override void Close()
        {
            if (_document == null)
            {
                throw new Exception("Cannot close a document that is not opened");
            }

            _document = null;
        }

        protected override void Open()
        {
            if (_document != null)
            {
                Close();
            }

            _document = new XmlDocument();
        }

        protected override void ProcessData()
        {
            if (_document == null)
            {
                throw new Exception("Cannot process data of a not loaded document");
            }

            var root = _document.FirstChild;

            Result = LongestName(root); 
        }

        private int LongestName(XmlNode node)
        {
            int longest = node.Name.Length;

            foreach (var child in node.ChildNodes)
            {
                longest = Math.Max(longest, LongestName((XmlNode)child));
            }


            if (node.NextSibling != null)
            {
                longest = Math.Max(longest, LongestName(node.NextSibling));
            }

            return longest;
        }

        protected override void ReadData()
        {
            if (_document == null)
            {
                throw new Exception("Cannot read from a file that is not loaded.");
            }

            _document.LoadXml("<?xml version=\"1.0\"?> \n" +
                "<books xmlns=\"http://www.contoso.com/books\"> \n" +
                "  <book genre=\"novel\" ISBN=\"1-861001-57-8\" publicationdate=\"1823-01-28\"> \n" +
                "    <title>Pride And Prejudice</title> \n" +
                "    <price>24.95</price> \n" +
                "  </book> \n" +
                "  <book genre=\"novel\" ISBN=\"1-861002-30-1\" publicationdate=\"1985-01-01\"> \n" +
                "    <title>The Handmaid's Tale</title> \n" +
                "    <price>29.95</price> \n" +
                "  </book> \n" +
                "</books>");
        }
    }

    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void CalculateSumOfValuesOfConreteColumnInDatabase()
        {
            var valuesSummer = new DatabaseColumnValueSummer();

            valuesSummer.Execute();

            Assert.AreEqual(valuesSummer.Result, 1 + 2 + 3 + 4);
        }

        [TestMethod]
        public void FindLongestNameInXMLDocument()
        {
            var longestNameFinder = new XMLLongestNodeNameFinder();

            longestNameFinder.Execute();

            Assert.AreEqual(longestNameFinder.Result, 5);
        }
    }
}
