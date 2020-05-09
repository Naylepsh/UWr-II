using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Ex_3;
using System.Collections.Generic;
using System.Linq;
using System.Xml;

namespace DataAccessTest
{
    public class DBSumColumnStrategy : IDataAccessStrategy<int>
    {
        private Dictionary<string, List<int>> _data;
        private int _result;

        public void Close()
        {
            if (_data == null)
            {
                throw new Exception("Database connection is not established");
            }

            _data = null;
        }

        public int GetResult()
        {
            return _result;
        }

        public void Open()
        {
            if (_data != null)
            {
                Close();
            }

            _data = new Dictionary<string, List<int>>();
        }

        public void ProcessData()
        {
            if (_data == null)
            {
                throw new Exception("Connection to database is not established");
            }

            _result = _data["column1"].Sum();
        }

        public void ReadData()
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

    public class XMLFindLongestNodeNameStrategy : IDataAccessStrategy<int>
    {
        private XmlDocument _document;
        private int _result;

        public void Close()
        {
            if (_document == null)
            {
                throw new Exception("Cannot close a document that is not opened");
            }

            _document = null;
        }

        public void Open()
        {
            if (_document != null)
            {
                Close();
            }

            _document = new XmlDocument();
        }

        public void ProcessData()
        {
            if (_document == null)
            {
                throw new Exception("Cannot process data of a not loaded document");
            }

            var root = _document.FirstChild;

            _result = LongestName(root); 
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

        public void ReadData()
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

        public int GetResult()
        {
            return _result;
        }
    }

    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void SumInConcreteColumnInDatabase()
        {
            var strategy = new DBSumColumnStrategy();
            var dataAccessHandler = new DataAccessHandler<int>(strategy);

            dataAccessHandler.Execute();

            Assert.AreEqual(dataAccessHandler.Result, 1 + 2 + 3 + 4);
        }

        [TestMethod]
        public void FindLongestNodeNameInXMLDocument()
        {
            var strategy = new XMLFindLongestNodeNameStrategy();
            var dataAccessHandler = new DataAccessHandler<int>(strategy);

            dataAccessHandler.Execute();

            Assert.AreEqual(dataAccessHandler.Result, 5);
        }
    }
}
