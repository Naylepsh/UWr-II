using System;
using System.Collections.Generic;

namespace Bridge_1
{
    public class Person { }

    public class PersonRegistry
    {
        
        private ILoader _loader;

        public PersonRegistry(ILoader loader)
        {
            _loader = loader;
        }

        /// <summary>
        /// Pierwszy stopień swobody - różne wczytywanie
        /// </summary>
        public List<Person> GetPeople()
        {
            return _loader.GetPeople();
        }

        /// <summary>/
        /// Drugi stopień swobody - różne użycie
        /// </summary>
        public void NotifyPeople()
        {
            foreach ( Person person in _loader.GetPeople() )
                Console.WriteLine( person );
        }
    }

    public interface ILoader
    {
        List<Person> GetPeople();
    }

    public class XMLLoader : ILoader
    {
        public List<Person> GetPeople()
        {
            return new List<Person>() { new Person() };
        }
    }

    public class DBLoader : ILoader
    {
        public List<Person> GetPeople()
        {
            return new List<Person>() { new Person() };
        }
    }
}
