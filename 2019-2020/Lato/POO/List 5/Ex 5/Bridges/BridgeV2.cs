using System;
using System.Collections.Generic;

namespace Bridge_2
{
    public class Person { }

    public class PersonRegistry
    {
        private INotifier _notifier;

        public PersonRegistry(INotifier notifier)
        {
            _notifier = notifier;
        }

        /// <summary>
        /// Pierwszy stopień swobody - różne wczytywanie
        /// </summary>
        public List<Person> GetPeople()
        {
            return new List<Person>() { new Person() };
        }

        /// <summary>/
        /// Drugi stopień swobody - różne użycie
        /// </summary>
        public void NotifyPeople()
        {
            _notifier.NotifyPeople(GetPeople());
        }
    }

    public interface INotifier
    {
        void NotifyPeople(List<Person> people);
    }

    public class MailNotifier : INotifier
    {
        public void NotifyPeople(List<Person> people)
        {
            foreach (Person person in people)
                Console.WriteLine("Sending mail to {0}", person);
        }
    }

    public class SMSNotifier : INotifier
    {
        public void NotifyPeople(List<Person> people)
        {
            foreach (Person person in people)
                Console.WriteLine("Sending SMS to {0}", person);
        }
    }
}
