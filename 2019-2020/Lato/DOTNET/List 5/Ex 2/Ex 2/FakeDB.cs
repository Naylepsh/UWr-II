using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex_2
{
    public class FakeDB
    {
        private List<Contact> _contacts;

        public FakeDB()
        {
            _contacts = new List<Contact>()
            {
                new Contact("a", "a@mail.com"),
                new Contact("b", "b@mail.com"),
                new Contact("c", "c@mail.com"),
                new Contact("d", "d@mail.com"),
                new Contact("e", "e@mail.com"),
                new Contact("f", "f@mail.com"),
                new Contact("g", "g@mail.com"),
                new Contact("h", "h@mail.com"),
                new Contact("i", "i@mail.com"),
                new Contact("j", "j@mail.com"),
                new Contact("k", "k@mail.com"),
            };
        }

        public List<Contact> LoadContacts()
        {
            return _contacts;
        }
    }
}
