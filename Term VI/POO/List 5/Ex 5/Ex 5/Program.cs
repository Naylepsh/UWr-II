using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex_5
{
    class Program
    {
        static void Main(string[] args)
        {
            RunXML();
            RunDB();
            RunMail();
            RunSMS();

            Console.ReadLine();
        }

        static void RunXML()
        {
            Bridge_1.XMLLoader loader = new Bridge_1.XMLLoader();
            Bridge_1.PersonRegistry registry = new Bridge_1.PersonRegistry(loader);
            registry.NotifyPeople();
        }

        static void RunDB()
        {
            Bridge_1.DBLoader loader = new Bridge_1.DBLoader();
            Bridge_1.PersonRegistry registry = new Bridge_1.PersonRegistry(loader);
            registry.NotifyPeople();
        }

        static void RunMail()
        {
            Bridge_2.MailNotifier notifier = new Bridge_2.MailNotifier();
            Bridge_2.PersonRegistry registry = new Bridge_2.PersonRegistry(notifier);
            registry.NotifyPeople();
        }

        static void RunSMS()
        {
            Bridge_2.SMSNotifier notifier = new Bridge_2.SMSNotifier();
            Bridge_2.PersonRegistry registry = new Bridge_2.PersonRegistry(notifier);
            registry.NotifyPeople();
        }
    }
}
