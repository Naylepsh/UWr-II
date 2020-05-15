using System;
using System.Collections.Generic;
using System.Threading;

namespace Ex_4
{
    class Program
    {
        public static Queue<Customer> queue = new Queue<Customer>();
        public static int maxQueueSize = 5;

        static void Main(string[] args)
        {
            Thread thr = new Thread(new ThreadStart(Barber.Work));
            thr.Start();

            Random random = new Random();

            for (var i = 0; i < 10; i++)
            {
                Thread.Sleep(random.Next(500, 2000));

                Customer customer = new Customer();
                customer.Name = i.ToString();
                customer.VisitBarber();
            }
        }
    }

    class Customer
    {
        public string Name { get; set; }

        public static void WakeBarberUp()
        {
            Barber.isSleeping = false;
        }

        public void VisitBarber()
        {
            Console.WriteLine(string.Format("Customer {0} is coming", this.Name));
            lock (Program.queue)
            {
                if (Barber.isSleeping)
                {
                    WakeBarberUp();
                    Enqueue();
                }
                else
                {
                    if (Program.queue.Count == Program.maxQueueSize)
                    {
                        Leave();
                    }
                    else
                    {
                        Enqueue();
                    }
                }
            }
        }

        public void Enqueue()
        {
            Program.queue.Enqueue(this);
        }

        public void Leave()
        {
            Console.WriteLine(string.Format("Customer {0} is leaving", this.Name));
        }
    }

    class Barber
    {
        public static bool isSleeping = false;
        public static bool willWork = true;

        public static void Work()
        {
            Customer customer = null;
            while (willWork)
            {
                lock (Program.queue)
                {
                    if (Program.queue.Count != 0)
                    {
                        customer = Program.queue.Dequeue();
                    }
                }

                if (customer != null)
                {
                    CutHair(customer);
                    customer = null;
                } 
                else
                {
                    GoToSleep();
                }
            }
        }

        public static void CutHair(Customer customer)
        {
            Thread.Sleep(2000);
            Console.WriteLine(string.Format("Cut hair of customer {0}", customer.Name));
        }

        public static void GoToSleep()
        {
            isSleeping = true;
            while (isSleeping)
            {
            }
        }
    }
}
