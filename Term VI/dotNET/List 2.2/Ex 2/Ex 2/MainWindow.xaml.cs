using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.Threading;

namespace Ex_2
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        private FakeDB _db;

        public MainWindow()
        {
            InitializeComponent();
            _db = new FakeDB();
        }

        private void ListView_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {

        }

        private void Button_Click(object sender, RoutedEventArgs e)
        {
            Status.Text = "Importing contacts..";  
  
            Thread.Sleep(1000);  
  
            Status.Text = "In Progress...";  
  
            ProgressBar.Value = 0;
  
            Task.Run(() =>  
            {
                var contacts = _db.LoadContacts();

                foreach (var contact in contacts)
                {  
                    Thread.Sleep(200);  
                    this.Dispatcher.Invoke(() => //Use Dispather to Update UI Immediately  
                    {
                        var increment = Math.Ceiling(100 * contacts.Count / ProgressBar.Maximum);
                        ProgressBar.Value += increment;
                        Contacts.Items.Add(contact);
                    });  
                }
                this.Dispatcher.Invoke(() =>
                {
                    ProgressBar.Value = ProgressBar.Maximum;
                    Status.Text = string.Format("Displaying {0} contacts.", Contacts.Items.Count);
                });
            });

            //var contacts = _db.LoadContacts();
            //StartProgressBar(contacts.Count);

            //foreach (var contact in contacts)
            //{
            //    Contacts.Items.Add(contact);
            //    UpdateProgressBar();
            //}

            //FinishProgressBar();
        }

        private void StartProgressBar(int max)
        {
            ProgressBar.Minimum = 0;
            ProgressBar.Maximum = max;
            ProgressBar.Value = 0;
        }

        private void UpdateProgressBar()
        {
            ProgressBar.Value++;
            //Thread.Sleep(200);
            Console.WriteLine(ProgressBar.Value);
        }

        private void FinishProgressBar()
        {
            ProgressBar.Value = ProgressBar.Maximum;
            Thread.Sleep(2000);
            ProgressBar.Value = ProgressBar.Minimum;
        }
    }
}
