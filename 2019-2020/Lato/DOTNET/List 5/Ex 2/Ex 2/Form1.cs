using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Threading;

namespace Ex_2
{
    public partial class Form1 : Form
    {
        private FakeDB _db;
        public Form1()
        {
            InitializeComponent();
            _db = new FakeDB();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            // for some reason next instruction doesnt seem to have any effect
            // unless followed by MessageBox.Show(this.toolStripStatusLabel1.Text)
            this.toolStripStatusLabel1.Text = "Loading contacts..."; 
            StartProgressBar();
            this.listView1.Items.Clear();
            var contacts = _db.LoadContacts();
            foreach (Contact contact in contacts)
            {
                ListViewItem item = new ListViewItem(contact.ToString());
                item.SubItems.Add(contact.Name);
                item.SubItems.Add(contact.Email);
                this.listView1.Items.Add(item); 
                UpdateProgressBar();
            }
            FinishProgressBar();
            int contactsFound = contacts.Count();
            this.toolStripStatusLabel1.Text = contactsFound > 0
                ? string.Format("Displaying {0} contacts", contactsFound)
                : "No contacts found";
        }

        private void StartProgressBar()
        {
            this.progressBar2.Visible = true;
            this.progressBar2.Step = 1;
            this.progressBar2.Minimum = 0;
            this.progressBar2.Maximum = 10;
        }

        private void UpdateProgressBar()
        {
            this.progressBar2.PerformStep();
            Thread.Sleep(200);
        }

        private void FinishProgressBar()
        {
            this.progressBar2.Value = this.progressBar2.Maximum;
            Thread.Sleep(2000);
            this.progressBar2.Visible = false;
            this.progressBar2.Value = this.progressBar2.Minimum;
        }
    }
}
