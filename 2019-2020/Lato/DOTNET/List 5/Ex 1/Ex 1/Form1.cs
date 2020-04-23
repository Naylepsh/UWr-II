using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace Ex_1
{
    public partial class Form1 : Form
    {
        private string _name;
        private string _address;
        private string _studiesType;
        private string _fullTime;
        private string _supplementary;

        public Form1()
        {
            InitializeComponent();
        }

        private string GetInputValue(Control input)
        {
            return input.Text;
        }

        private void button1_Click(object sender, EventArgs e)
        {
            string data = string.Join("\n", new List<string>()
            {
                _name, _address, _studiesType, _fullTime, _supplementary
            });
            MessageBox.Show(data);
        }

        private void button2_Click(object sender, EventArgs e)
        {
            Application.Exit();
        }

        private void textBox1_TextChanged(object sender, EventArgs e)
        {
            _name = GetInputValue((TextBox)sender);
        }

        private void textBox2_TextChanged(object sender, EventArgs e)
        {
            _address = GetInputValue((TextBox)sender);
        }

        private void comboBox1_SelectedIndexChanged(object sender, EventArgs e)
        {
            _studiesType = GetInputValue((ComboBox)sender);
        }

        private void checkBox1_CheckedChanged(object sender, EventArgs e)
        {
            _fullTime = GetInputValue((CheckBox)sender);
        }

        private void checkBox2_CheckedChanged(object sender, EventArgs e)
        {
            _supplementary = GetInputValue((CheckBox)sender);
        }
    }
}
