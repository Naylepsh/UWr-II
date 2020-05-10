using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Net;
using System.Net.Http;

namespace Ex_7
{
    public partial class Form1 : Form
    {
        private static readonly HttpClient _httpClient = new HttpClient();
        private static readonly WebClient _webClient = new WebClient();
        private string _data;
        private string _url;

        public Form1()
        {
            InitializeComponent();
        }

        private async void button1_Click(object sender, EventArgs e)
        {
            try
            {
                _data = await GetResourceAsync();
                MessageBox.Show("Loaded data");
                this.textBox1.Text = _data;
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message);
            }
        }

        private async Task<string> GetResourceAsync()
        {
            var response = await _httpClient.GetAsync(_url);
            response.EnsureSuccessStatusCode();
            return await response.Content.ReadAsStringAsync();
        }

        private string GetResourceSync()
        {
            return _webClient.DownloadString(_url);
        }

        private void button2_Click(object sender, EventArgs e)
        {
            try
            {
                _data = GetResourceSync();
                MessageBox.Show("Loaded data");
                this.textBox1.Text = _data;
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message);
            }
        }

        private void button3_Click(object sender, EventArgs e)
        {
            string message;

            if (_data == null)
            {
                message = "No HTTP response yet";
            }
            else
            {
                message = string.Format("Reponse contains {0} characters", _data.Length);
            }

            MessageBox.Show(message);
        }

        private void textBox2_TextChanged(object sender, EventArgs e)
        {
            _url = ((TextBox)sender).Text;
        }
    }
}
