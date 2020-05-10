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

namespace Ex_1
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
        }

        private void Anuluj_Click(object sender, RoutedEventArgs e)
        {
            Application.Current.Shutdown();
        }

        private void Akceptuj_Click(object sender, RoutedEventArgs e)
        {
            List<string> content = new List<string>();

            content.Add(Nazwa.Text);
            content.Add(Adres.Text);
            content.Add(CyklNauki.Text);

            if (CzyDzienne.IsChecked.Value)
            {
                content.Add(CzyDzienne.Content.ToString());
            }
            
            if (CzyUzupelniajace.IsChecked.Value)
            {
                content.Add(CzyUzupelniajace.Content.ToString());
            }

            MessageBox.Show(string.Join("\n", content.Where(str => str.Length > 0)));
        }
    }
}
