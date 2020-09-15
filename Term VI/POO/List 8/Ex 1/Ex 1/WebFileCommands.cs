using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Net;

namespace Ex_1
{
    public class FtpResourceDownloader : ICommand
    {
        private WebClient _client = new WebClient();
        private Uri _address;
        private string _fileName;

        public FtpResourceDownloader(string address, string fileName)
        {
            _address = new Uri(address);
            _fileName = fileName;
        }

        public void Execute()
        {
            _client.DownloadFile(_address, _fileName);
        }
    }

    public class HttpResourceDownloader : ICommand
    {
        private WebClient _client = new WebClient();
        private Uri _address;
        private string _fileName;

        public HttpResourceDownloader(string address, string fileName)
        {
            _address = new Uri(address);
            _fileName = fileName;
        }

        public void Execute()
        {
            _client.DownloadFile(_address, _fileName);
        }
    }
}
