using System.IO;

namespace Ex_1
{
    public class FileFiller : ICommand
    {
        private string _fileName;

        public FileFiller(string fileName)
        {
            _fileName = fileName;
        }

        public void Execute()
        {
            int randomLength = RandomGenerator.RandomInt() % 100;
            if (randomLength <= 0)
            {
                randomLength = 1;
            }

            string randomString = RandomGenerator.RandomString(randomLength);
            using (var fileStream = File.CreateText(_fileName))
            {
                fileStream.Write(randomString);
            }
        }
    }

    public class FileCopier : ICommand
    {
        private string _source;
        private string _destination;

        public FileCopier(string source, string destination)
        {
            _source = source;
            _destination = destination;
        }

        public void Execute()
        {
            File.Copy(_source, _destination);
        }
    }
}
