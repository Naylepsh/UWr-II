using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Ex_1;
using System.IO;
using System.Threading;

namespace CommandsTest
{
    [TestClass]
    public class UnitTest1
    {
        private bool shouldCleanupAfter = true;

        [TestMethod]
        public void CreateFile()
        {
            string fileName = "foo.txt";
            Invoker invoker = new Invoker();
            FileFiller filler = new FileFiller(fileName);

            invoker.Execute(filler);

            using (var fileStream = File.OpenText(fileName))
            {
                string content = fileStream.ReadToEnd();
                Assert.IsNotNull(content);
                Assert.IsTrue(content.Length > 0);
            }

            if (shouldCleanupAfter)
            {
                File.Delete(fileName);
            }
        }

        [TestMethod]
        public void CopyFile()
        {
            string source = "foo.txt";
            string destination = "bar.txt";
            Invoker invoker = new Invoker();
            FileFiller filler = new FileFiller(source);
            FileCopier copier = new FileCopier(source, destination);

            invoker.Execute(filler);
            invoker.Execute(copier);

            using (var sourceStream = File.OpenText(source))
            {
                using (var destinationStream = File.OpenText(destination))
                {
                    string sourceContent = sourceStream.ReadToEnd();
                    string destinationContent = destinationStream.ReadToEnd();

                    Assert.IsNotNull(sourceContent);
                    Assert.IsNotNull(destinationContent);
                    Assert.AreEqual(sourceContent, destinationContent);
                }
            }

            if (shouldCleanupAfter)
            {
                File.Delete(source);
                File.Delete(destination);
            }
        }

        [TestMethod]
        public void DownloadThroughHttp()
        {
            string uri = "https://raw.githubusercontent.com/microsoft/vscode/master/README.md";
            string fileName = "README.md";
            var ftpDownloader = new FtpResourceDownloader(uri, fileName);
            Invoker invoker = new Invoker();

            invoker.Execute(ftpDownloader);

            using (var fileStream = File.OpenText(fileName))
            {
                string content = fileStream.ReadToEnd();
                Assert.IsNotNull(content);
                Assert.IsTrue(content.Length > 0);
            }

            if (shouldCleanupAfter)
            {
                File.Delete(fileName);
            }
        }

        [TestMethod]
        public void DownloadThroughFtp()
        {
            string uri = "ftp://ftp.freebsd.org/pub/FreeBSD/README.TXT";
            string fileName = "README.txt";
            var ftpDownloader = new FtpResourceDownloader(uri, fileName);
            Invoker invoker = new Invoker();

            invoker.Execute(ftpDownloader);

            using (var fileStream = File.OpenText(fileName))
            {
                string content = fileStream.ReadToEnd();
                Assert.IsNotNull(content);
                Assert.IsTrue(content.Length > 0);
            }

            if (shouldCleanupAfter)
            {
                File.Delete(fileName);
            }
        }

        [TestMethod]
        public void ThreadTest()
        {
            bool isThreaded = true;
            string fileName1 = "foo.txt";
            string fileName2 = "bar.txt";
            string fileName3 = "baz.txt";
            var filler1 = new FileFiller(fileName1);
            var filler2 = new FileFiller(fileName2);
            var filler3 = new FileFiller(fileName3);
            var invoker = new Invoker(isThreaded);

            invoker.Execute(filler1);
            invoker.Execute(filler2);
            invoker.Execute(filler3);

            Thread.Sleep(1000);

            using (var fileStream = File.OpenText(fileName1))
            {
                string content = fileStream.ReadToEnd();
                Assert.IsNotNull(content);
                Assert.IsTrue(content.Length > 0);
            }
            using (var fileStream = File.OpenText(fileName2))
            {
                string content = fileStream.ReadToEnd();
                Assert.IsNotNull(content);
                Assert.IsTrue(content.Length > 0);
            }
            using (var fileStream = File.OpenText(fileName3))
            {
                string content = fileStream.ReadToEnd();
                Assert.IsNotNull(content);
                Assert.IsTrue(content.Length > 0);
            }

            File.Delete(fileName1);
            File.Delete(fileName2);
            File.Delete(fileName3);
        }
    }
}
