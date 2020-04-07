using System;
using System.IO;
using System.Text;
using Ex_2;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace CaesarStreamTests
{
    [TestClass]
    public class UnitTest1
    {
        private static readonly string initialString = "abcde";

        [TestMethod]
        public void ShouldProperlyEncrypt()
        {
            MemoryStream stream = new MemoryStream();
            CaesarStream cstream = new CaesarStream(stream, 1);
            cstream.Write(Encoding.UTF8.GetBytes(initialString), 0, initialString.Length);

            Assert.AreEqual("bcdef", Encoding.UTF8.GetString(stream.ToArray()));
        }

        
        [TestMethod]
        public void ShouldProperlyDecryptEncrypted()
        {
            byte[] buffer = Encoding.UTF8.GetBytes(initialString);
            MemoryStream stream = new MemoryStream();
            CaesarStream cstreamShiftOneRight = new CaesarStream(stream, 1);
            cstreamShiftOneRight.Write(buffer, 0, initialString.Length);
            CaesarStream cstreamShiftOneLeft = new CaesarStream(stream, -1);
            cstreamShiftOneLeft.Read(buffer, 0, initialString.Length);

            Assert.AreEqual(initialString, Encoding.UTF8.GetString(buffer));
        }

        [TestMethod]
        public void ShouldHandleStreamStacking()
        {
            // oneLeft should negate oneRight
            byte[] buffer = Encoding.UTF8.GetBytes(initialString);
            MemoryStream stream = new MemoryStream();
            CaesarStream cstreamShiftOneRight = new CaesarStream(stream, 1);
            CaesarStream cstreamShiftOneLeft = new CaesarStream(cstreamShiftOneRight, -1);
            cstreamShiftOneLeft.Read(buffer, 0, initialString.Length);

            Assert.AreEqual(initialString, Encoding.UTF8.GetString(buffer));
        }
    }
}
