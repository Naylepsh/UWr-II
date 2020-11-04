using System;
using System.IO;
using System.Linq;
using System.Text;
using System.Net.Mime;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using netDumbster.smtp;
using Ex_1;

namespace SmtpFacadeTests
{
    [TestClass]
    public class UnitTest1
    {
        static string host;
        static int port;
        static SimpleSmtpServer server;
        static string from;
        static string to;
        static string subject;
        static string body;

        [ClassInitialize]
        public static void InitTests(TestContext context)
        {
            host = "localhost";
            port = 12345;
            server = SimpleSmtpServer.Start(port);
            from = "sender@gmail.com";
            to = "receiver@gmail.com";
            subject = "test subject";
            body = "test body";
        }

        [TestInitialize]
        public void Init()
        {
            server.ClearReceivedEmail();
        }

        [TestMethod]
        public void ShouldSendEmail()
        {
            SmtpFacade smtpFacade = new SmtpFacade(host, port);
            smtpFacade.Send(from, to, subject, body);

            Assert.AreEqual(1, server.ReceivedEmailCount);

            var received = server.ReceivedEmail.Single();

            Assert.AreEqual(from, received.FromAddress.ToString());
            Assert.AreEqual(to, received.ToAddresses.Single().ToString());
            Assert.AreEqual(subject, received.Headers["subject"]);
            Assert.AreEqual(body, received.MessageParts.Single().BodyData);
        }

        [TestMethod]
        public void ShouldSendAttachmentWithoutMimeType()
        {
            string attachmentData = "test att content";
            MemoryStream attachment = new MemoryStream(Encoding.UTF8.GetBytes(attachmentData));
            string attachmentMimeType = MediaTypeNames.Text.Plain;

            SmtpFacade smtpFacade = new SmtpFacade(host, port);
            smtpFacade.Send(from, to, subject, body, attachment, attachmentMimeType);

            Assert.AreEqual(1, server.ReceivedEmailCount);

            var received = server.ReceivedEmail.Single();
            var receivedAttachment = Convert.FromBase64String(received.MessageParts[1].BodyData);

            Assert.AreEqual(attachmentData, Encoding.UTF8.GetString(receivedAttachment));
        }

        [TestMethod]
        public void ShouldSendAttachmentWithMimeType()
        {
            string attachmentData = "test att content";
            MemoryStream attachment = new MemoryStream(Encoding.UTF8.GetBytes(attachmentData));
            string attachmentMimeType = MediaTypeNames.Text.Plain;

            SmtpFacade smtpFacade = new SmtpFacade(host, port);
            smtpFacade.Send(from, to, subject, body, attachment, attachmentMimeType);

            Assert.AreEqual(1, server.ReceivedEmailCount);

            var received = server.ReceivedEmail.Single();

            Assert.IsTrue(received.MessageParts[0].HeaderData.Contains(attachmentMimeType));
        }    
    }
}
