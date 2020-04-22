using System;
using System.IO;
using System.Net;
using System.Net.Mail;
using System.Net.Mime;

namespace Ex_1
{
    public class SmtpFacade
    {
        private SmtpClient _client;

        public SmtpFacade(string host, int port)
        {
            _client = new SmtpClient(host, port);
        }

        public SmtpFacade(string host, int port, string userName, string password) : this(host, port)
        {
            _client.Credentials = new NetworkCredential(userName, password);

        }

        public void Send(
            string from,
            string to,
            string subject,
            string body,
            Stream attachment = null,
            string attachmentMimeType = null
            )
        {
            MailMessage message = new MailMessage(from, to, subject, body);
            if (attachment != null && attachmentMimeType != null)
            {
                message.Attachments.Add(new Attachment(attachment, new ContentType(attachmentMimeType)));
                //message.Attachments.Add(Attachment.CreateAttachmentFromString("test att content", new ContentType(attachmentMimeType)));
                //message.Attachments.Add(attachment);
            }

            _client.Send(message);
        }
    }
}