using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex_2
{
    public class CaesarStream : Stream
    {
        private Stream _stream;
        private readonly int _rightShift;

        public CaesarStream(Stream stream, int leftShift)
        {
            _stream = stream;
            _rightShift = leftShift;
        }

        private byte EncryptByte(byte b)
        {
            int shifted = (b + _rightShift) % (byte.MaxValue + 1);
            return (byte)shifted;
        }

        public override bool CanRead => _stream.CanRead;

        public override bool CanSeek => _stream.CanSeek;

        public override bool CanWrite => _stream.CanWrite;

        public override long Length => _stream.Length;

        public override long Position {
            get => _stream.Position;
            set => _stream.Position = value;
        }

        public override void Flush()
        {
            _stream.Flush();
        }

        public override int Read(byte[] buffer, int offset, int count)
        {
            int read = _stream.Read(buffer, offset, count);
            buffer.Select(EncryptByte);
            return read;
        }

        public override long Seek(long offset, SeekOrigin origin)
        {
            return _stream.Seek(offset, origin);
        }

        public override void SetLength(long value)
        {
            _stream.SetLength(value);
        }

        public override void Write(byte[] buffer, int offset, int count)
        {
            byte[] encryptedBuffer = buffer.Select(EncryptByte).ToArray();
            _stream.Write(encryptedBuffer, offset, count);
        }
    }


}
