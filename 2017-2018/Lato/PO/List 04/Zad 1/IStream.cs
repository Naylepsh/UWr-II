namespace Zad_1
{
    public interface IStream
    {
        int Next();
        bool Eos();
        void Reset();
    }
}