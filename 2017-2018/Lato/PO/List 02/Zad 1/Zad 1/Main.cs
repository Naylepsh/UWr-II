class Zad_1
{
    public static void Main()
    {
        Streams.RandomWordStream rws = new Streams.RandomWordStream();
        for (int i = 0; i < 10; i++)
            System.Console.WriteLine(rws.Next());

        //System.Console.Read();
    }
}