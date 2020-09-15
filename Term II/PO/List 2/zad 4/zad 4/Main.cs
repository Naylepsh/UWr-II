using System;

class zad_4
{
    public static void Main()
    {
        Listy.ListaLeniwa lista;
        lista = new Listy.ListaLeniwa();
        Console.WriteLine("Czy lista ma dlugosc 0?: " + (lista.Size() == 0));
        Console.WriteLine("40 element: " + lista.Element(40));
        Console.WriteLine("Czy lista ma dlugosc 40?: " + (lista.Size() == 40));
        Console.WriteLine("38 element: " + lista.Element(38));
        Console.WriteLine("Czy lista ma dlugosc 40?: " + (lista.Size() == 40));
        Console.WriteLine("40 element: " + lista.Element(40));

        Console.WriteLine("\nProba wywolania lista.Element(-1)");
        try
        {
            Console.WriteLine(lista.Element(-1));
        }
        catch (ArgumentOutOfRangeException a)
        {
            Console.WriteLine(a);
        }


        Console.WriteLine();
        Listy.Pierwsze pierwsze = new Listy.Pierwsze();
        Console.WriteLine("Czy lista ma dlugosc 0?: " + (pierwsze.Size() == 0));
        Console.WriteLine("10 element: " + pierwsze.Element(10));
        Console.WriteLine("Czy lista ma dlugosc 10?: " + (pierwsze.Size() == 10));
        Console.WriteLine("8 element: " + pierwsze.Element(8));
        Console.WriteLine("Czy lista ma dlugosc 10?: " + (pierwsze.Size() == 10));
        Console.WriteLine("10 element: " + pierwsze.Element(10));
        Console.WriteLine(pierwsze.ToString());
        //System.Console.Read();
    }
}