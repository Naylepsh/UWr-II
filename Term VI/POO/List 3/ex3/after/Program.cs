using System;

namespace after
{
  class Program
  {
    static void Main(string[] args)
    {
      Item[] items = new Item[]
      {
        new Item(3.89m, "Milk"),
        new Item(2.99m, "Cucumber"),
        new Item(4.69m, "Banana"),
        new Item(64.99m, "Salmon")
      };

      test1(items);
      Console.WriteLine();
      test2(items);
    }

    public void x()
    {
      Console.Write("???");
    }

    static void test1(Item[] items)
    {
      var cashRegister = new CashRegister(new Simple23VatTaxCalculator(), new PriceDescSort());
      Console.WriteLine("Sumeryczna cena: {0}", cashRegister.CalculatePrice(items));
      cashRegister.PrintBill(items);
    }

    static void test2(Item[] items)
    {
      var cashRegister = new CashRegister(new Simple23VatTaxCalculator(), new AlphabeticalNameSort());
      Console.WriteLine("Sumeryczna cena: {0}", cashRegister.CalculatePrice(items));
      cashRegister.PrintBill(items);
    }
  }
}