using System;

namespace before
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

      var cashRegister = new CashRegister();
      Console.WriteLine("Sumeryczna cena: {0}", cashRegister.CalculatePrice(items));
      cashRegister.PrintBill(items);
    }
  }

  public class TaxCalculator
  {
    static Decimal tax = new Decimal(0.22);
    public Decimal CalculateTax(Decimal Price) { return Decimal.Round(Price * tax, 2); }
  }

  public class Item
  {
    public Item(Decimal price, string name)
    {
      this.Price = price;
      this.Name = name;
    }
    public Decimal Price { get; }
    public string Name { get; }
  }

  public class CashRegister
  {
    public TaxCalculator taxCalc = new TaxCalculator();
    public Decimal CalculatePrice(Item[] Items)
    {
      Decimal _price = 0;
      foreach (Item item in Items)
      {
        _price += item.Price + taxCalc.CalculateTax(item.Price);
      }
      return _price;
    }

    public void PrintBill(Item[] Items)
    {
      foreach (var item in Items)
        Console.WriteLine("towar {0} : cena {1} + podatek {2}",
          item.Name, item.Price, taxCalc.CalculateTax(item.Price));
    }
  }
}