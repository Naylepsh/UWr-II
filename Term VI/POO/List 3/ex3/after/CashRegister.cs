using System;

public class CashRegister
{
  public ITaxCalculator taxCalc;
  public IItemSort itemSort;

  public CashRegister(ITaxCalculator taxCalc, IItemSort itemSort = null)
  {
    this.taxCalc = taxCalc;
    this.itemSort = itemSort;
  }

  public Decimal CalculatePrice(Item[] items)
  {
    Decimal _price = 0;
    if (itemSort != null)
    {
      itemSort.Sort(items);
    }

    foreach (Item item in items)
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