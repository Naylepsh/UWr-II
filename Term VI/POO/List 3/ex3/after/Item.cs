using System;

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


