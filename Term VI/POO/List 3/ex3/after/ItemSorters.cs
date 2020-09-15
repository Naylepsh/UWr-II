using System;

public interface IItemSort
{
  Item[] Sort(Item[] items);
}

class AlphabeticalNameSort : IItemSort
{
  public Item[] Sort(Item[] items)
  {
    Array.Sort(items, (x, y) => x.Name.CompareTo(y.Name));
    return items;
  }
}

class PriceDescSort : IItemSort
{
  public Item[] Sort(Item[] items)
  {
    Array.Sort(items, (x, y) => y.Price.CompareTo(x.Price));
    return items;
  }
}