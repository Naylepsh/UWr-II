using System;

public interface ITaxCalculator
{
  Decimal CalculateTax(Decimal Price);
}

public class Simple23VatTaxCalculator : ITaxCalculator
{
  public Decimal CalculateTax(Decimal Price) { return Decimal.Round(Price * 0.23m, 2); }
}