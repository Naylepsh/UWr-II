using System;

namespace ex4
{
  class Program
  {
    static void Main(string[] args)
    {
      int w = 4, h = 5;
      Rectangle rect = new Rectangle(w, h); // can't convert from Square to Rect anymore
      AreaCalculator calc = new AreaCalculator();
      Console.WriteLine("prostokąt o wymiarach {0} na {1} ma pole {2}", w, h, calc.CalculateArea(rect));

      Square sq = new Square(w);
      Console.WriteLine("prostokąt o wymiarach {0} na {1} ma pole {2}", w, w, calc.CalculateArea(sq));
    }
  }

  public abstract class Rectangular
  {
    protected int _width;
    protected int _height;
    public abstract int GetWidth();
    public abstract int GetHeight();
    // public abstract int CalculateArea(); // or have children implement area calculation
  }

  public class Rectangle : Rectangular
  {
    public Rectangle(int width, int height)
    {

      _width = width;
      _height = height;
    }

    public override int GetWidth() { return _width; }
    public override int GetHeight() { return _height; }
  }

  public class Square : Rectangular
  {
    public Square(int size)
    {
      _width = size;
      _height = size;
    }

    public override int GetWidth() { return _width; }
    public override int GetHeight() { return _height; }
  }


  public class AreaCalculator
  {
    public int CalculateArea(Rectangular rect)
    {
      return rect.GetWidth() * rect.GetHeight();
    }
  }
}
