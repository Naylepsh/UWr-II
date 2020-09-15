import Figures.*;

public class Main {
    public static void main(String[] args) throws Exception{
        List<Figure> figures = new List<Figure>();
        figures.add(new Rhombus(new Point(-5,-5), 10, Math.PI / 4));
        figures.add(new Rectangle(new Point(1,1), 5,6));
        figures.add(new Square(new Point(1,1), 2));
        figures.add(new Triangle(new Point(0,0), new Point(10,0), new Point(5,50)));
        System.out.println(figures);

        System.out.println("Removing first element: " + figures.pop());
        System.out.println("Queue after removal of first elem:");
        System.out.println(figures);

    }
}
