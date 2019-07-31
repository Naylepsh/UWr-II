package Figures;

public class Square extends Rectangle {
    public Square(Point point, double size){
        super(point, size, size);
    }

    @Override
    public String toString() {
        return "Square:" + super.toString();
    }
}
