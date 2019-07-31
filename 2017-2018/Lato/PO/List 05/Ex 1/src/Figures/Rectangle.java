package Figures;

import java.util.HashMap;

public class Rectangle extends Figure implements Comparable<Figure>{
    private static final int numberOfPoints = 4;

    public Rectangle(Point bottomLeft, double sizeVertical, double sizeHorizontal){
        super(null);
        // creating all other points
        Point bottomRight = new Point(bottomLeft.getX() + sizeHorizontal, bottomLeft.getY());
        Point topLeft = new Point(bottomLeft.getX(), bottomLeft.getY() + sizeVertical);
        Point topRight = new Point(bottomLeft.getX() + sizeHorizontal, bottomRight.getY() + sizeVertical);
        // adding them to list of points
        HashMap<String, Point> points = new HashMap<>();
        points.put("topLeft", topLeft);
        points.put("topRight", topRight);
        points.put("bottomRight", bottomRight);
        points.put("bottomLeft", bottomLeft);
        setPoints(points);
    }

    @Override
    protected boolean contains(Point point){
        return (getPoints().get("bottomLeft").getX() <= point.getX() &&
                point.getX() <= getPoints().get("topRight").getX() &&
                getPoints().get("bottomLeft").getY() <= point.getY() &&
                point.getY() <= getPoints().get("topRight").getY());
    }

    @Override
    public String toString() {
        return "Rectangle:" + super.toString();
    }
}
