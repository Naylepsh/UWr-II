package Figures;

import java.util.HashMap;

public class Rhombus extends Figure {
    public Rhombus(Point bottomLeft, double size, double angle){
        super(null);
        // creating all other points
        Point bottomRight = new Point(bottomLeft.getX() + size, bottomLeft.getY());
        Point topLeft = new Point(bottomLeft.getX() + size * Math.cos(angle),
                bottomLeft.getY() + size * Math.sin(angle));
        Point topRight = new Point(topLeft.getX() + size, topLeft.getY());

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
        // y = ax + b -> b = y - ax -> y1 - ax1 = y2 - ax2 -> a = (y2 - y1) / (x2 - x1)
        // b = y - ax

        // line from bottomLeft to topLeft
        double a1 = (getPoints().get("topLeft").getY() - getPoints().get("bottomLeft").getY()) /
                (getPoints().get("topLeft").getX() - getPoints().get("bottomLeft").getX());
        double b1 = getPoints().get("bottomLeft").getY() - a1 * getPoints().get("bottomLeft").getX();

        // line from bottomRight to topRight
        double a2 = (getPoints().get("topRight").getY() - getPoints().get("bottomRight").getY()) /
                (getPoints().get("topRight").getX() - getPoints().get("bottomRight").getX());
        double b2 = getPoints().get("bottomRight").getY() - a1 * getPoints().get("bottomRight").getX();

        return (point.getY() >= a1 * point.getX() + b1 &&
                point.getY() <= a2 * point.getX() + b2 &&
                point.getY() >= getPoints().get("bottomLeft").getY() &&
                point.getY() <= getPoints().get("topRight").getY() &&
                point.getX() >= getPoints().get("bottomLeft").getX() &&
                point.getX() <= getPoints().get("topRight").getX());
    }

    @Override
    public String toString() {
        return "Rhombus:" + super.toString();
    }
}
