package Figures;

import java.util.HashMap;

public class Triangle extends Figure {
    public Triangle(Point botLeft, Point botRight, Point top) throws Exception{
        super(null);
        if (botLeft.getY() != botRight.getY())
            throw new Exception("Invalid coordinates of points");
        HashMap<String, Point> points = new HashMap<>();
        points.put("bottomLeft", botLeft);
        points.put("bottomRight", botRight);
        points.put("top", top);
        setPoints(points);
    }

    public boolean contains(Point point){
        // y = ax + b -> b = y - ax -> y1 - ax1 = y2 - ax2 -> a = (y2 - y1) / (x2 - x1)
        // b = y - ax
        // line from bottomLeft to top
        double a1 = (getPoints().get("top").getY() - getPoints().get("bottomLeft").getY()) /
                (getPoints().get("top").getX() - getPoints().get("bottomLeft").getX());
        double b1 = getPoints().get("bottomLeft").getY() - a1 * getPoints().get("bottomLeft").getX();

        // line from bottomRight to top
        double a2 = (getPoints().get("top").getY() - getPoints().get("bottomRight").getY()) /
                (getPoints().get("top").getX() - getPoints().get("bottomRight").getX());
        double b2 = getPoints().get("bottomRight").getY() - a2 * getPoints().get("bottomRight").getX();

        // line from bottomLeft to bottomRight
        double a3 = 0;
        double b3 = getPoints().get("bottomRight").getY();

        /* doesn't work? -- supposedly works tho..
        System.out.println(point);
        System.out.println(point.getY() <= a1*point.getX() + b1);
        System.out.println(point.getY() <= a2*point.getX() + b2);
        System.out.println(point.getX() >= a3*point.getX() + b3);
        */
        // y = a*figure.x + b    if figure.y <= y then figure is under that line?
        return (point.getY() <= a1*point.getX() + b1 &&
                point.getY() <= a2*point.getX() + b2 &&
                point.getY() >= a3*point.getX() + b3);
    }

    @Override
    public String toString() {
        return "Triangle:" + super.toString();
    }
}
