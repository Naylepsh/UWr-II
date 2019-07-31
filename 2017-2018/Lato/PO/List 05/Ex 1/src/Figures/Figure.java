package Figures;

import java.util.HashMap;

public class Figure implements Comparable<Figure>{
    HashMap<String, Point> points;

    public Figure(HashMap<String, Point> points){
        this.points = points;
    }

    public void setPoints(HashMap<String, Point> points) {
        this.points = points;
    }

    public HashMap<String, Point> getPoints() {
        return points;
    }

    @Override
    public int compareTo(Figure o) {
        int containedInThis = 0;
        int allPointsOfFigure = 0;
        for (Point point : o.points.values()) {
            if (contains(point))
                ++containedInThis;
            ++allPointsOfFigure;
        }

        int containedInFigure = 0;
        int allPointsOfThis = 0;
        for (Point point : this.points.values()){
            if (o.contains(point))
                ++containedInFigure;
            ++allPointsOfThis;
        }

        if (containedInThis == allPointsOfFigure) // if the whole figure o is inside this-figure
            return 1;
        if (containedInFigure == allPointsOfThis) // if the whole this-figure's inside figure o
            return -1;
        else
            return 0;
    }

    protected boolean contains(Point point){
        return false;
    }

    @Override
    public String toString() {
        String s = "";
        for (Point point : getPoints().values()){
            s += point + " ";
        }
        return s;
    }
}
