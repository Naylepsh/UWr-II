import java.io.Serializable;

public class Tram extends Vehicle implements Serializable {
    private String line;
    private int maxCapacity;
    private String color;
    public Tram(String name, int productionYear, double maxSpeed,
                String line, int maxCapacity, String color){
        super(name, productionYear, maxSpeed);
        this.line = line;
        this.maxCapacity = maxCapacity;
        this.color = color;
    }

    public Tram(){
        super();
        line = "";
        maxCapacity = 0;
        color = "";
    }

    @Override
    public String toString() {
        return super.toString() +
                "Line:" + line + "\n" +
                "Max capacity:" + maxCapacity + "\n" +
                "Color:" + color + "\n";
    }

    public String getLine() {
        return line;
    }

    public int getMaxCapacity() {
        return maxCapacity;
    }

    public String getColor() {
        return color;
    }
}
