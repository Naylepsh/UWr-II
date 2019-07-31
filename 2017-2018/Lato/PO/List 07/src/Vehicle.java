import java.io.Serializable;

public class Vehicle implements Serializable{
    private String name;
    private int productionYear;
    private double maxSpeed;

    public Vehicle(String name, int productionYear, double maxSpeed){
        this.name = name;
        this.productionYear = productionYear;
        this.maxSpeed = maxSpeed;
    }

    public Vehicle(){
        maxSpeed = 0.0;
        name = "";
        productionYear = 2018;
    }

    @Override
    public String toString() {
        return "Name: " +name + "\n" +
                "Production year: " + productionYear + "\n" +
                "Max speed: " + maxSpeed + " km/h";
        //return name + "\n" + productionYear + "\n" + maxSpeed + "\n";
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getProductionYear() {
        return productionYear;
    }

    public void setProductionYear(int productionYear) {
        this.productionYear = productionYear;
    }

    public double getMaxSpeed() {
        return maxSpeed;
    }

    public void setMaxSpeed(double maxSpeed) {
        this.maxSpeed = maxSpeed;
    }
}
