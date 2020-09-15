import java.io.Serializable;

public class Car extends Vehicle implements Serializable {
    private int gears; // liczba biegow
    private double mileage; // przebieg
    private double trunkVolume; // pojemnosc bagaznika

    public Car(String name, int productionYear, double maxSpeed,
               int gears, double mileage, double trunkVolume){
        super(name, productionYear, maxSpeed);
        this.gears = gears;
        this.mileage = mileage;
        this.trunkVolume = trunkVolume;
    }

    @Override
    public String toString() {
        return super.toString() +
                "Gears:" + gears + "\n" +
                "Mileage:" + mileage + "\n" +
                "Trunk volume:" + trunkVolume + "\n";
    }

    public Car(){
        super();
        this.gears = 2;
        this.mileage = 0.0;
        this.trunkVolume = 0.0;
    }

    public int getGears() {
        return gears;
    }

    public double getMileage() {
        return mileage;
    }

    public double getTrunkVolume() {
        return trunkVolume;
    }
}
