import javax.swing.*;
import java.io.File;


public class Main {
    public static void main(String[] args) {
        String plik = args[0];
        String nazwaKlasy = args[1];
        //String plik = "C:\\Users\\naylepsh\\Documents\\Code\\PO\\List 07\\test2.txt";
        //String nazwaKlasy = "Vehicle";

        JFrame frame;
        if (nazwaKlasy.equals("Vehicle"))
            frame = new VehicleEditor(new File(plik));
        else if (nazwaKlasy.equals("Car"))
            frame = new CarEditor(new File(plik));
        else if (nazwaKlasy.equals("Tram"))
            frame = new TramEditor(new File(plik));
    }
}
