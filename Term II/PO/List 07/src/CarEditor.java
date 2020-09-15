import com.sun.org.apache.xml.internal.resolver.readers.ExtendedXMLCatalogReader;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;
import java.util.Scanner;

public class CarEditor extends JFrame implements ActionListener {
    private JTextField textName, textProductionYear, textMaxSpeed,
            textGear, textMileage, textTrunkVolume;
    private JButton buttonWrite, buttonRead;
    private Car car;
    private File file;

    public CarEditor(File file){
        car = new Car();
        this.file = file;
        makeGUI();
    }

    private void makeGUI(){
        // Main window
        setDefaultCloseOperation(EXIT_ON_CLOSE);
        setLocationRelativeTo(null);
        Container container = getContentPane();
        GridLayout layout = new GridLayout(7, 2);
        container.setLayout(layout);

        // Save button
        buttonWrite = new JButton("Save");
        buttonWrite.addActionListener(this);
        container.add(buttonWrite);

        // Open button
        buttonRead = new JButton("Open");
        buttonRead.addActionListener(this);
        container.add(buttonRead);

        // Name edit
        JLabel nameLabel = new JLabel("Name");
        container.add(nameLabel);
        textName = new JTextField(this.car.getName(),4);
        container.add(textName);

        // Production year edit
        JLabel yearLabel = new JLabel("Production year");
        container.add(yearLabel);
        textProductionYear = new JTextField(Integer.toString(this.car.getProductionYear()),4);
        container.add(textProductionYear);

        // Max speed edit
        JLabel speedLabel = new JLabel("Max Speed");
        container.add(speedLabel);
        textMaxSpeed = new JTextField(Double.toString(this.car.getMaxSpeed()),4);
        container.add(textMaxSpeed);

        // Gears edit
        JLabel gearLabel = new JLabel("Gears");
        container.add(gearLabel);
        textGear = new JTextField(Integer.toString(this.car.getGears()),4);
        container.add(textGear);

        // Mileage edit
        JLabel mileageLabel = new JLabel("Mileage");
        container.add(mileageLabel);
        textMileage = new JTextField(Double.toString(this.car.getMileage()),4);
        container.add(textMileage);

        // Trunk volume edit
        JLabel volumeLabel = new JLabel("Trunk volume");
        container.add(volumeLabel);
        textTrunkVolume = new JTextField(Double.toString(this.car.getTrunkVolume()),4);
        container.add(textTrunkVolume);

        pack();
        setVisible(true);
    }
    private void ReadFromCar(){
        textName.setText(car.getName());
        textProductionYear.setText(Integer.toString(car.getProductionYear()));
        textMaxSpeed.setText(Double.toString(car.getMaxSpeed()));
        textGear.setText(Integer.toString(car.getGears()));
        textMileage.setText(Double.toString(car.getMileage()));
        textTrunkVolume.setText(Double.toString(car.getTrunkVolume()));
    }

    private void WriteToCar(){
        car = new Car(textName.getText(),
                Integer.parseInt(textProductionYear.getText()),
                Double.parseDouble(textMaxSpeed.getText()),
                Integer.parseInt(textGear.getText()),
                Double.parseDouble(textMileage.getText()),
                Double.parseDouble(textTrunkVolume.getText()));
    }

    private void WriteToFile(){
        try {
            ObjectOutputStream outputStream = new ObjectOutputStream(new FileOutputStream(this.file));
            WriteToCar();
            outputStream.writeObject(this.car);
        } catch (Exception e){
            System.out.println("Encountered a problem during writing to a file");
        }
    }

    private void ReadFromFile(){
        try {
            ObjectInputStream inputStream = new ObjectInputStream(new FileInputStream(this.file));
            car = (Car) inputStream.readObject();
            ReadFromCar();
        } catch (Exception e){
            System.out.println("Encountered a problem during reading from a file");
        }
    }

    public void actionPerformed(ActionEvent actionEvent){
        if (actionEvent.getSource() == buttonWrite) {
            WriteToFile();
        } else if (actionEvent.getSource() == buttonRead) {
            ReadFromFile();
        }
    }
}
