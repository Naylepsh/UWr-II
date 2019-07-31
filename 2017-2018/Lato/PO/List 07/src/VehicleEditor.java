import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;

public class VehicleEditor extends JFrame implements ActionListener{
    private JTextField textName, textProductionYear, textMaxSpeed;
    private JButton buttonWrite, buttonRead;
    private Vehicle vehicle;
    private File file;

    public VehicleEditor(File file){
        vehicle = new Vehicle();
        this.file = file;
        makeGUI();
    }

    private void makeGUI(){
        // Main window
        setDefaultCloseOperation(EXIT_ON_CLOSE);
        setLocationRelativeTo(null);
        Container container = getContentPane();
        GridLayout layout = new GridLayout(4, 2);
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
        textName = new JTextField(this.vehicle.getName(),4);
        container.add(textName);

        // Production year edit
        JLabel yearLabel = new JLabel("Production year");
        container.add(yearLabel);
        textProductionYear = new JTextField(Integer.toString(this.vehicle.getProductionYear()),4);
        container.add(textProductionYear);

        // Max speed edit
        JLabel speedLabel = new JLabel("Max Speed");
        container.add(speedLabel);
        textMaxSpeed = new JTextField(Double.toString(this.vehicle.getMaxSpeed()),4);
        container.add(textMaxSpeed);

        //'Finishing touches'
        pack();
        setVisible(true);
    }

    private void vehicleToText(){
        textName.setText(vehicle.getName());
        textProductionYear.setText(Integer.toString(vehicle.getProductionYear()));
        textMaxSpeed.setText(Double.toString(vehicle.getMaxSpeed()));
    }

    private void textToVehicle(){
        vehicle = new Vehicle(textName.getText(),
                Integer.parseInt(textProductionYear.getText()),
                Double.parseDouble(textMaxSpeed.getText()));
    }

    private void WriteToFile(){
        try {
            textToVehicle(); // pobierz aktualne wartosci z pol tekstowych
            ObjectOutputStream outputStream = new ObjectOutputStream(new FileOutputStream(this.file));
            outputStream.writeObject(this.vehicle); // zapisz zupdetjowany plik
        } catch (Exception e){
            System.out.println("Encountered problem during writing to file");
        }
    }

    private void ReadFromFile(){
        try {
            ObjectInputStream inputStream = new ObjectInputStream(new FileInputStream(this.file));
            vehicle = (Vehicle) inputStream.readObject();
            vehicleToText();
        } catch (Exception e){
            System.out.println("Encountered problem during reading from file");
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
