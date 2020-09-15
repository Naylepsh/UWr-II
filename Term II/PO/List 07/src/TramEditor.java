import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;

public class TramEditor extends JFrame implements ActionListener{
    private JTextField textName, textProductionYear, textMaxSpeed,
            textLine, textMaxCapacity, textColor;
    private JButton buttonWrite, buttonRead;
    private Tram tram;
    private File file;

    public TramEditor(File file){
        tram = new Tram();
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
        textName = new JTextField(this.tram.getName(),4);
        container.add(textName);

        // Production year edit
        JLabel yearLabel = new JLabel("Production year");
        container.add(yearLabel);
        textProductionYear = new JTextField(Integer.toString(this.tram.getProductionYear()),4);
        container.add(textProductionYear);

        // Max speed edit
        JLabel speedLabel = new JLabel("Max Speed");
        container.add(speedLabel);
        textMaxSpeed = new JTextField(Double.toString(this.tram.getMaxSpeed()),4);
        container.add(textMaxSpeed);

        // Gears edit
        JLabel gearLabel = new JLabel("Line");
        container.add(gearLabel);
        textLine = new JTextField(this.tram.getLine(),4);
        container.add(textLine);

        // Mileage edit
        JLabel mileageLabel = new JLabel("Max Capacity");
        container.add(mileageLabel);
        textMaxCapacity = new JTextField(Integer.toString(this.tram.getMaxCapacity()),4);
        container.add(textMaxCapacity);

        // Trunk volume edit
        JLabel volumeLabel = new JLabel("Color");
        container.add(volumeLabel);
        textColor = new JTextField(this.tram.getColor(),4);
        container.add(textColor);

        pack();
        setVisible(true);
    }
    private void ReadFromTram(){
        textName.setText(tram.getName());
        textProductionYear.setText(Integer.toString(tram.getProductionYear()));
        textMaxSpeed.setText(Double.toString(tram.getMaxSpeed()));
        textLine.setText(tram.getLine());
        textMaxCapacity.setText(Integer.toString(tram.getMaxCapacity()));
        textColor.setText(tram.getColor());
    }

    private void WriteToTram(){
        tram = new Tram(textName.getText(),
                Integer.parseInt(textProductionYear.getText()),
                Double.parseDouble(textMaxSpeed.getText()),
                textLine.getText(),
                Integer.parseInt(textMaxCapacity.getText()),
                textColor.getText());
    }

    private void WriteToFile(){
        try {
            ObjectOutputStream outputStream = new ObjectOutputStream(new FileOutputStream(this.file));
            WriteToTram();
            outputStream.writeObject(this.tram);
        } catch (Exception e){
            System.out.println("Encountered a problem during writing to file");
        }
    }

    private void ReadFromFile(){
        try {
            ObjectInputStream inputStream = new ObjectInputStream(new FileInputStream(this.file));
            tram = (Tram) inputStream.readObject();
            ReadFromTram();
        } catch (Exception e){
            System.out.println("Encountered a problem during reading from file");
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
