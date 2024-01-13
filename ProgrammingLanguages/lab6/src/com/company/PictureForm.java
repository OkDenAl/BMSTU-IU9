package com.company;
import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

public class PictureForm {
    private JPanel mainPanel;
    private JSpinner legsSpinner;
    private JSpinner armsSpinner;
    private CanvasPanel canvasPanel;
    private JList colorsList;

    public PictureForm () {
        colorsList.addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent evt) {
                    String val = colorsList.getSelectedValue().toString();
                    canvasPanel.setColor(val);
            }
        });
        legsSpinner.addChangeListener (new ChangeListener( ) {
            public void stateChanged (ChangeEvent e) {
                int legs = (int) legsSpinner.getValue( );
                canvasPanel.setAmountOfLegs(legs);
            }
        } ) ;
        armsSpinner.addChangeListener (new ChangeListener( ) {
            public void stateChanged (ChangeEvent e) {
                int arms = (int) armsSpinner.getValue( );
                canvasPanel.setAmountOfArms(arms);
            }
        } ) ;
        armsSpinner.setValue(2);
        legsSpinner.setValue(4);
    }
    public static void main(String[] args) {
        JFrame frame = new JFrame("Кентавр");
        frame.setContentPane(new PictureForm().mainPanel);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.pack();
        frame.setVisible(true);
    }
}
