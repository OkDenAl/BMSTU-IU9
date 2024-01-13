package com.company;

import javax.swing.*;
import java.awt.*;
public class CanvasPanel extends JPanel {
    private int arms = 2;
    private int legs =4;
    private String inputColor="Черный";
    public void setAmountOfLegs(int l) {
        legs = l;
        repaint();
    }
    public void setAmountOfArms(int a) {
        arms = a;
        repaint();
    }
    public void setColor(String col){
        inputColor=col;
        repaint();
    }
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        Color mainColorOfKentavr=Color.BLACK;
        switch (inputColor){
            case "Черный":
                mainColorOfKentavr=Color.BLACK;
                break;
            case "Зеленый":
                mainColorOfKentavr=Color.GREEN;
                break;
            case "Красный":
                mainColorOfKentavr=Color.RED;
                break;
            case "Синий":
                mainColorOfKentavr=Color.BLUE;
        }
        g.setColor(mainColorOfKentavr);
        setBackground(Color.LIGHT_GRAY);
        g.translate(-30,20);
        //Горизонтальная часть
        g.fillRoundRect(300,200,250,80,70,70);
        //Вертикальная часть
        g.fillRoundRect(470,100,80,180,70,70);
        //Срез вертикальной части
        g.setColor(getBackground());
        g.fillRect(470,100,100,30);
        //Голова
        g.setColor(mainColorOfKentavr);
        g.fillOval(480,65,60,60);
        //Хвост
        int[] xPoints={320,300,240,250};
        int[] yPoints={220,260,330,230};
        g.fillPolygon(xPoints,yPoints,4);
        //Ноги
        int neededPlace=0;
        if (legs!=0){
            neededPlace=100/legs;
        }
        if (neededPlace>15){
            neededPlace=15;
        }
        for (int i=0;i<legs/2+legs%2;i++){
            g.fillRoundRect(305+i*(neededPlace+2),250,neededPlace,130,10,10);
        }
        for (int i=0;i<legs/2;i++){
            g.fillRoundRect(510+25-i*(neededPlace+2),240,neededPlace,140,10,10);
        }
        //Руки
        int[] xArmsPoints={540,550,575,565};
        int[] yArmsPoints={150,150,260,260};
        int[] x1ArmsPoints={480,470,430,440};
        int[] y1ArmsPoints={150,150,40,40};
        double angle=-(1.5*Math.PI)/arms;
        int x=540;
        int y=150;
        int[]xCord={540,0,0,0};
        int[]yCord={130,0,0,0};
        for (int i=0;i<arms/2+arms%2;i++){
            for (int j=0;j<4;j++){
                xCord[j]=(int)((xArmsPoints[j]-x)*Math.cos(i*angle)-(yArmsPoints[j]-y)*Math.sin(i*angle)+x);
                yCord[j]=(int)((xArmsPoints[j]-x)*Math.sin(i*angle)+(yArmsPoints[j]-y)*Math.cos(i*angle)+y);
            }
            g.fillPolygon(xCord,yCord,4);
        }
        x=490;
        y=140;
        for (int i=0;i<arms/2;i++) {
            for (int j = 0; j < 4; j++) {
                xCord[j] = (int) ((x1ArmsPoints[j] - x) * Math.cos(i * angle) - (y1ArmsPoints[j] - y) * Math.sin(i * angle) + x);
                yCord[j] = (int) ((x1ArmsPoints[j] - x) * Math.sin(i * angle) + (y1ArmsPoints[j] - y) * Math.cos(i * angle) + y);
            }
            g.fillPolygon(xCord, yCord, 4);
        }
    }
}


