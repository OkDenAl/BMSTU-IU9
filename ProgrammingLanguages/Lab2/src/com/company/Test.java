package com.company;

public class Test{
    public static void main(String[] args) {
        Matrix matrix = new Matrix(3,3);
        matrix.displayMatrix();
        System.out.println();

        matrix.writeElem(2,0,5);
        matrix.writeElem(1,1,7);
        matrix.displayMatrix();
        System.out.println();

        matrix.addColumn(2);
        matrix.addLine(1);
        matrix.displayMatrix();
        System.out.println();

        matrix.removeColumn(1);
        matrix.removeLine(2);
        matrix.displayMatrix();


    }
}
