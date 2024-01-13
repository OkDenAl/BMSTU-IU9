package com.company;
import java.util.ArrayList;

public class Matrix {
    private int columns,lines;
    private ArrayList<ArrayList<Integer>> matrix = new ArrayList<ArrayList<Integer>>();

    //Конструкторы класса
    public Matrix() {}

    public Matrix(int lines, int columns) {
        this.columns=columns;
        this.lines=lines;
        fillMatrix(columns,lines);
    }

    public Matrix (ArrayList<ArrayList<Integer>> matrix){
        this.matrix=matrix;
    }
    //Дефолтное заполнение матрицы нулями
    public void fillMatrix(int lines, int columns){
        matrix.clear();
        for (int i=0;i<lines;i++){
            matrix.add(new ArrayList<Integer>());
            for (int j=0;j<columns;j++){
                matrix.get(i).add(0);
            }
        }
    }
    //Вывод матрицы
    public void displayMatrix(){
        for (ArrayList<Integer> line : matrix){
            for (Integer elem : line ) System.out.print(elem +" ");
            System.out.println();
        }
    }
    //Фичи

    public void writeElem(int line, int column, int elem){
        matrix.get(line).set(column,elem);
    }

    public int readElem(int line, int column){
        return matrix.get(line).get(column);
    }

    public void addColumn(int column){
        columns+=1;
        for (ArrayList<Integer> line : matrix){
            line.add(column,0);
        }
    }

    public void addLine(int line){
        lines+=1;
        matrix.add(line,new ArrayList<Integer>());
        for (int i=0;i<columns;i++){
            matrix.get(line).add(0);
        }
    }

    public void removeColumn(int column){
        columns-=1;
        for (ArrayList<Integer> line : matrix){
            line.remove(column);
        }
    }

    public void removeLine(int line){
        lines-=1;
        matrix.remove(line);
        }
}
