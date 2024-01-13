package com.company;

import java.util.ArrayList;
import java.util.Iterator;

public class NODList implements Iterable {
    private ArrayList<Integer> inputArray = new ArrayList<Integer>();
    private int n;
    public NODList(int n){
        this.n=n;
        for (int i=0;i<n;i++){
            inputArray.add((int)(Math.random()*100));
        }
        //makeNODArray();
    }
    /*private void makeNODArray(){
        //NODArray.clear();
        for (int k=0;k<n;k++){
            if (k == 0) {
                NODArray.add(countNOD(inputArray.get(k),inputArray.get(k+1)));
            } else if (k==n-1){
                NODArray.add(countNOD(inputArray.get(k),inputArray.get(k-1)));
            } else {
                // Если проверяем на НОД оба соседних числа сразу
                NODArray.add(countNOD((countNOD(inputArray.get(k),inputArray.get(k-1))),inputArray.get(k+1)));

                // Если проверяем на НОД сначала число справа, потом слева и ищем наибольший

                /*int a = countNOD(inputArray.get(k),inputArray.get(k+1));
                int b = countNOD(inputArray.get(k),inputArray.get(k-1));
                NODArray.add(Math.max(a,b));
            }
        }
    }*/
    public int addElem(int index, int num){
        if (index>=(n+1)){
            System.out.println("\nError Index Out of Range");
            return -1;
        }
        inputArray.add(index,num);
        n++;
        //makeNODArray();
        return 1;
    }
    public int deleteElem(int index){
        if (index>=n){
            System.out.println("\nError Index Out of Range");
            return -1;
        }
        inputArray.remove(index);
        n--;
        return 1;
    }
    private int countNOD(int a, int b){
        if (a == 0)
            return b;
        while (b != 0) {
            if (a > b)
                a = a - b;
            else
                b = b - a;
        }
        return a;
    }

    public Iterator iterator() {
        return new NODList.NODIterator();
    }
    private class NODIterator implements Iterator {
        private int pos;
        public NODIterator () {
            pos = 0 ;
        }
        public boolean hasNext () {
            return pos < n;
        }
        public Integer next () {
            if (pos==0){
                pos++;
                return countNOD(inputArray.get(pos-1),inputArray.get(pos));
            } else if (pos==n-1){
                pos++;
                return countNOD(inputArray.get(pos-1),inputArray.get(pos-2));
            }
            pos++;
            return countNOD(countNOD(inputArray.get(pos-1),inputArray.get(pos)), inputArray.get(pos-2));
        }
    }
    public void Printer(){
        for (int i=0;i<n;i++){
            System.out.printf("%d  ",inputArray.get(i));
        }
    }
}
