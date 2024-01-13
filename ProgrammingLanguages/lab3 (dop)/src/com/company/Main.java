package com.company;
import java.util.Arrays;
public class Main {

    public static void main(String[] args) {
        quadraticEquation[] A= new quadraticEquation[5];
        for (int i =0;i<5;i++){
            A[i]=new quadraticEquation(5-(int)(Math.random()*10),5-(int)(Math.random()*10),
                    5-(int)(Math.random()*10));
        }
        for (quadraticEquation a:A){
            System.out.println(a);
        }
        Arrays.sort(A);
        System.out.println("Отсортировано");
        for (quadraticEquation a:A){
            System.out.println(a);
        }
    }
}
