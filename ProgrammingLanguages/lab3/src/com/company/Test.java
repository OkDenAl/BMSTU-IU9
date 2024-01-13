package com.company;

import java.util.Arrays;

public class Test {

    public static void main(String[] args) {
        SumOfElemFromQueue[] arr = new SumOfElemFromQueue[5];
        for (int i=0;i<5;i++){
            arr[i]=new SumOfElemFromQueue(5);
        }
        for (SumOfElemFromQueue i : arr){
            System.out.println(i);
        }
        System.out.println("Отсортированные очереди");
        Arrays.sort(arr);
        for (SumOfElemFromQueue i : arr){
            System.out.println(i);
        }
    }
}
