package com.company;

import java.util.PriorityQueue;
import java.util.Queue;

public class SumOfElemFromQueue implements Comparable<SumOfElemFromQueue>  {
    private Queue<Integer> curQueue= new PriorityQueue<>();;
    private int sum;
    public SumOfElemFromQueue(){}
    public SumOfElemFromQueue(int n){
        for (int i=0;i<n;i++){
            curQueue.add((int)(Math.random()*100));
        }
    }
    public SumOfElemFromQueue(Queue<Integer> inputQueue){
        this.curQueue=inputQueue;
    }
    public int sumCounter(){
        sum=0;
        for (int i: curQueue){
            sum+=i;
        }
        return sum;
    }
    public int compareTo(SumOfElemFromQueue obj) {
        if (sumCounter() < obj.sumCounter()) return -1;
        else if (sumCounter() > obj.sumCounter()) return 1;
        else return 0;
    }

    public String toString(){
        return " " + curQueue + "  " + sum;
    }
}
