package com.company;

public class quadraticEquation implements Comparable<quadraticEquation> {
    private int a,b,c,squaresAmount;
    public quadraticEquation(int a,int b,int c){
        this.a=a;
        this.b=b;
        this.c=c;
    }
    public int calculateX(){
        if ((b*b-4*a*c)==0){
            squaresAmount= 1;
        }
        else if ((b*b-4*a*c)>0){
            squaresAmount= 2;
        }
        else{
            squaresAmount= 0;
        }
        return squaresAmount;
    }
    public int compareTo(quadraticEquation obj) {
        if (calculateX() < obj.calculateX()) return -1;
        else if (calculateX() > obj.calculateX()) return 1;
        else return 0;
    }
    public String toString(){
        return "("+ a+")x^2 " + "+ ("+b+")x"+" + ("+c+") " +"Количество корней: "+ squaresAmount;
    }
}
