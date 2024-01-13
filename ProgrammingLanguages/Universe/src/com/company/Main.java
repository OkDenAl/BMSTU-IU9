package com.company;

public class Main {

    public static void main(String[] args) {
        Universe Alpha = new Universe();
        Alpha.addParticle(10,10,8); //820
        Alpha.addParticle(8,8,7); //452
        Alpha.addParticle(2,3,5); //34
        System.out.println("Your Kinetic Energy is "+Alpha.countKineticOfAllParticles());
    }
}
