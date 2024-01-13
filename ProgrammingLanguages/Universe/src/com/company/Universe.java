package com.company;
import java.util.ArrayList;
public class Universe {
    private ArrayList<Particles> particles=new ArrayList<Particles>();
    private int countOfParticles;

    public Universe(){}

    public class Particles{
        private double m ,V;
        public Particles (double m, double Vx, double Vy){
            this.m=m;
            this.V=Math.sqrt(Vx*Vx+Vy*Vy);
            countOfParticles++;
        }
        public double getSpeed(){
            return V;
        }
        public double getMass(){
            return m;
        }
        public double getKineticEnergy(){
            return m*V*V/2;
        }
    }

    public void addParticle(double m, double Vx, double Vy){
         particles.add(countOfParticles,new Particles(m,Vx,Vy));
    }
    //Отладка---------------------------------------------------
    public void printParticle(){
        for (int i=0;i<countOfParticles;i++){
            System.out.println(particles.get(i).getMass());
        }
    }
    //------------------------------------------------------------
    public double countKineticOfAllParticles(){
        double res=0;
        for (int i=0;i<countOfParticles;i++){
            res+=particles.get(i).getKineticEnergy();
        }
        return res;
    }
}
