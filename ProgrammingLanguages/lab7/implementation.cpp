#include "declaration.h"
#include <iostream>
using namespace std;
//Конструктор класса Point
PointsSystem::Point::Point(int x,int y, int mass){
    this->x=x;
    this->y=y;
    this->mass=mass;
}
PointsSystem::Point::Point() {}
//Конструктор класса PointsSystem
PointsSystem::PointsSystem(int n) {
    this->n=n;
    this->points=new Point[n];
    for (int i=0;i<n;i++){
        points[i]=Point(i,i+5,i+i+5);
    }
}
PointsSystem::~PointsSystem() {
    delete[] this->points;
    //cout<<"Im here"<<endl;
}
void PointsSystem::Point::PrintPoint() {
    cout<<x<<"  "<<y<<"  "<<mass<<"  "<<endl;
}
void PointsSystem::Printer() {
    for (int i=0;i<n;i++){
        points[i].PrintPoint();
    }
}
PointsSystem::PointsSystem(const PointsSystem &other){
    this->n=other.n;
    this->points=new Point[other.n];
    for (int i=0;i<other.n;i++){
        this->points[i]=other.points[i];
    }
}
/*
Point::~Point(){
    delete mass;
}*/



