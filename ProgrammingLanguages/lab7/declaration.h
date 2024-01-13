#ifndef LAB7_DECLARATION_H
#define LAB7_DECLARATION_H
#include <iostream>

class PointsSystem {
private:
    int n;
    class Point {
    private:
        int x,y,mass;
    public:
        Point();
        Point(int x, int y, int mass);
        void PrintPoint();
    };

    Point *points;

public:
    PointsSystem(int n);
    PointsSystem(const PointsSystem &other);
    virtual ~PointsSystem();
    void Printer();
    //PointsSystem& operator=(const PointsSystem&);
};

#endif //LAB7_DECLARATION_H
