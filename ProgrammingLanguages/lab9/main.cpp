#include <iostream>
#include <vector>
using namespace std;
template <typename T>
class Relation {
private:
    unsigned int max(unsigned int a, unsigned int b) {
        if (a > b) {
            return a;
        }
        else {
            return b;
        }
    }
    unsigned int min(unsigned int a, unsigned int b) {
        if (a < b) {
            return a;
        }
        else {
            return b;
        }
    }
    class Pair {
    private:
        T x, y;
    public:
        Pair() {}
        Pair(T x, T y) {
            this->x = x;
            this->y = y;
        }
        T get_x() {
            return x;
        }
        T get_y() {
            return y;
        }
        void print_pair() {
            cout << "(" << x << "," << y << ") ";
        }
        bool operator ==(Pair& other) {
            if ((x == other.get_x()) && (y == other.get_y())) {
                return true;
            }
            return false;
        }
    };
    vector <Pair> all_rel;
public:
    Relation(){}
    Relation(T input_set[], bool(*relation)(T, T), int n) {
        all_rel = vector <Pair>();
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (relation(input_set[i], input_set[j])) {
                    all_rel.push_back(Pair(input_set[i], input_set[j]));
                }
            }
        }
    }
    Relation<T> operator +(const Relation<T>& other) {
        Relation<T> res;
        res.all_rel.insert(res.all_rel.end(), all_rel.begin(), all_rel.end());
        res.all_rel.insert(res.all_rel.end(), other.all_rel.begin(), other.all_rel.end());
        cout << res.all_rel[2].get_x();
        return res;
    }
    Relation<T> operator* (Relation<T>& other) {
        Relation<T> res;
        for (unsigned int i = 0; i < max(all_rel.size(), other.all_rel.size()); i++){
            for (unsigned int j = 0; j < min(all_rel.size(), other.all_rel.size()); j++) {
                if (all_rel[i] == other.all_rel[j]){
                    res.all_rel.push_back(all_rel[i]);
                }
            }
        }
        return res;
    }
    void print_relation() {
        cout << "{ ";
        for (unsigned int i = 0; i < all_rel.size(); i++) {
            all_rel[i].print_pair();
        }
        cout << "}";
    }

};

bool comare1(int x, int y) {
    if (x > y) {
        return true;
    }
    return false;
}

bool comare2(int x, int y) {
    if (x == (y + 1)) {
        return true;
    }
    return false;
}

int main(int argc, char* argv[]) {
    int arr[]{ 1,2,3,4 };
    Relation<int> rel1(arr, comare1, atoi(argv[1]));
    Relation<int> rel2(arr, comare2,atoi(argv[1]));
    rel1.print_relation();
    cout << endl;
    rel2.print_relation();
    Relation<int> summer = rel1 * rel2;
    cout << endl;
    summer.print_relation();
    cout << endl;
    rel1.print_relation();
    cout << endl;
    rel2.print_relation();
    return 0;
}