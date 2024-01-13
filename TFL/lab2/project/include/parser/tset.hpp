/*
Совокупность множеств First, Follow и Last регулярного выражения
*/
#pragma once
#include <set>
#include <utility>

#include "StateMachine.h"

// Нумерованный символ в линеаризации рв
struct cchar {
    char s;
    int num;
};

bool operator<(const cchar& a, const cchar& b);
bool operator==(const cchar& a, const cchar& b);

template <class T>
std::set<T>& set_union(std::set<T>& A, const std::set<T>& B) {
    for (T b : B) {
        A.insert(b);
    }
    return A;
}

using std::pair;
using cpair = pair<cchar, cchar>;
using cset = std::set<cchar>;
using dset = std::set<cpair>;

struct TSet {
    cset first;
    dset follow;
    cset last;
    bool e_flag;
    int num;

    TSet();
    TSet(cchar a);

    // Отражение операций над рв в множествах
    void plus(const TSet& arg);
    void concat(const TSet& arg);
    void iter();

    // Строит автомат Глушкова
    StateMachine to_machine();
};
