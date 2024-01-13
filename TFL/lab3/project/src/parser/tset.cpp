#include "parser/tset.hpp"

bool operator<(const cchar& a, const cchar& b) {
    if (a.s < b.s) return true;
    if (a.s == b.s) return a.num < b.num;
    return false;
}

bool operator==(const cchar& a, const cchar& b) {
    return a.s == b.s && a.num == b.num;
}

TSet::TSet() {
    first = last = {};
    follow = {};
    e_flag = false;
    num = 0;
}

TSet::TSet(cchar a) {
    first = last = {a};
    e_flag = false;
    num = 1;
}

void TSet::plus(const TSet& arg) {
    set_union(first, arg.first);
    set_union(follow, arg.follow);
    set_union(last, arg.last);
    e_flag = e_flag || arg.e_flag;
    num += arg.num;
};

void TSet::concat(const TSet& arg) {
    if (e_flag) {
        for (cchar b : arg.first) {
            first.insert(b);
        }
    }

    for (auto ab : arg.follow) {
        follow.insert(ab);
    }

    for (cchar a : last) {
        for (cchar b : arg.first) {
            follow.insert(cpair(a, b));
        }
    }

    if (arg.e_flag) {
        for (cchar b : arg.last) {
            last.insert(b);
        }
    } else {
        last = arg.last;
    }

    e_flag = e_flag && arg.e_flag;
    num += arg.num;
};

void TSet::iter() {
    for (const cchar& a : last) {
        for (const cchar& b : first) {
            follow.insert(cpair(a, b));
        }
    }
    e_flag = true;
};

StateMachine TSet::to_machine() {
    StateMachine machine(num);
    for (cchar a : first) {
        machine.AddTransition(0, std::string(1,a.s), a.num);
    }
    for (auto ab : follow) {
        machine.AddTransition(ab.first.num, std::string(1,ab.second.s), ab.second.num);
    }
    for (cchar b : last) {
        machine.AddFinalState(b.num);
    }
    if (e_flag) {
        machine.AddFinalState(0);
    }
    return machine;
};
