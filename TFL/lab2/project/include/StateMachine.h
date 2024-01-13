#ifndef TFL_LABS_STATEMACHINE_H
#define TFL_LABS_STATEMACHINE_H
#include <iostream>
#include <vector>
#include <string>
#include <unordered_set>

class StateMachine{
    friend void dfs (int v, std::unordered_set<int>& globalUsed, std::vector<char>& curUsed,
                     StateMachine& automata);
    friend class StringGenerator;
    friend void fixStates(StateMachine& res);
private:
    int stateCount;
    std::vector<std::vector<char>> transitions;
    std::unordered_set<int> finalStates;

public:
    StateMachine(){
        transitions=std::vector<std::vector<char>>();
        stateCount=0;
        finalStates=std::unordered_set<int>();
    }
    explicit StateMachine(int statesNum);

    bool Determine(const std::string& word);

    StateMachine(std::vector<std::vector<char>>& transitions1,std::unordered_set<int>& finalStates1,
                 int statesCount1);

    void AddTransition(int curState, char curSignal, int nextState);
    void SetFinalStates(std::unordered_set<int> final);
    void AddFinalState(int);
    [[nodiscard]] int GetStateNum() const;
    static StateMachine ConcatStateMachines(const StateMachine& stM1, const StateMachine& stM2);
    static StateMachine IntersectStateMachines(const StateMachine& stM1, const StateMachine& stM2);
    static StateMachine UnionStateMachines(const StateMachine& stM1, const StateMachine& stM2);
    std::string ConvertToRegularExpr();

    static void To_Graph(const StateMachine& M, std::ostream& out);

    ~StateMachine()=default;
};

#endif //TFL_LABS_STATEMACHINE_H
