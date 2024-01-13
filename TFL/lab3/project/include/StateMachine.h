#pragma once
#include <iostream>
#include <vector>
#include <string>
#include <unordered_set>
#include <stack>
#include <list>

class StateMachine {
    friend void dfs (int v, std::unordered_set<int>& globalUsed, std::vector<char>& curUsed,
                     StateMachine& automata);
private:
    int stateCount;
    std::vector<std::vector<std::string>> transitions;
    std::unordered_set<int> finalStates;

    bool dfs (int v, std::vector<int>& colors);
    void FindPathsDfs(int v, int target,
                   std::vector<bool> visited, std::vector<std::string>& path,
                   std::unordered_set<std::string>& dest) const;
public:
    StateMachine(){
        transitions=std::vector<std::vector<std::string>>();
        stateCount=0;
        finalStates=std::unordered_set<int>();
    }
    explicit StateMachine(int statesNum);

    StateMachine(std::vector<std::vector<std::string>>& transitions1,std::unordered_set<int>& finalStates1,
                 int statesCount1);

    void AddTransition(int curState, const std::string& curSignal, int nextState);
    void SetFinalStates(std::unordered_set<int> final);
    void AddFinalState(int);
    [[nodiscard]] int GetStateNum() const;
    std::unordered_set<int> GetFinalStates() const;
    std::vector<std::vector<std::string>> GetTransitions() const;
    bool IsWordBelong(const std::string& word);
    bool IsFinal(int state);
    bool IsAnyCycle();
    void FixStates();

    // Возвращает все слова вдоль путей из v в какое-либо состояние из targets
    std::unordered_set<std::string> FindPaths(int source, const std::unordered_set<int>& targets) const;
    std::unordered_set<std::string> FindCycles(int vertex) const;

    static void To_Graph(StateMachine& M, std::ostream& out);

    ~StateMachine()=default;

    // Функции нужные для парсера из лабы 2
    static StateMachine ConcatStateMachines(const StateMachine& stM1, const StateMachine& stM2);
    static StateMachine IntersectStateMachines(const StateMachine& stM1, const StateMachine& stM2);
    static StateMachine UnionStateMachines(const StateMachine& stM1, const StateMachine& stM2);
};
