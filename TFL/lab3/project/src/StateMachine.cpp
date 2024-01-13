#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <queue>
#include "StateMachine.h"
#include <sstream>
#include <set>
#include <list>
#include <algorithm>

StateMachine::StateMachine(int statesNum){
    std::vector<std::string> v(statesNum+1);
    transitions=std::vector<std::vector<std::string>> (statesNum+1, v);
    finalStates=std::unordered_set<int>();
    stateCount = statesNum;
}

StateMachine::StateMachine(std::vector<std::vector<std::string>>& transitions1,std::unordered_set<int>& finalStates1,
                           int statesCount1){
    transitions=transitions1;
    finalStates=finalStates1;
    stateCount=statesCount1;
};

bool StateMachine::IsFinal(int state) {
    return finalStates.find(state) != finalStates.end();
}

int StateMachine::GetStateNum() const{
    return stateCount;
}

std::unordered_set<int> StateMachine::GetFinalStates() const {
    return finalStates;
}

std::vector<std::vector<std::string>> StateMachine::GetTransitions() const {
    return transitions;
}

void StateMachine::AddTransition(int curState, const std::string& curSignal, int nextState){
    transitions[curState][nextState]=curSignal;
}

void StateMachine::SetFinalStates(std::unordered_set<int> final) {
    finalStates=std::move(final);
}

void StateMachine::To_Graph(StateMachine& M, std::ostream& out) {
    out << "digraph {" << std::endl;
    out << 0 << " [color=\"green\"]" << std::endl;
    for (int state : M.finalStates) {
        out << state << " [peripheries = 2]" << std::endl;
    }
    if (M.stateCount!=-1){
        for (int from = 0; from < M.stateCount+1; ++from) {
            const std::vector<std::string>& from_trans = M.transitions[from];
            for (int to = 0; to < M.stateCount+1; ++to) {
                if (!from_trans[to].empty()) {
                    std::stringstream ss(from_trans[to]);
                    std::string word;
                    while (ss >> word) { // Extract word from the stream.
                        out << from << " -> " << to << " [label=\"" << word
                            << "\"]" << std::endl;
                    }
                }
            }
        }
    }
    out << "}" << std::endl;
};

void StateMachine::AddFinalState(int a) { finalStates.insert(a); };

bool StateMachine::IsWordBelong(const std::string& word){
    if (stateCount == 0 && finalStates.empty()) {
        return false;
    }
    std::unordered_set<int> curState = {0};
    for (char ch : word) {
        std::unordered_set<int> nStates = {};
        for (int state : curState) {
            std::vector<std::string>& trans = transitions[state];
            for (int i = 0; i < trans.size(); ++i) {
                std::stringstream ss(trans[i]);
                std::string word1;
                while (ss >> word1) {
                    if (std::string(1,ch) == word1 || word1 == std::string(1,'.')) {
                        nStates.insert(i);
                    }
                }
            }
        }
        if (nStates.empty()) {
            return false;
        }
        curState = nStates;
    }
    for (int state : curState) {
        if (IsFinal(state)) {
            return true;
        }
    }
    return false;
}

bool StateMachine::dfs (int v, std::vector<int>& colors) {
    colors[v] = 1; // grey
    for (int i=0;i<transitions.size();i++){
        if (!transitions[v][i].empty()){
            if (colors[i]==0){ // white
                if (dfs(i,colors)){
                    return true;
                }
            }
            if (colors[i]==1){ // grey
                return true;
            }
        }
    }
    colors[v]=2; // black
    return false;
}

bool StateMachine::IsAnyCycle(){
    // 0-white, 1-grey, 2-black
    std::vector<int> colors(stateCount+1);
    return dfs(0,colors);
}

void unblock(int v,std::vector<bool>& blocked,std::vector<std::list<int>>&b){
    blocked[v] = false;

    while (!b[v].empty()) {
        int W = b[v].front();
        b[v].pop_front();

        if (blocked[W]) {
            unblock(W,blocked,b);
        }
    }
}

// Возвращает все слова, раскрывая альтернативы, которые встречаются в пути
std::vector<std::string> get_words_by_path(std::vector<std::string>& path) {
    std::vector<std::string> words(1, "");
    for (int i = 0; i < path.size(); ++i) {
        const std::string& t_string = path[i];
        if (t_string.length() == 1) {
            for (std::string& word : words) {
                word += t_string;
            }
        } else {
            std::stringstream trans_stream(t_string);
            std::vector<char> trans_labels;
            char label = 0;
            while (trans_stream >> label) {
                trans_labels.push_back(label);
            }
            int original_size = words.size();
            for (int j = 0; j < original_size; ++j) {
                for (int k = 1; k < trans_labels.size(); ++k) {
                    words.push_back(words[j] + trans_labels[k]);
                }
                words[j] += trans_labels[0];
            }
        }
    }
    return words;
}

void StateMachine::FindPathsDfs(int v, int target, std::vector<bool> visited,
                                std::vector<std::string>& path,
                                std::unordered_set<std::string>& dest) const {
    visited[v] = true;
    if (v == target) {
        std::vector<std::string> words = get_words_by_path(path);
        for (std::string& word : words) {
            dest.insert(word);
        }
    } else {
        const std::vector<std::string>& v_trans = transitions[v];
        for (int i = 0; i < transitions.size(); ++i) {
            std::string t_string = v_trans[i];
            if (!t_string.empty() && !visited[i]) {
                path.push_back(t_string);
                FindPathsDfs(i, target, visited, path, dest);
            }
        }
    }
    path.pop_back();
    visited[v] = false;
    return;
};

std::unordered_set<std::string> StateMachine::FindCycles(int vertex) const {
    std::unordered_set<std::string> dest;
    std::vector<bool> visited(GetStateNum() + 1, false);
    std::vector<std::string> path;
    const std::vector<std::string>& v_trans = transitions[vertex];
    for (int i = 0; i < transitions.size(); ++i) {
        std::string t_string = v_trans[i];
        if (!t_string.empty() && !visited[i]) {
            path.push_back(t_string);
            FindPathsDfs(i, vertex, visited, path, dest);
        }
    }
    return dest;
}


std::unordered_set<std::string> StateMachine::FindPaths(
    int source, const std::unordered_set<int>& targets) const {
    std::unordered_set<std::string> dest;
    std::vector<bool> visited(GetStateNum() + 1, false);
    for (int target : targets) {
        if (source == target) {
            dest.insert("");
            continue;
        }
        visited[source] = true;
        std::vector<std::string> path;
        const std::vector<std::string>& v_trans = transitions[source];
        for (int i = 0; i < transitions.size(); ++i) {
            std::string t_string = v_trans[i];
            if (!t_string.empty() && !visited[i]) {
                path.push_back(t_string);
                FindPathsDfs(i, target, visited, path, dest);
            }
        }
        visited[source] = false;
    }
    return dest;
}

void dfs (int v, std::unordered_set<int>& globalUsed, std::vector<char>& curUsed, StateMachine& automata) {
    curUsed[v] = true;
    globalUsed.insert(v);
    for (int i=0;i<automata.transitions.size();i++){
        if (automata.transitions[i][v]!="" && !curUsed[i]){
            dfs(i,globalUsed,curUsed,automata);
        }
    }
}

void StateMachine::FixStates(){
    std::unordered_set<int> globalUsed;
    for (auto finalState: finalStates){
        auto it=globalUsed.find(finalState);
        if (it==globalUsed.end()){
            std::vector<char> curUsed(stateCount+1);
            ::dfs(finalState,globalUsed,curUsed, *this);
        }
    }
    for (int i=transitions.size()-1;i>0;i--){
        auto it = globalUsed.find(i);
        if (it==globalUsed.end()){
            for(auto& row:transitions) row.erase(next(row.begin(), i));
            transitions.erase(transitions.begin()+i);
            stateCount--;
            std::set<int> needToChange;
            for (auto finalState: finalStates){
                if (finalState>i){
                    needToChange.insert(finalState);
                }
            }
            for (auto j : needToChange){
                finalStates.erase(j);
                finalStates.insert(j-1);
            }
        }
    }
}

// Функции нужные для парсера из лабы 2

StateMachine StateMachine::ConcatStateMachines(const StateMachine& stM1, const StateMachine& stM2) {
    if ((stM1.stateCount==0 && stM1.finalStates.empty()) || (stM2.stateCount==0 && stM2.finalStates.empty())) {
        return {};
    }
    if (stM1.stateCount==0 && !stM1.finalStates.empty()) {
        return stM2;
    }
    if (stM2.stateCount==0 && !stM2.finalStates.empty()) {
        return stM1;
    }
    StateMachine res(stM1.GetStateNum()+stM2.GetStateNum());
    for (int i=0;i<stM1.transitions.size();i++){
        for (int j=0;j<stM1.transitions[i].size();j++){
            if (stM1.transitions[i][j]!=""){
                res.AddTransition(i,stM1.transitions[i][j],j);
            }
        }
    }
    for (auto finalState:stM1.finalStates){
        for (int j=0;j<stM2.transitions[0].size();j++){
            if (stM2.transitions[0][j]!=""){
                res.AddTransition(finalState,stM2.transitions[0][j],j+stM1.GetStateNum());
            }
        }
    }
    for (int i=1;i<stM2.transitions.size();i++){
        for (int j=0;j<stM2.transitions[i].size();j++){
            if (stM2.transitions[i][j]!=""){
                res.AddTransition(i+stM1.GetStateNum(),stM2.transitions[i][j],j+stM1.GetStateNum());
            }
        }
    }
    std::unordered_set<int> newFinalStates;
    for (auto finalState:stM2.finalStates){
        if (finalState==0 ) {
            for (auto finalState1:stM1.finalStates) {
                newFinalStates.insert(finalState1);
            }
        }
        newFinalStates.insert(finalState+stM1.GetStateNum());
    }
    res.SetFinalStates(newFinalStates);
    return res;
}

StateMachine StateMachine::IntersectStateMachines(const StateMachine& stM1, const StateMachine& stM2) {
    if ((stM1.stateCount==0 && stM1.finalStates.empty()) || (stM2.stateCount==0 && stM2.finalStates.empty())) {
        return {};
    }
    std::unordered_map<int,std::pair<int,int>> Q;
    std::queue<int> que;
    StateMachine res(stM1.GetStateNum()*stM2.GetStateNum());
    std::unordered_set<int> newFinalStates;
    Q[0]=std::make_pair(0,0);
    que.push(0);
    int stateCount=0;
    auto it1 = stM1.finalStates.find(0);
    auto it2 = stM2.finalStates.find(0);
    if (it1 != stM1.finalStates.end() && it2!=stM2.finalStates.end()) {
        newFinalStates.insert(stateCount);
    }
    while (!que.empty()){
        auto curStateCount=que.front();
        auto curState=Q[que.front()];
        que.pop();
        for (int i=0;i<stM1.transitions[curState.first].size();i++){
            std::string curTransStM1=stM1.transitions[curState.first][i];
            if (curTransStM1=="."){
                for (int j=0;j<stM2.transitions[curState.second].size();j++){
                    std::string curTransStM2=stM2.transitions[curState.second][j];
                    if (curTransStM2!=""){
                        bool f= false;
                        for (auto state : Q) {
                            if (state.second.first==i && state.second.second==j){
                                res.AddTransition(curStateCount,curTransStM2,state.first);
                                f= true;
                                break;
                            }
                        }
                        if (f){
                            continue;
                        }
                        stateCount++;
                        Q[stateCount]=std::make_pair(i,j);
                        auto it1 = stM1.finalStates.find(i);
                        auto it2 = stM2.finalStates.find(j);
                        if (it1 != stM1.finalStates.end() && it2!=stM2.finalStates.end()){
                            newFinalStates.insert(stateCount);
                        }
                        que.push(stateCount);
                        res.AddTransition(curStateCount,curTransStM2,stateCount);
                    }
                }
            } else if (curTransStM1!=""){
                for (int j=0;j<stM2.transitions[curState.second].size();j++){
                    auto curTransStM2=stM2.transitions[curState.second][j];
                    if (curTransStM2==curTransStM1 || curTransStM2 == "."){
                        bool f= false;
                        for (auto state : Q) {
                            if (state.second.first==i && state.second.second==j){
                                res.AddTransition(curStateCount,curTransStM1,state.first);
                                f= true;
                                break;
                            }
                        }
                        if (f){
                            continue;
                        }
                        stateCount++;
                        Q[stateCount]=std::make_pair(i,j);
                        auto it1 = stM1.finalStates.find(i);
                        auto it2 = stM2.finalStates.find(j);
                        if (it1 != stM1.finalStates.end() && it2!=stM2.finalStates.end()){
                            newFinalStates.insert(stateCount);
                        }
                        que.push(stateCount);
                        res.AddTransition(curStateCount,curTransStM1,stateCount);
                    }
                }
            }
        }
    }
    res.SetFinalStates(newFinalStates);
//    // Удаление лишнего
    res.FixStates();
    return res;
}

StateMachine StateMachine::UnionStateMachines(const StateMachine& stM1, const StateMachine& stM2) {
    StateMachine res(stM1.GetStateNum()+stM2.GetStateNum());
    for (int i=0;i<stM1.transitions.size();i++){
        for (int j=0;j<stM1.transitions[i].size();j++){
            if (stM1.transitions[i][j]!=""){
                res.AddTransition(i,stM1.transitions[i][j],j);
            }
        }
    }
    for (int i=0;i<stM2.transitions.size();i++){
        for (int j=0;j<stM2.transitions[i].size();j++){
            if (stM2.transitions[i][j]!=""){
                if (i==0){
                    res.AddTransition(i,stM2.transitions[i][j],j+stM1.GetStateNum());
                } else {
                    res.AddTransition(i+stM1.GetStateNum(),stM2.transitions[i][j],j+stM1.GetStateNum());
                }
            }
        }
    }
    std::unordered_set<int> newFinalStates;
    for (auto finalState:stM1.finalStates){
        newFinalStates.insert(finalState);
    }
    for (auto finalState:stM2.finalStates){
        if (finalState==0) newFinalStates.insert(finalState);
        newFinalStates.insert(finalState+stM1.GetStateNum());
    }
    res.SetFinalStates(newFinalStates);
    return res;
}
