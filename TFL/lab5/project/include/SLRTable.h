#pragma once
#include "string"
#include "Grammar.h"
#include "unordered_map"
#include "map"
#include <iostream>

struct ExtendedRule {
    std::string LHS;
    std::vector<std::string> RHS;

    bool operator==(const ExtendedRule& rule) const{
        if (rule.LHS==LHS) {
            if (rule.RHS.size()!=RHS.size()){
                return false;
            }
            for (int i=0;i<rule.RHS.size();i++){
                if (rule.RHS[i]!=RHS[i]){
                    return false;
                }
            }
            return true;
        }
        return false;
    }
};

struct Actions {
    std::vector<int> shiftActions;
    std::vector<ExtendedRule> reduceActions;
    bool is_acc;
};


class SLRTable{
private:
    struct Comp{
        bool operator()(const std::string& lhs, const std::string& rhs) const{
            if (std::string(1,lhs[lhs.size()-1])=="'"){
                return true;
            }
            if (std::string(1,rhs[rhs.size()-1])=="'"){
                return false;
            }
            if (lhs<rhs){
                return true;
            }
            return false;
        }
    };


    Grammar inputGrammar;
    std::vector<ExtendedRule> extendedGrammarRules;
    std::map<int,std::vector<ExtendedRule>> stateDict;
    std::map<std::pair<int,std::string>,int> GOTOStateDict;
    std::vector<std::vector<std::string>> table;
    std::vector<std::string> cols;

    std::map<std::string,std::vector<std::string>,Comp> dict;

    std::string newStartToken;
    int stateCount=0;

    void processGrammar();
    std::vector<ExtendedRule> findClosure(std::vector<ExtendedRule>& inpState, std::string& tokenAfterDOT);
    void computeGOTO(int state);
    void GOTO(int state,const std::string& token);
    void generateStates();
    void createParseTable();
    std::vector<std::string> follow(const std::string& nonTerm, std::unordered_set<std::string>& used);
    std::vector<std::string> first(std::vector<std::string>& rule, std::unordered_set<std::string>& used);

    bool isBelong(ExtendedRule rule,std::vector<ExtendedRule> arr);
    std::vector<int> getKeys(std::map<int,std::vector<ExtendedRule>>);
    std::vector<std::string> findCols();
public:
    explicit SLRTable(Grammar grammar);
    SLRTable()=delete;
    void PrintTable();

    int GoTo(int state, std::string token);
    Actions GetActions(int state, std::string token);

    ~SLRTable()=default;
};
