#include "SLRTable.h"

#include <utility>
#include "iostream"
#include <sstream>
#include <algorithm>
#include <set>

const std::string DOT = ".";
const std::string EPSILON = "#";
const std::string NOTHING ="NOTHING";
const std::string SPEC_TOKEN="@";


SLRTable::SLRTable(Grammar  grammar):inputGrammar(std::move(grammar)){
    processGrammar();

    std::vector<ExtendedRule> tmp;

    auto I0 = findClosure(tmp,newStartToken);
    stateDict[0] = I0;

    generateStates();

    createParseTable();
}

Actions SLRTable::GetActions(int state, std::string token) {
    std::unordered_set<std::string> terms = inputGrammar.Terms();
    if (terms.find(token) == terms.end() && token != SPEC_TOKEN){
        return {};
    }

    int ind = std::find(cols.begin(), cols.end(),token) - cols.begin();
    std::string actionsString = table[state][ind];

    if (actionsString == "acc") {
        Actions ans;
        ans.is_acc = true;
        return ans;
    }

    std::vector<int> shiftActions;
    std::vector<ExtendedRule> reduceActions;

    std::stringstream ss(actionsString);
    std::string action;
    while (ss >> action) {
        if (action[0]=='S'){
            shiftActions.push_back(std::stoi(action.substr(1)));
        } else if (action[0]=='R'){
            auto rule = extendedGrammarRules[std::stoi(action.substr(1))];
            rule.RHS.erase(rule.RHS.begin());
            reduceActions.push_back(rule);
        }
    }

    return {shiftActions,reduceActions};
}

void SLRTable::PrintTable(){
    std::cout<<"\t";
    for (auto col : cols) {
        std::cout<<col<<"\t\t";
    }
    std::cout<<std::endl;
    for (int i=0;i<table.size();i++){
        for (int j=0;j<table[i].size();j++){
            if (j==0){
                std::cout<<i<<"\t"<<table[i][j]<<"\t\t";
            } else {
                std::cout<<table[i][j]<<"\t\t";
            }
        }
        std::cout<<std::endl;
    }
}

void SLRTable::processGrammar(){
    const std::string SUFFIX = "'";

    newStartToken = inputGrammar.StartToken() + SUFFIX;
    while (inputGrammar.NonTerms().find(newStartToken)!=inputGrammar.NonTerms().end()){
        newStartToken+=SUFFIX;
    }

    extendedGrammarRules.push_back(ExtendedRule {
        newStartToken,
        std::vector<std::string> {DOT,inputGrammar.StartToken()},
    });

    for (std::string rule : inputGrammar.Rules()) {
        std::stringstream ss(rule);
        std::string token;
        std::string lhs;
        std::vector<std::string> rhs {DOT};

        while (ss >> token) {
            if (lhs.empty()){
                lhs=token;
            } else if (token=="->"){
                continue;
            } else if (token == "|"){
                extendedGrammarRules.push_back(ExtendedRule{lhs,rhs});
                rhs.clear();
                rhs.push_back(DOT);
            } else {
                rhs.push_back(token);
            }
        }
        extendedGrammarRules.push_back(ExtendedRule{lhs,rhs});
    }
}

std::vector<ExtendedRule> SLRTable::findClosure(std::vector<ExtendedRule>& inpState, std::string& tokenAfterDOT) {
    std::vector<ExtendedRule> closure;

    if (tokenAfterDOT==newStartToken){
        for (const ExtendedRule& rule :extendedGrammarRules){
            if (rule.LHS==tokenAfterDOT){
                closure.push_back(rule);
            }
        }
    } else {
        closure=inpState;
    }

    int prevLen = -1;
    while(prevLen!=closure.size()) {
        prevLen = closure.size();

        std::vector<ExtendedRule> tmpClosure;

        for (ExtendedRule rule : closure) {
                if (rule.RHS[rule.RHS.size() - 1] != DOT) {
                    int dotInd = (std::find(rule.RHS.begin(),rule.RHS.end(),DOT) - rule.RHS.begin());
                    std::string dotPointsHere = rule.RHS[dotInd + 1];
                    for (const ExtendedRule &in_rule: extendedGrammarRules) {
                        if (dotPointsHere == in_rule.LHS && !isBelong(in_rule, tmpClosure)) {
                            tmpClosure.push_back(in_rule);
                        }
                    }

                }
        }
        for (const ExtendedRule& rule:tmpClosure){
            if (!isBelong(rule,closure)){
                closure.push_back(rule);
            }
        }

    }

    return closure;
}

void SLRTable::computeGOTO(int state){
    for (ExtendedRule rule : stateDict[state]) {
        std::set<std::string> generateStatesFor;

        if (rule.RHS[rule.RHS.size()-1]!=DOT){
            int dotInd = (std::find(rule.RHS.begin(),rule.RHS.end(),DOT) - rule.RHS.begin());
            std::string dotPointsHere = rule.RHS[dotInd + 1];
            if (generateStatesFor.find(dotPointsHere)==generateStatesFor.end()){
                generateStatesFor.insert(dotPointsHere);
            }
        }
        if (!generateStatesFor.empty()) {
            for (const std::string& token : generateStatesFor){
                GOTO(state,token);
            }
        }
    }
}

int SLRTable::GoTo(int state, std::string token) {
    auto element_iter = GOTOStateDict.find(std::make_pair(state,token));
    if (element_iter == GOTOStateDict.end()) {
        return -1;
    }
    return element_iter->second;
};


void SLRTable::GOTO(int state,const std::string& token) {
    std::vector<ExtendedRule> newState;

    for (ExtendedRule rule : stateDict[state]) {
        if (rule.RHS[rule.RHS.size()-1]!=DOT){
            int dotInd = (std::find(rule.RHS.begin(),rule.RHS.end(),DOT) - rule.RHS.begin());
            if ( rule.RHS[dotInd + 1] == token){
                auto shiftedRule = rule;
                shiftedRule.RHS[dotInd]=shiftedRule.RHS[dotInd + 1];
                shiftedRule.RHS[dotInd + 1] = '.';
                newState.push_back(shiftedRule);
            }
        }
    }

    std::vector<ExtendedRule> addClosureRules;
    for (ExtendedRule rule : newState){
        if (rule.RHS[rule.RHS.size()-1]!=DOT){
            int dotInd = (std::find(rule.RHS.begin(),rule.RHS.end(),DOT) - rule.RHS.begin());
            auto closureRes = findClosure(newState,rule.RHS[dotInd+1]);
            for (ExtendedRule inRule: closureRes){
                if (!isBelong(inRule,addClosureRules) && !isBelong(inRule,newState)){
                    addClosureRules.push_back(inRule);
                }
            }
        }
    }

    for (ExtendedRule rule : addClosureRules){
        newState.push_back(rule);
    }

    int stateExists = -1;
    for (std::pair<const int, std::vector<ExtendedRule>> _state : stateDict){
        if (stateDict[_state.first]==newState){
            stateExists=_state.first;
            break;
        }
    }

    if (stateExists == -1){
        stateCount++;
        stateDict[stateCount]=newState;
        GOTOStateDict[std::make_pair(state,token)]=stateCount;
    } else {
        GOTOStateDict[std::make_pair(state,token)]=stateExists;
    }
}

void SLRTable::generateStates(){
    int prevLen=-1;
    std::unordered_set<int> used;
    while(stateDict.size()!=prevLen){
        prevLen=stateDict.size();
        auto keys= getKeys(stateDict);
        for (auto key : keys){
            if (used.find(key)==used.end()){
                used.insert(key);
                computeGOTO(key);
            }
        }
    }
}

std::vector<std::string> SLRTable::findCols(){
    std::vector<std::string> rows;
    for (const auto& term : inputGrammar.Terms()){
        rows.push_back(term);
    }
    rows.push_back(SPEC_TOKEN);
    for (const auto& nonTerm : inputGrammar.NonTerms()){
        rows.push_back(nonTerm);
    }


    return rows;
}

template<typename T>
std::vector<T> slice(std::vector<T> const &v, int m, int n)
{

    auto first = v.cbegin() + m;
    auto last = v.cbegin() + n + 1;

    std::vector<T> vec(first, last);
    return vec;
}

std::vector<std::string> strToVect(const std::string& str){
    std::stringstream ss(str);
    std::vector<std::string> ans;
    std::string token;

    while (ss >> token) {
        ans.push_back(token);
    }
    return ans;
}

std::vector<std::string> SLRTable::first(std::vector<std::string>& rule, std::unordered_set<std::string>& used){
    if (rule[0]!=NOTHING && !rule.empty()) {
        if (rule[0]==EPSILON) {
            return {EPSILON};
        }
        if (inputGrammar.Terms().find(rule[0])!=inputGrammar.Terms().end()){
            return {rule[0]};
        }

        if (dict.find(rule[0])!=dict.end()){
            std::vector<std::string> res;
            if (used.find(rule[0])==used.end()){
                used.insert(rule[0]);
                auto rhs = dict[rule[0]];
                for (std::string subRule : rhs){
                    auto subRuleVect= strToVect(subRule);
                    auto inRes= first(subRuleVect,used);
                    if (!inRes.empty() && inRes[0]!=NOTHING){
                        for (auto t : inRes){
                            res.push_back(t);
                        }
                    }
                }
            }
            if (std::find(res.begin(), res.end(),EPSILON)==res.end()){
                return res;
            }
            res.erase(std::remove(res.begin(), res.end(), EPSILON), res.end());
            if (res.size()>1){
                auto sliced =slice(rule,1,rule.size()-1);
                auto ansNew = first(sliced,used);
                if (!ansNew.empty() && ansNew[0]!=NOTHING){
                    res.insert( res.end(), ansNew.begin(), ansNew.end());
                }
                return res;
            }
            res.push_back(EPSILON);
            return res;
        }
    }
    return {NOTHING};
}

std::vector<std::string> setToVect(std::unordered_set<std::string> s ){
    std::vector<std::string> res;
    for (auto i : s){
        res.push_back(i);
    }
    return res;
}

std::vector<std::string> SLRTable::follow(const std::string& nonTerm, std::unordered_set<std::string>& used) {
    std::unordered_set<std::string> solSet;
    if (nonTerm==newStartToken){
        solSet.insert(SPEC_TOKEN);
    }
    used.insert(nonTerm);

    for (auto cur : dict){
        std::string curNonTerm = cur.first;
        auto rhs = cur.second;
        for (std::string subRule : rhs) {
            auto subRuleVect= strToVect(subRule);

            while (std::find(subRuleVect.begin(), subRuleVect.end(),nonTerm)!=subRuleVect.end()) {
                auto nonTermInd = std::find(subRuleVect.begin(), subRuleVect.end(),nonTerm);
                subRuleVect = slice(subRuleVect,nonTermInd-subRuleVect.begin()+1,subRuleVect.size()-1);
                std::vector<std::string> firstRes;
                if (!subRuleVect.empty()){
                    std::unordered_set<std::string> used1;
                    firstRes=first(subRuleVect,used1);
                    if (std::find(firstRes.begin(), firstRes.end(),EPSILON)!=firstRes.end()){
                        firstRes.erase(std::remove(firstRes.begin(), firstRes.end(), EPSILON), firstRes.end());
                        auto ansNew = follow(curNonTerm,used);
                        firstRes.insert(firstRes.end(), ansNew.begin(), ansNew.end());
                    }
                } else {
                    if (nonTerm!=curNonTerm && used.find(curNonTerm)==used.end()){
                        firstRes = follow(curNonTerm,used);
                    }
                }
                if (!firstRes.empty() && firstRes[0]!=NOTHING){
                    for (auto t : firstRes) {
                        solSet.insert(t);
                    }
                }
            }
        }
    }
    auto v = setToVect(solSet);
    return v;
}

void SLRTable::createParseTable() {
    std::vector<std::string> v(inputGrammar.NonTerms().size()+inputGrammar.Terms().size()+1);
    table = std::vector<std::vector<std::string>> (stateCount+1,v);

    auto nonTerms= inputGrammar.NonTerms();
    auto terms= inputGrammar.Terms();

    cols = findCols();

    for (auto entry : GOTOStateDict){
        int state = entry.first.first;
        std::string token = entry.first.second;
        int col = std::find(cols.begin(), cols.end(),token) - cols.begin();
        if (nonTerms.find(token)!=nonTerms.end()){
            table[state][col]+= std::to_string(GOTOStateDict[entry.first]);
        }
        if (terms.find(token)!=terms.end()) {
            table[state][col]+= "S"+std::to_string(GOTOStateDict[entry.first])+" ";
        }
    }

    std::map<int,ExtendedRule> processed;
    int c=0;
    for (ExtendedRule rule : extendedGrammarRules){
        auto tmpRule=rule;
        tmpRule.RHS.erase(std::find(tmpRule.RHS.begin(), tmpRule.RHS.end(),DOT));
        processed[c]=tmpRule;
        c++;
    }

    std::string addedRule = extendedGrammarRules[0].LHS + " -> " + extendedGrammarRules[0].RHS[1];
    auto rules= inputGrammar.Rules();
    rules.emplace(rules.cbegin(),addedRule);
    for (auto rule : rules){
        std::stringstream ss(rule);
        std::string token;
        std::string lhs;
        std::string rhs;
        std::vector<std::string> multirhs;

        while (ss >> token) {
            if (lhs.empty()){
                lhs=token;
            } else if (token=="->"){
                continue;
            } else if (token == "|"){
                rhs.pop_back();
                multirhs.push_back(rhs);
                rhs.clear();
            } else {
                rhs+=token+" ";
            }
        }
        rhs.pop_back();
        multirhs.push_back(rhs);
        if (dict.find(lhs)!=dict.end()){
            dict[lhs].insert(dict[lhs].end(),multirhs.begin(),multirhs.end());
        } else {
            dict[lhs]=multirhs;
        }
    }

    for (auto state : stateDict){
       int stateNum=state.first;
       auto rules = state.second;
       for (auto rule:rules){
           if (rule.RHS[rule.RHS.size()-1]==DOT){
               auto tmpRule = rule;
               tmpRule.RHS.erase(std::find(tmpRule.RHS.begin(), tmpRule.RHS.end(),DOT));
               for (auto proc : processed){
                   if (proc.second==tmpRule){
                       std::unordered_set<std::string> used;
                       auto followRes= follow(rule.LHS,used);
                       for (auto col : followRes) {
                           int ind = std::find(cols.begin(), cols.end(),col)-cols.begin();
                           if (proc.first==0){
                               table[stateNum][ind]="acc";
                           } else {
                               table[stateNum][ind]+="R"+std::to_string(proc.first)+" ";
                           }
                       }
                   }
               }
           }
       }
    }
}

std::vector<int> SLRTable::getKeys(std::map<int,std::vector<ExtendedRule>> map) {
    std::vector<int> keys;
    for (const auto& record :map){
        keys.push_back(record.first);
    }
    return keys;
}

bool SLRTable::isBelong(ExtendedRule rule,std::vector<ExtendedRule> arr){
    for (const ExtendedRule& elem : arr){
        if (rule==elem){
            return true;
        }
    }
    return false;
}
