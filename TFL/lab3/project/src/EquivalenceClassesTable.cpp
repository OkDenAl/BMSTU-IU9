#include "EquivalenceClassesTable.h"
#include <fstream>
#include <utility>
#include "Constant.h"

EquivalenceClassesTable::EquivalenceClassesTable(std::string& alphabet, IMAT& MAT)
:alphabet(alphabet),MAT(MAT){
    mainTable[""]="";
    suffixes.emplace_back("");
}

void EquivalenceClassesTable::updateClassesOfEquivalence(){
    classesOfEquivalence.clear();
    for (const auto& equivClass:mainTable){
        if (classesOfEquivalence.find(equivClass.second)==classesOfEquivalence.end()){
            classesOfEquivalence[equivClass.second]=equivClass.first;
        }
    }
}

void EquivalenceClassesTable::fillRecognitionStringForAdditionalTable(const std::string& curPrefix,const std::string& mode){
    int alreadyFilled = additionalTable[curPrefix].length();
    for (int i=alreadyFilled;i<suffixes.size();i++){
        std::string curWord = curPrefix+suffixes[i];
        if (MAT.IsMembership(curWord,mode)){
            additionalTable[curPrefix]+="+";
        }else {
            additionalTable[curPrefix]+="-";
        }
    }
}

void EquivalenceClassesTable::fillRecognitionStringForMainTable(const std::string& curPrefix,const std::string& mode){
    int alreadyFilled = mainTable[curPrefix].length();
    for (int i=alreadyFilled;i<suffixes.size();i++){
        std::string curWord = curPrefix+suffixes[i];
        if (MAT.IsMembership(curWord,mode)){
            mainTable[curPrefix]+="+";
        }else {
            mainTable[curPrefix]+="-";
        }
    }
}

void EquivalenceClassesTable::AddNewKeysToAdditionalTable(){
    for (const auto& equivClass : mainTable){
        for (char symb : alphabet){
            std::string curPrefix=equivClass.first+std::string(1,symb);
            if (additionalTable.find(curPrefix)==additionalTable.end()){
                additionalTable[curPrefix]="";
            }
        }
    }
}

void EquivalenceClassesTable::FillMainTable(const std::string& mode){
    for (const auto& equivClass : mainTable){
        fillRecognitionStringForMainTable(equivClass.first,mode);
    }
}

void EquivalenceClassesTable::FillAdditionalTable(const std::string& mode){
    for (const auto& equivClass : additionalTable){
        fillRecognitionStringForAdditionalTable(equivClass.first,mode);
    }
}

void EquivalenceClassesTable::AddWordAndItsPrefixesToMainTable(std::string & word,const std::string& mode){
    std::string curPrefix;
    for (char symb : word){
        curPrefix+=std::string(1,symb);
        if (mainTable.find(curPrefix)==mainTable.end()){
            mainTable[curPrefix]="";
            fillRecognitionStringForMainTable(curPrefix,mode);
        }
    }
}

void EquivalenceClassesTable::MakeComplete(const std::string& mode){
    for (const auto& elem : additionalTable) {
        if (classesOfEquivalence.find(elem.second)==classesOfEquivalence.end()){
            mainTable[elem.first]=elem.second;
            classesOfEquivalence[elem.second]=elem.first;
            fillRecognitionStringForMainTable(elem.first,mode);
            AddNewKeysToAdditionalTable();
            FillAdditionalTable(mode);
        }
    }
}

bool EquivalenceClassesTable::MakeConsistent(const std::string& mode){
    for (auto elem1 : additionalTable){
        for (auto elem2 : additionalTable){
            if (elem1==elem2 || elem1.first[elem1.first.size()-1]!=elem2.first[elem2.first.size()-1]){
                continue;
            }
            auto word1=elem1.first;
            auto word2=elem2.first;
            word1.pop_back();
            word2.pop_back();
            if (word1==word2){
                continue;
            }
            if ((elem1.second==elem2.second) != (mainTable[word1]==mainTable[word2])){
                if (mainTable[word1]==mainTable[word2]){
//                    std::cout<<word1<<"\t"<<word2<<std::endl;
                    for (int i=0;i<mainTable[word1].size();i++){
                        int p=rand()%suffixes.size();
                        if (mainTable[word1][i]!=elem2.second[i] || mainTable[word1][i]!=elem1.second[i]){
                            suffixes.push_back(elem1.first[elem1.first.size()-1]+suffixes[p]);
                            FillMainTable(mode);
                            FillAdditionalTable(mode);
                            updateClassesOfEquivalence();
                            return true;
                        }
                    }
                }
            }
        }
    }
    return false;
}


StateMachine EquivalenceClassesTable::BuildDFA() {
    int countOfStates=-1;
    std::unordered_set<int> finalStates;
    std::unordered_map<std::string,int> prefixToStateNumDict;
    std::unordered_set<std::string> used;
    for (auto elem:mainTable){
        if (used.find(elem.second)==used.end()){
            countOfStates++;
            used.insert(elem.second);
            prefixToStateNumDict[elem.first]=countOfStates;
            if (elem.second[0]=='+'){
                finalStates.insert(countOfStates);
            }
        }
    }
    std::vector<std::string> v(countOfStates+1);
    std::vector<std::vector<std::string>> transitions(countOfStates+1, v);
    for (const auto& elem: prefixToStateNumDict){
        for (char letter: alphabet){
            auto nextState=elem.first+std::string(1,letter);
            auto from=elem.second;
            auto to = prefixToStateNumDict[classesOfEquivalence[additionalTable[nextState]]];
            if (transitions[from][to].empty()){
                transitions[from][to]=std::string(1,letter);
            } else {
                transitions[from][to]+=" "+std::string(1,letter);
            }
        }
    }
    return {transitions,finalStates,countOfStates};
}

std::unique_ptr<StateMachine> EquivalenceClassesTable::LStar
(std::string& alphabet,int maxNumOfEquivClasses, int maxTryCount, IMAT& MAT, const std::string& mode){
    EquivalenceClassesTable equivTable(alphabet, MAT);
    equivTable.fillRecognitionStringForMainTable("",mode);
    StateMachine DFA;
    int i=0;
    while (true){
        equivTable.updateClassesOfEquivalence();
        if (equivTable.classesOfEquivalence.size()>=maxNumOfEquivClasses){
            return nullptr;
        }
        equivTable.AddNewKeysToAdditionalTable();
        equivTable.FillAdditionalTable(mode);

        bool f = true;
        while (f) {
            equivTable.MakeComplete(mode);
            f = equivTable.MakeConsistent(mode);
        }

        DFA = equivTable.BuildDFA();
        i++;

        auto verdict=equivTable.MAT.IsEqual(DFA,alphabet,maxTryCount,mode);
        if (verdict=="equal"){
            break;
        }
        equivTable.AddWordAndItsPrefixesToMainTable(verdict,mode);
    }
    return std::make_unique<StateMachine>(DFA);
}
