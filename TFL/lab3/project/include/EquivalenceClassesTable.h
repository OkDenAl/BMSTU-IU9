#pragma once
#include <unordered_map>
#include <map>
#include <string>
#include <vector>
#include <memory>
#include "IMAT.h"

class EquivalenceClassesTable{
private:
    struct custom_string_comparer
    {
        bool operator()(const std::string& s1, const std::string& s2) const
        {
            if (s1.length()<s2.length()){
                return true;
            }
            if (s1.length()>s2.length()){
                return false;
            }
            return s1<s2 ;
        }
    };


    std::vector<std::string> suffixes;
    std::map<std::string,std::string,custom_string_comparer> mainTable;
    std::map<std::string,std::string,custom_string_comparer> additionalTable;
    std::unordered_map<std::string,std::string> classesOfEquivalence;
    std::string alphabet;
    IMAT& MAT;

    void updateClassesOfEquivalence();
    void fillRecognitionStringForAdditionalTable(const std::string& curPrefix,const std::string& mode);
    void fillRecognitionStringForMainTable(const std::string& curPrefix,const std::string& mode);
public:
    explicit EquivalenceClassesTable(std::string& alphabet, IMAT& MAT);

    StateMachine BuildDFA();

    void AddNewKeysToAdditionalTable();
    void FillMainTable(const std::string& mode);
    void FillAdditionalTable(const std::string& mode);
    void AddWordAndItsPrefixesToMainTable(std::string & word,const std::string& mode);
    void MakeComplete(const std::string& mode);
    bool MakeConsistent(const std::string& mode);

    static std::unique_ptr<StateMachine> LStar
    (std::string& alphabet,int maxNumOfEquivClasses,int maxTryCount, IMAT& MAT,
     const std::string& mode);

    ~EquivalenceClassesTable()=default;
};
