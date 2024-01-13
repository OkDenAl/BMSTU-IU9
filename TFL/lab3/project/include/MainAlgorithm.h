#pragma once
#include "EquivalenceClassesTable.h"

class MainAlgorithm{
private:
    std::vector<std::shared_ptr<StateMachine>> prefixLanguage;
    std::vector<std::string> prefixLanguageStr;
    std::vector<std::shared_ptr<StateMachine>> suffixLanguage;
    std::vector<std::string> suffixLanguageStr;
    std::string alphabet;
    int admissionToRegularity;
    int maxTryCount;
    int maxNumOfItersForSymmetricPump;

    void fillLanguage(IMAT& MAT, const std::string& mode);
public:
    MainAlgorithm(std::string& alphabet,int admissionToRegularity,
                  int maxTryCount,int maxNumOfItersForSymmetricPump);
    void Run(IMAT& MAT);
};
