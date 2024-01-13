#pragma once

#include <string>
#include <unordered_map>
#include "IMAT.h"

class MATMockDYCK : public IMAT {
private:
    std::unordered_map<char,int> openBrackets;
    std::unordered_map<char,int> closeBrackets;
    char freeChar;
    int innerMaxTryCount;
    void checkPermutationsWithLen(StateMachine& M,
                                  const std::string& alphabet,std::vector<std::string>& permutation,
                                  int len,int curIndex,const std::string& mode, std::string *res);
    bool checkDYCKpref(const std::string& word);
    bool checkDYCKsuff(const std::string& word);
    bool checkDYCK(const std::string& word);
public:
    MATMockDYCK(std::string alphabet);
    bool IsMembership(const std::string& word,const std::string& mode) override;
    std::string IsEqual(StateMachine& M,std::string& alphabet,int maxTryCount,const std::string& mode) override;
};