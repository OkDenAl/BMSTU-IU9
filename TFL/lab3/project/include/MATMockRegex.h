#pragma once

#include <string>
#include "IMAT.h"

class MATMockRegex : public IMAT {
private:
    StateMachine lang;
    StateMachine pref;
    StateMachine suff;
    int innerMaxTryCount=0;
    void checkPermutationsWithLen(StateMachine& M,
                                           const std::string& alphabet,std::vector<std::string>& permutation,
                                           int len,int curIndex,const std::string& mode, std::string *res);
public:
    MATMockRegex(StateMachine& lang, StateMachine& pref,StateMachine& suff);
    bool IsMembership(const std::string& word,const std::string& mode) override;
    std::string IsEqual(StateMachine& M,std::string& alphabet,int maxTryCount,const std::string& mode) override;
};
