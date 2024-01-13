#pragma once

#include <string>
#include "StateMachine.h"

class IMAT {
public:
    virtual bool IsMembership(const std::string& word,const std::string& mode)=0;
    virtual std::string IsEqual(StateMachine& M,std::string& alphabet,int maxTryCount,const std::string& mode) = 0;
    virtual ~IMAT() = default;
};
