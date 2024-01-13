#ifndef TFL_LABS_STRINGGENERATOR_H
#define TFL_LABS_STRINGGENERATOR_H

#include <string>
#include "StateMachine.h"
#include "RegexGenerator.h"

class StringGenerator{
private:
    size_t seed = 0;
    void changeSeed();
public:
    StringGenerator()=default;
    std::string GenerateString(const StateMachine& stM, RegexGenerator* rg= nullptr);
};

#endif //TFL_LABS_STRINGGENERATOR_H
