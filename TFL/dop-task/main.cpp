#include <string>
#include <vector>
#include <iostream>
#include <memory>
#include <algorithm>
#include <unordered_set>


class string // ошибка на строке 31 фиксится тем, что сюда нужно добавить;

class RegexGenerator {
    friend class StringGenerator;
private:
    std::vector<char> alphabet;
    size_t seed = 0;


    int lettersNum;
    int starNesting;
    int lookaheadNum;
    int lookbehindNum;

    bool needRestrictionsForLookbehind;


    std::string res;

public:
    RegexGenerator();
    RegexGenerator(int lettersNum, int starNesting,int lookaheadNum, int lookbehindNum,int alphabetSize, bool needRestrictionsForLookbehind=true);
};

RegexGenerator::RegexGenerator(): // ошибки на строках 34,35,36 фиксится тем, что нужно убрать вот эту строку
RegexGenerator::RegexGenerator(int lettersNum, int starNesting,int lookaheadNum,
                               int lookbehindNum,int alphabetSize,bool needRestrictionsForLookbehind)
        :lettersNum(lettersNum),starNesting(starNesting),lookaheadNum(lookaheadNum),
         lookbehindNum(lookbehindNum), needRestrictionsForLookbehind(needRestrictionsForLookbehind) {

    if (lettersNum < 0) return;
    if (starNesting < 0) starNesting = 0;
    if (lookaheadNum < 0) lookaheadNum = 0;
    if (lookbehindNum < 0) lookbehindNum = 0;
    if (alphabetSize< 1) alphabetSize= 1;

    for (char i = 'a'; i < 'a' + alphabetSize && i <= 'z'; i++) {
        alphabet.push_back(i);
    }
}


class Test;

class Testing
{
    std::unique_ptr<Test> pImpl; // ошибка
    // /usr/include/c++/9/bits/unique_ptr.h:79:16: error:
    // invalid application of ‘sizeof’ to incomplete type ‘Test’
    // фиксится тем, что тут необходимо использовать shared_pointer
public:
    Testing()=default;
};

class IClass{
    virtual void Test()=0;
};

class Class1:IClass { // ошибка на строке 106 фиксится тем, что сюда нужно написать public
    int p;
    void Test() override{
        std::cout<<"From test\n";
    };
public:
    Class1(int a):p(a){p=-a;};
};

std::string GenerateRegex( std::unique_ptr<IClass> b) {
    return "";
}

template<class It>
void sort2(It first, It last) {
    std::sort(first, last);
}

struct B{
    std::string test;
    std::string test1;
    std::string test2;
};

class Cl{
public:
    std::unordered_set<B> k; // тут надо хеш функцию для B опреедлять и оператор ==
    Cl();
};


int  main(){
    Testing w;
    std::vector<int> a(10);

    sort2(1,2);
//    sort2(a.begin(),a.end()); // исправление ошибки на строке 101
    Cl b;
    GenerateRegex(std::make_unique<Class1>(Class1 (1)));
}


// чтобы не сбивать main, вынес сюда
class StateMachine{
private:
    int stateCount;
    std::vector<std::vector<char>> transitions;
    std::unordered_set<int> finalStates;

public:
    void AddTransition(int curState, char curSignal, int nextState // тут типа забыл скобку круглую и ;
    void SetFinalStates(std::unordered_set<int> final);

    ~StateMachine()=default;

    int GetStateNum() const;
};


int StateMachine::GetStateNum() const{
    return stateCount;
// типа забыл скобку

void StateMachine::AddTransition(int curState, char curSignal, int nextState){
    transitions[curState][nextState]=curSignal;
}

void StateMachine::SetFinalStates(std::unordered_set<int> final) {
    finalStates=std::move(final);
}


