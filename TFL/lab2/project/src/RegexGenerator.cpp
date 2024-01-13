#include "../include/RegexGenerator.h"
#include <ctime>
#include <iostream>

RegexGenerator::RegexGenerator()
        : RegexGenerator::RegexGenerator(6, 2, 2, 2, 2) {}

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

void RegexGenerator::changeSeed() {
    seed++;
    srand((size_t)time(nullptr) + seed + rand());
}

char RegexGenerator::randSymb() {
    return alphabet[rand()%alphabet.size()];
}

std::string RegexGenerator::GenerateRegex() {
    changeSeed();
    curLettersNum=0;
    curNesting=0;
    curLookaheadNum=0;
    curLookbehindNum=0;
    fromLookahead= false;
    fromLookbehind = false;
    wasLookaheadInBrackets= false;
    wasLookbehindInBrackets=false;
    wasUnionInBrackets = false;
    needToReturn = false;
    curOpenBracketsNum=0;

    res="^(";
    while(curLettersNum!=lettersNum && !needToReturn) generateRegex();
    res+=")$";
    return res;
}


//<regex> ::= <conc-regex> <alt> <regex> | <conc-regex>
void RegexGenerator::generateRegex(){
    int state;
    if (lettersNum==curLettersNum) return;
    if (lettersNum-curLettersNum<2 || (fromLookbehind && needRestrictionsForLookbehind)) state=1;
    else state=rand()%2;
    switch (state) {
        case 0:
            generateConcRegex();
            if (lettersNum==curLettersNum || needToReturn ||
                (wasLookbehindInBrackets && needRestrictionsForLookbehind)) {
                return;
            }
            if (curOpenBracketsNum!=0){
                wasUnionInBrackets=true;
            }
            res+="|";
            generateRegex();
            break;
        case 1:
            generateConcRegex();
            break;
    }
}

// <conc-regex> ::= <simple-regex> | <simple-regex><conc-regex>
void RegexGenerator::generateConcRegex() {
    int state = rand()%2;
    switch (state) {
        case 0:
            generateSimpleRegex();
            break;
        case 1:
            generateSimpleRegex();
            if (lettersNum==curLettersNum || needToReturn) return;
            generateConcRegex();
            break;
    }
}

// <simple-regex> ::= <lbr><regex><rbr><unary>? | буква <unary>? | (?=(<regex>)$?) | (?<=^?(<regex>))
void RegexGenerator::generateSimpleRegex() {
    int state = rand()%3;
    if (curLettersNum==0){
        state = rand() % 7;
        if (state != 0 && state!=2) state = 1;
    }
    if ((lookaheadNum==curLookaheadNum && lookbehindNum == curLookbehindNum) || (lookaheadNum==curLookaheadNum && wasUnionInBrackets)
        || fromLookahead || fromLookbehind) {
        if (fromLookahead || fromLookbehind) {
            state = rand() % 4;
            if (state != 0) state = 1;
        } else {
            state=rand()%2;
        }
    }
    int v;
    bool needStar= false;
    switch (state) {
        case 0:
            curOpenBracketsNum++;
            res+="(";
            if (curNesting!=starNesting){
                v=rand()%2;
                if (!v) {
                    needStar=true;
                    curNesting++;
                }
            }
            curNestingOnThisLevel=curNesting;
            generateRegex();
            res+=")";
            if (!wasLookaheadInBrackets && needStar && !wasLookbehindInBrackets &&
                (!fromLookbehind || !needRestrictionsForLookbehind)){
                res+="*";
                curNesting--;
            } else if (needStar) {
                curNesting-=2;
            }
            curOpenBracketsNum--;
            if (wasLookaheadInBrackets && wasUnionInBrackets){
                needToReturn = true;
                return ;
            }
            if (curOpenBracketsNum==0){
                wasLookaheadInBrackets= false;
                wasLookbehindInBrackets=false;
                wasUnionInBrackets=false;
                curNesting=0;
            }
            break;
        case 1:
            v=rand()%10;
            if (v==0) {
                res+=".";
            }else {
                res+=randSymb();
            }
            curLettersNum++;
            if (curNesting!=starNesting){
                v=rand()%2;
                if (!v && (!fromLookbehind || !needRestrictionsForLookbehind)) {
                    res+="*";
                }
            }
            break;
        case 2:
            bool isLookbehind=false;
            if (lookaheadNum==curLookaheadNum || lookbehindNum == curLookbehindNum) {
                if (lookaheadNum==curLookaheadNum && !wasUnionInBrackets){
                    curLookbehindNum++;
                    isLookbehind=true;
                    res+="(?<=";
                } else {
                    curLookaheadNum++;
                    res+="(?=(";
                }
            } else {
                v=rand()%2;
                if (!v && !wasUnionInBrackets){
                    curLookbehindNum++;
                    res+="(?<=";
                    isLookbehind=true;
                } else {
                    curLookaheadNum++;
                    res+="(?=(";
                }
            }
            if (isLookbehind){
                v=rand()%7;
                if (!v){
                    res+="(";
                } else {
                    res+="^(";
                }
                fromLookbehind=true;
            } else {
                fromLookahead= true;
            }
            generateRegex();
            if (curOpenBracketsNum!=0){
                if (isLookbehind) wasLookbehindInBrackets=true;
                else wasLookaheadInBrackets= true;
            }
            if (isLookbehind){
                res+="))";
                fromLookbehind=false;
            } else {
                v=rand()%7;
                if (!v){
                    res+="))";
                } else {
                    res+=")$)";
                }
                fromLookahead= false;
            }
    }
}

/*
GRAMMAR:
<regex> ::= <conc-regex> <alt> <regex> | <conc-regex>
<conc-regex> ::= <simple-regex> | <simple-regex><conc-regex>
<simple-regex> ::= <lbr><regex><rbr><unary>? | буква <unary>? | (?=(<regex>)$?) | (?<=^?(<regex>))
<alt> ::= '|'
<lbr> ::= '('
<rbr> ::= ')'
<unary> ::= '*'
*/

// при этом дополнительно не допускаются lookahead вложенные и под звездой, а также не может после альтернативы
// с lookahead идти конкатенация

// в lookbehind действуют те же, ограничения, что и на lookahead кроме последнего
// (вместо него перед альтернативой с lookbehind не может идти конкатенация)
