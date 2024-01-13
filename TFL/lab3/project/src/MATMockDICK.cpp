#include "MATMockDYCK.h"
#include "Constant.h"

MATMockDYCK::MATMockDYCK(std::string alphabet) {
    int counter=0;
    for (int i=0;i< alphabet.length();i++){
        if (i%2==0){
            if (i+1>=alphabet.length()){
                freeChar=alphabet[i];
                break;
            }
            openBrackets.emplace(alphabet[i],counter);
        } else {
            closeBrackets.emplace(alphabet[i],counter);
            counter++;
        }
    }
}

bool  MATMockDYCK::checkDYCKpref(const std::string& word){
    std::vector<int> counter (closeBrackets.size()+1);
    for (char i : word){
        if (i==freeChar){
            continue;
        }
        if (openBrackets.find(i)!=openBrackets.end()){
            counter[openBrackets.find(i)->second]++;
        }
        auto clBr=closeBrackets.find(i);
        if (clBr!=closeBrackets.end()){
            counter[clBr->second]--;
            if (counter[clBr->second]<0){
                return false;
            }
        }
    }
    return true;
}

bool  MATMockDYCK::checkDYCKsuff(const std::string& word){
    std::vector<int> counter (closeBrackets.size()+1);
    for (char i : word){
        if (i==freeChar){
            continue;
        }
        auto oBr=openBrackets.find(i);
        if (oBr!=openBrackets.end()){
            counter[oBr->second]--;
            if (counter[oBr->second]<0){
                return false;
            }
        }
        auto clBr=closeBrackets.find(i);
        if (clBr!=closeBrackets.end()){
            counter[clBr->second]++;
        }
    }
    return true;
}

bool  MATMockDYCK::checkDYCK(const std::string& word){
    std::vector<int> counter (closeBrackets.size()+1);
    for (char i : word){
        if (i==freeChar){
            continue;
        }
        auto oBr=openBrackets.find(i);
        if (oBr!=openBrackets.end()){
            counter[oBr->second]++;
        }
        auto clBr=closeBrackets.find(i);
        if (clBr!=closeBrackets.end()){
            counter[clBr->second]--;
            if (counter[clBr->second]<0){
                return false;
            }
        }
    }
    for (int var: counter){
        if (var!=0){
            return false;
        }
    }
    return true;
}

bool MATMockDYCK::IsMembership(const std::string& word,const std::string& mode){
    if (mode==PREFIX_MODE) {
        return checkDYCKpref(word);
    }
    if (mode == SUFFIX_MODE){
        return checkDYCKsuff(word);
    }
    return checkDYCK(word);
}

std::string arrToStr(std::vector<std::string>& permutation){
    std::string res;
    for (auto str: permutation){
        res+=str;
    }
    return res;
}

void MATMockDYCK::checkPermutationsWithLen(StateMachine& M,
                                                const std::string& alphabet,std::vector<std::string>& permutation,
                                                int len,int curIndex,const std::string& mode, std::string *res){
    if (*res != "equal" || innerMaxTryCount-- <= 0){
        return;
    }
    if(curIndex == len){
        auto perm=arrToStr(permutation);
        if (IsMembership(perm,mode) != M.IsWordBelong(perm)){
            *res= perm;
        }
    }
    else{
        for(int i = 0; i < alphabet.size(); i++){
            if (*res != "equal" || innerMaxTryCount-- <= 0){
                return;
            }
            permutation[curIndex] = std::string(1,alphabet[i]);
            checkPermutationsWithLen(M,alphabet,permutation,
                                     len,curIndex+1,mode,res);
        }
    }
}

std::string MATMockDYCK::IsEqual(StateMachine& M,std::string& alphabet,int maxTryCount,const std::string& mode) {
    innerMaxTryCount = maxTryCount;
    for (int i=1;i<=MAX_GENERATED_WORDS_LEN ;i++){
        std::vector<std::string> permutation(i);
        std::string res = "equal";
        checkPermutationsWithLen(M,alphabet,permutation,i,0,mode,&res);
        if (res!= "equal"){
            return res;
        }
    }
    return "equal";
}

