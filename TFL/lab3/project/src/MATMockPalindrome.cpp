#include "MATMockPalindrome.h"
#include "Constant.h"


bool MATMockPalindrome::IsMembership(const std::string& word,const std::string& mode){
    if (mode==PREFIX_MODE || mode == SUFFIX_MODE){
        return true;
    }
    return equal(word.begin(), word.begin() + word.size()/2, word.rbegin());
}

std::string arrToStr(std::vector<std::string>& permutation){
    std::string res;
    for (auto str: permutation){
        res+=str;
    }
    return res;
}

void MATMockPalindrome::checkPermutationsWithLen(StateMachine& M,
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

std::string MATMockPalindrome::IsEqual(StateMachine& M,std::string& alphabet,int maxTryCount,const std::string& mode) {
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
