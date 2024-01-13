#include "MATMockRegex.h"
#include <sstream>
#include "Constant.h"

MATMockRegex::MATMockRegex(StateMachine& lang,StateMachine& pref,StateMachine& suff): lang(lang),pref(pref),suff(suff){}

bool MATMockRegex::IsMembership(const std::string& word,const std::string& mode){
    if (mode==PREFIX_MODE){
        return pref.IsWordBelong(word);
    }
    if (mode==SUFFIX_MODE){
        return suff.IsWordBelong(word);
    }
    return lang.IsWordBelong(word);
}

std::string arrToStr(std::vector<std::string>& permutation){
    std::string res;
    for (auto str: permutation){
        res+=str;
    }
    return res;
}

void MATMockRegex::checkPermutationsWithLen(StateMachine& M,
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

std::string MATMockRegex::IsEqual(StateMachine& M,std::string& alphabet,int maxTryCount,const std::string& mode) {
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
