#include "../include/StringGenerator.h"
#include <ctime>
#include <queue>

void StringGenerator::changeSeed() {
    seed++;
    srand((size_t)time(nullptr) + seed + rand());
}

std::string StringGenerator::GenerateString(const StateMachine& stM, RegexGenerator* rg){
    changeSeed();
    std::string res;
    if (stM.stateCount==0){
        while (true){
            int v=rand()%3;
            if (v==0){
                break;
            }
            if (rg == nullptr){
                res+='a'+v;
            } else {
                res+=rg->randSymb();
            }
        }
    } else {
        std::queue<int> Q;
        Q.push(0);
        while (!Q.empty()){
            int curState=Q.front();
            auto it=stM.finalStates.find(curState);
            int v=rand()%3;
            if (it!=stM.finalStates.end() && curState!=0 && !v){
                return res;
            }
            Q.pop();
            std::vector<int> validTrans;
            for (int i=0;i<stM.transitions[curState].size();i++){
                if (stM.transitions[curState][i]!=' '){
                    validTrans.push_back(i);
                }
            }
            if (validTrans.empty()){
                break;
            }
            v = rand()%validTrans.size();
            Q.push(validTrans[v]);
            auto curStr=std::string(1,stM.transitions[curState][validTrans[v]]);
            if (curStr=="."){
                int v=rand()%26;
                curStr ='a'+v;
            }
            res+=curStr;
            v=rand()%20;
            if (!v || v==3 || v==4){
                res+=curStr;
            } else if (v==1) {
                res+=res;
            } else if (v==2){
                res.pop_back();
            }
        }
    }
    return res;
}
