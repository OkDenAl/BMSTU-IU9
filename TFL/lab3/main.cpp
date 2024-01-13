#include <iostream>
#include <fstream>
#include "MainAlgorithm.h"
#include "parser/parser2.hpp"
#include "MATMockPalindrome.h"
#include "MATMockDYCK.h"
#include "MATMockRegex.h"

struct InputData{
    std::string alhabet;
    int admissionToRegularity;
    int maxTryCount;
    int maxNumOfItersForPump;
};

static void showUsage(const std::string& name) {
    std::cout << "\nUsage: " << name << " <option(s)>\n"
              << "You must spec options in any order:\n"
              << "\t-a,  --alphabet\t\tSpecify the alphabet (string like \"abc\", without repeating letters)\n"
              << "\t-ra, --regadm\t\tSpecify admission to regularity\n"
              << "\t-mtc, --maxtrycount\t\tSpecify max try count for equality query to MAT\n"
              << "\t-pi, --pumpit\t\tSpecify max num of iters for symmetric pump \n"
              << std::endl;
}

int parseFlags(int argc, char *argv[], InputData& data) {
    for (int i = 1; i < argc; i++) {
        std::string arg = argv[i];
        if (arg=="--alphabet" || arg=="-a"){
            i++;
            if (i>=argc) return -1;
            data.alhabet=argv[i];
        } else if (arg=="--regadm" || arg=="-ra") {
            i++;
            if (i>=argc) return -1;
            data.admissionToRegularity=std::stoi(argv[i]);
        } else if (arg=="--maxtrycount" || arg=="-mtc") {
            i++;
            if (i>=argc) return -1;
            data.maxTryCount=std::stoi(argv[i]);
        } else if (arg=="--pumpit" || arg=="-pi") {
            i++;
            if (i>=argc) return -1;
            data.maxNumOfItersForPump=std::stoi(argv[i]);
        } else {
            return -1;
        }
    }
    return 0;
}

class parse_error: public std::exception {};

StateMachine convertToStateMachine(const std::string& inputRegex) {
    Parser r(inputRegex.data(), inputRegex.length());
    node_ptr R = r.Parse();
    if (!R) {
        throw parse_error();
    }
    StateMachine M = R->to_machine_dfs();
    M.FixStates();
    return M;
}

StateMachine makePrefixLang(StateMachine& m){
    std::unordered_set<int> finalStatesPrefixes;
    for (int i=0;i<=m.GetStateNum();i++){
        finalStatesPrefixes.insert(i);
    }
    auto trans = m.GetTransitions();
    return {trans,finalStatesPrefixes,m.GetStateNum()};
}

int main(int argc, char *argv[]){
     if (argc !=9) {
         showUsage(argv[0]);
         return 1;
     }
     InputData data;
     parseFlags(argc,argv,data);

    // Regular language
    // std::string langRegex = "^(aa)*a(bb)*(c|a)$";
    // std::string reverseRegex  = "^(c|a)(bb)*a(aa)*$";

    // StateMachine lang;
    // StateMachine reverse;
    // StateMachine automataPrefixes;
    // StateMachine automataSuffixes;
    // try {
    //     lang = convertToStateMachine(langRegex);
    //     automataPrefixes = makePrefixLang(lang);
    //     reverse = convertToStateMachine(reverseRegex);
    //     automataSuffixes = makePrefixLang(reverse);
    // } catch (const parse_error& e) {
    //     std::cout << "parse error" << std::endl;
    //     return 0;
    // }    
    // MATMockRegex m(lang, automataPrefixes, automataSuffixes);

    // Palindrome language
    // MATMockPalindrome m;

    // DYCK language
    MATMockDYCK m(data.alhabet);

    MainAlgorithm main(data.alhabet,data.admissionToRegularity,
                       data.maxTryCount,data.maxNumOfItersForPump);
    main.Run(m);
    return 0;
}
