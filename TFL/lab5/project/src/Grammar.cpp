#include "Grammar.h"
#include <iostream>
#include <fstream>
#include <sstream>

Grammar::Grammar(const std::string& filename) {
    std::string line;
    std::ifstream in(filename);

    if (in.is_open()) {
        while (std::getline(in, line)) {
//            std::cout <<"DEBUG: "<< line << std::endl;

            if (!line.empty() && line[line.size() - 1] == '\r')
                line.erase(line.size() - 1);

            rules.push_back(line);
            addNonTerm(line);
        }
    }
    in.close();

    addTerms();
}

void Grammar::addNonTerm(const std::string& nonTerm) {
    std::stringstream ss(nonTerm);
    std::string token;

    ss>>token;
    nonTerms.emplace(token);

    if (startToken.empty()){
        startToken=token;
    }
}

void Grammar::addTerms() {
    for (std::string str : rules){
        std::stringstream ss(str);
        std::string token;

        while (ss>>token){
            if (token!="->" && token!="|" && nonTerms.find(token)==nonTerms.end()){
                terms.emplace(token);
            }
        }
    }
}

std::vector<std::string> Grammar::Rules() {
    return rules;
}
std::unordered_set<std::string> Grammar::NonTerms() {
    return nonTerms;
}
std::unordered_set<std::string> Grammar::Terms() {
    return terms;
}
std::string Grammar::StartToken() {
    return startToken;
}
