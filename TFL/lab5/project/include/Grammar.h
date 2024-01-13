#pragma once
#include "vector"
#include "string"
#include "unordered_set"

class Grammar{
private:
    std::vector<std::string> rules;
    std::unordered_set<std::string> nonTerms;
    std::unordered_set<std::string> terms;
    std::string startToken;

    void addNonTerm(const std::string& nonTerm);
    void addTerms();
public:
    explicit Grammar(const std::string& filename);

    std::vector<std::string> Rules();
    std::unordered_set<std::string> NonTerms();
    std::unordered_set<std::string> Terms();
    std::string StartToken();

};
