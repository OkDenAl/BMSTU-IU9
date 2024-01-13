#include "MainAlgorithm.h"

#include <algorithm>
#include <fstream>
#include <functional>
#include <unordered_set>

#include "Constant.h"

MainAlgorithm::MainAlgorithm(std::string& alphabet, int admissionToRegularity,
                             int maxTryCount,
                             int maxNumOfItersForSymmetricPump)
    : alphabet(alphabet),
      admissionToRegularity(admissionToRegularity),
      maxTryCount(maxTryCount),
      maxNumOfItersForSymmetricPump(maxNumOfItersForSymmetricPump) {}

void MainAlgorithm::fillLanguage(IMAT& MAT, const std::string& mode) {
    int N = alphabet.size();
    std::vector<int> maxStateCount(alphabet.size());
    std::unordered_set<std::string> badWords;
    maxStateCount[0] = 0;
    for (int i = 1; i <= alphabet.size(); i++) {
        int K = i;
        std::string bitmask(K, 1);
        bitmask.resize(N, 0);
        do {
            std::string curStr;
            std::unordered_set<char> curLetter;
            for (int j = 0; j < N; ++j) {
                if (bitmask[j]) {
                    curStr += alphabet[j];
                    curLetter.insert(alphabet[j]);
                }
            }
            bool isBad = false;
            for (const auto& badPref : badWords) {
                bool needToSkip = false;
                for (auto letter : badPref) {
                    if (curLetter.find(letter) == curLetter.end()) {
                        needToSkip = true;
                        break;
                    }
                }
                if (!needToSkip) {
                    isBad = true;
                    break;
                }
            }
            if (isBad) {
                continue;
            }
            std::unique_ptr<StateMachine> automata =
                EquivalenceClassesTable::LStar(
                    curStr, maxStateCount[i - 1] + admissionToRegularity,
                    maxTryCount, MAT, mode);
            if (automata != nullptr) {
                automata->FixStates();
                if (automata->IsAnyCycle()) {
                    if (automata->GetStateNum() > maxStateCount[i]) {
                        maxStateCount[i] = automata->GetStateNum();
                    }
                    std::ofstream out(mode + "out_" + curStr);
                    StateMachine::To_Graph(*automata, out);
                    out.close();
                    if (mode == PREFIX_MODE) {
                        prefixLanguage.emplace_back(std::move(automata));
                        prefixLanguageStr.emplace_back(curStr);
                    } else {
                        suffixLanguage.emplace_back(std::move(automata));
                        suffixLanguageStr.emplace_back(curStr);
                    }
                }
            } else {
                badWords.insert(curStr);
            }
        } while (std::prev_permutation(bitmask.begin(), bitmask.end()));
    }
    return;
}

struct Pump {
    std::string w0;
    std::string u1;
    std::string w2;
};

struct PumpHash {
    std::hash<std::string> hash_;
    size_t operator()(const Pump& pump) const {
        return hash_(pump.w0 + pump.u1 + pump.w2);
    }
};

bool operator==(const Pump& a, const Pump& b) {
    return (a.u1 == b.u1) && (a.w0 == b.w0) && (a.w2 == b.w2);
}

using path = std::string;
using path_word = std::string;
using state_cycles = std::unordered_set<path_word>;
using automata_states_cycles = std::vector<state_cycles>;
using pump_set = std::unordered_set<Pump, PumpHash>;

std::string multiply(const std::string& s, int times) {
    std::string res = "";
    for (int i = 0; i < times; ++i) {
        res += s;
    }
    return res;
}

bool checkPump(const Pump& pref_pump, const Pump& suff_pump, IMAT& MAT, int addmission) {
    std::unordered_set<int> checked;
    for (int k1 = 0; k1 < addmission; ++k1) {
        bool is_good = false;
        std::string pref = pref_pump.w0 + multiply(pref_pump.u1, k1) + pref_pump.w2 + suff_pump.w0;
        for (int k2 = 0; k2 < addmission; ++k2) {
            std::string word = pref + multiply(suff_pump.u1, k2) + suff_pump.w2;
            if (MAT.IsMembership(word, BASE_MODE)) {
                checked.insert(k2);
                is_good = true;
                break;
            }
        }
        if (!is_good) {
            return false;
        }
    }
    for (int k2 = 0; k2 < addmission; ++k2) {
        if (checked.find(k2) != checked.end()) {
            continue;
        }
        bool is_good = false;
        std::string suff = pref_pump.w2 + suff_pump.w0 + multiply(suff_pump.u1, k2) + suff_pump.w2;;
        for (int k1 = 0; k1 < addmission; ++k1) {
            std::string word = pref_pump.w0 + multiply(pref_pump.u1, k1) + suff;
            if (MAT.IsMembership(word, BASE_MODE)) {
                is_good = true;
                break;
            }
        }
        if (!is_good) {
            return false;
        }
    }
    return true;
}

void get_fragments(const StateMachine& M, pump_set& dest) {
    int _size = M.GetStateNum() + 1;
    const std::unordered_set<int>& final = M.GetFinalStates();
    for (int i = 0; i < _size; ++i) {
        std::unordered_set<path_word> paths_on_cycle = M.FindCycles(i);
        if (paths_on_cycle.size() != 0) {
            std::unordered_set<path_word> paths_to_cycle =
                M.FindPaths(0, {i});
            std::unordered_set<path_word> paths_from_cycle =
                M.FindPaths(i, final);
            for (const path_word& to : paths_to_cycle) {
                for (const path_word& on : paths_on_cycle) {
                    for (const path_word& from : paths_from_cycle) {
                        dest.insert({to, on, from});
                    }
                }
            }
        }
    }
}

void MainAlgorithm::Run(IMAT& MAT) {
    fillLanguage(MAT, PREFIX_MODE);
    fillLanguage(MAT, SUFFIX_MODE);
    if (prefixLanguage.empty() || suffixLanguage.empty()) {
        std::cout << "unreal" << std::endl;
    }

    pump_set prefixes_pumps;
    pump_set suffixes_pumps;

    for (auto pref : prefixLanguage) {
        get_fragments(*pref, prefixes_pumps);
    }
    for (auto suf : suffixLanguage) {
        get_fragments(*suf, suffixes_pumps);
    }

    for (auto pref_pump : prefixes_pumps) {
        for (auto suf_pump : suffixes_pumps) {
            if (checkPump(pref_pump, suf_pump, MAT, maxNumOfItersForSymmetricPump)) {
                
                std::cout << pref_pump.w0 << "(" << pref_pump.u1 << ")*" << pref_pump.w2;  
                std::cout << suf_pump.w0 << "(" << suf_pump.u1 << ")*" << suf_pump.w2 << std::endl;  
            }
        }
    }



}
