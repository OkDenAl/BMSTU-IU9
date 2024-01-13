#include <iostream>
#include <utility>
#include <vector>
#include <fstream>
#include <unordered_map>
#include <sstream>

class Letter {
    friend void createSmt2File(std::unordered_map<char,Letter>& letters,std::unordered_map<std::string,
            Letter>& compositions,std::vector<std::string>& order, std::ifstream& smt2Template);
    friend void compareRules(Letter& LHS, Letter& RHS, std::ofstream& out);
private:
    std::string name;
    std::vector<std::string> xCoef;
    std::vector<std::string> freeMemberCoef;
public:
    Letter()=default;
    explicit Letter(std::string  letter):name(std::move(letter)){
        xCoef.push_back(name+"x0");
        xCoef.push_back(name+"x1");
        freeMemberCoef.push_back(name+"fm0");
        freeMemberCoef.push_back(name+"fm1");
    };
    Letter(const std::string& letter,const std::vector<std::string>& xCoef,
           const std::vector<std::string>& freeMemberCoef){
        name=letter;
        this->xCoef=xCoef;
        this->freeMemberCoef=freeMemberCoef;
    }
    ~Letter()=default;
    std::string& GetName(){
        return name;
    }
    std::vector<std::string>& GetXCoef(){
        return xCoef;
    }
    std::vector<std::string>& GetFreeMemberCoef(){
        return freeMemberCoef;
    }
    static Letter CountComposition(Letter& l1, Letter& l2) {
        std::vector<std::string> xCoefTemp;
        std::vector<std::string> freeMemberCoefTemp;

        auto l1XCoef=l1.GetXCoef();
        auto  l1FreeMemberCoef = l1.GetFreeMemberCoef();
        auto l2XCoef=l2.GetXCoef();
        auto  l2FreeMemberCoef = l2.GetFreeMemberCoef();

        for (size_t i =0;i<l1XCoef.size()-1;i++){
            xCoefTemp.push_back(l1XCoef[i]);
        }
        xCoefTemp.push_back("(* "+l1XCoef[l1XCoef.size()-1]+" "+l2XCoef[0]+")");
        xCoefTemp.push_back(l2XCoef[1]);

        for (size_t i =0;i<l1FreeMemberCoef.size()-1;i++){
            freeMemberCoefTemp.push_back(l1FreeMemberCoef[i]);
        }
        freeMemberCoefTemp.push_back("(+ (* "+l1XCoef[l1XCoef.size()-1]+" "+l2FreeMemberCoef[0]+") "+
        l1FreeMemberCoef[l1FreeMemberCoef.size()-1]+")");

        freeMemberCoefTemp.push_back(l2FreeMemberCoef[1]);

        Letter l(l1.GetName()+l2.GetName(),xCoefTemp,freeMemberCoefTemp);
        return l;
    }
};

void compareGreater(std::vector<std::string>& LHSCoef,std::vector<std::string>& RHSCoef,std::ofstream& out,
                    int startLHSLen){
    for (int i=startLHSLen-1;i>=0;i--){
        bool needToCloseBracket = false;
        for (size_t j=LHSCoef.size()-1;j>i;j--) {
            if (j==LHSCoef.size()-1){
                out<<" (and ";
                needToCloseBracket= true;
            }
            out<<"(= "+LHSCoef[j]+" "+RHSCoef[j]+") ";
        }
        out<<"(> "+LHSCoef[i]+" "+RHSCoef[i]+")";
        if (needToCloseBracket){
            out<<")";
        }
    }
}

void compareGreaterOrEqual(std::vector<std::string>& LHSCoef,std::vector<std::string>& RHSCoef,std::ofstream& out,
                           int startLHSLen){
    for (int i=startLHSLen-1;i>=0;i--){
        bool needToCloseBracket = false;
        for (size_t j=LHSCoef.size()-1;j>i;j--) {
            if (j==LHSCoef.size()-1){
                out<<" (and ";
                needToCloseBracket= true;
            }
            out<<"(= "+LHSCoef[j]+" "+RHSCoef[j]+") ";
        }
        if (i==0){
            out<<"(>= "+LHSCoef[i]+" "+RHSCoef[i]+")";
        } else {
            out<<"(> "+LHSCoef[i]+" "+RHSCoef[i]+")";
        }
        if (needToCloseBracket){
            out<<")";
        }
    }
}

void compareEqual(std::vector<std::string>& LHSCoef,std::vector<std::string>& RHSCoef,std::ofstream& out) {
    for (int i=LHSCoef.size()-1;i>0;i--){
        out<<"(= "+LHSCoef[i]+" "+RHSCoef[i]+") ";
    }
    out<<"(= "+LHSCoef[0]+" "+RHSCoef[0]+")";
}

void compareRules(Letter& LHS, Letter& RHS, std::ofstream& out) {
    out<<"(assert ";
    out<< "(or ";
    int startLenLHSCoef = LHS.xCoef.size();
    while (LHS.xCoef.size()<RHS.xCoef.size()){
        LHS.xCoef.emplace_back("0");
        LHS.freeMemberCoef.emplace_back("0");
    }
    while (LHS.xCoef.size()>RHS.xCoef.size()){
        RHS.xCoef.emplace_back("0");
        RHS.freeMemberCoef.emplace_back("0");
    }

    out<<"(and (or ";
    compareGreater(LHS.xCoef,RHS.xCoef,out,startLenLHSCoef);
    out<<") ";

    out<<"(or ";
    compareGreaterOrEqual(LHS.freeMemberCoef,RHS.freeMemberCoef,out,startLenLHSCoef);
    out<<")) ";

    out<<"(and (and ";
    compareEqual(LHS.xCoef,RHS.xCoef,out);
    out<<") ";

    out<<"(or ";
    compareGreater(LHS.freeMemberCoef,RHS.freeMemberCoef,out,startLenLHSCoef);
    out<<"))))\n";
}

void createSmt2File(std::unordered_map<char,Letter>& letters,std::unordered_map<std::string,
                    Letter>& compositions,std::vector<std::string>& order, std::ifstream& smt2Template) {
    std::ofstream out ("solver.smt2");
    if (out.is_open())
    {

        std::string lineFromTemlate;
        std::getline(smt2Template, lineFromTemlate);
        out << lineFromTemlate;
        std::getline(smt2Template, lineFromTemlate);

        for (auto & letter : letters){
            for (const auto & coef : letter.second.xCoef){
                out << "(declare-fun " + coef + " () Int)\n";
                out << "(assert (>= " + coef + " 0))\n";
            }
            for (const auto & coef : letter.second.freeMemberCoef){
                out << "(declare-fun " + coef + " () Int)\n";
                out << "(assert (>= " + coef + " 0))\n";
            }
            out << std::endl;
        }

        for (size_t i=0;i<order.size();i+=2){
            compareRules(compositions[order[i]],compositions[order[i+1]],out);
        }

        std::getline(smt2Template, lineFromTemlate);
        out << lineFromTemlate;
        std::getline(smt2Template, lineFromTemlate);
        out << lineFromTemlate;
    } else {
        std::cout<<"Cant open output file\n";
        return;
    }
    out.close();
}

void parseStrings(std::unordered_map<char,Letter>& letters,std::unordered_map<std::string,Letter>& compositions,
                  std::vector<std::string>& order,const std::string& filename){
    std::ifstream infile(filename);
    letters['f']=Letter("f");
    if (infile.is_open()){
        std::string line;
        while (std::getline(infile, line))
        {
            std::istringstream iss(line);
            std::string rule;
            while (iss >> rule) {
                if (rule=="->"){
                    continue;
                }
                order.push_back(rule);
                for (auto letter : rule) {
                    if (letters.find(letter)==letters.end()){
                        letters[letter]=Letter(std::string(1,letter));
                    }
                }
                if (compositions.find(rule)==compositions.end()){
                    if (rule.length()==1){
                        compositions[rule]=letters[rule[0]];
                    } else {
                        compositions[rule]=Letter::CountComposition(letters[rule[0]],letters[rule[1]]);
                        for (int i=2;i<rule.size();i++){
                            compositions[rule]=Letter::CountComposition(compositions[rule],letters[rule[i]]);
                        }
                    }
                }
            }
        }
    } else {
        std::cout<<"Cant open input file\n";
        return;
    }
    infile.close();
}


int main(int argc, char* argv[]) {
    std::string filename;
    if (argc == 2) {
        filename = static_cast<std::string>(argv[argc - 1]);
    } else {
        filename = "rules.txt";
    }

    std::unordered_map<char,Letter> letters;
    std::unordered_map<std::string,Letter> compositions;
    std::vector<std::string> order;

    parseStrings(letters,compositions, order, filename);

    std::ifstream smt2Template("template.smt2.example");
    createSmt2File(letters, compositions,order,smt2Template);
    smt2Template.close();

    return 0;
}
