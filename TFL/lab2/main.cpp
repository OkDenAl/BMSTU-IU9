#include <iostream>
#include <fstream>
#include <sstream>

#include "project/include/parser/parser2.hpp"
#include "project/include/StateMachine.h"
#include "project/include/RegexGenerator.h"
#include "project/include/StringGenerator.h"

struct InputData{
    std::string filePath;
    std::string regex;
};

static void showUsage(const std::string& name) {
    std::cout << "\nUsage: " << name << " <option(s)>\n"
              << "Options:\n"
              << "\t-h, --help\t\tShow this help message\n"
              << "\t-f, --file\t\tSpecify the input test file path with regex string\n"
              << "\t-r, --regex\t\tSpecify the test regex string\n"
              << std::endl;
}

int parseFlags(int argc, char *argv[], InputData& data) {
  for (int i = 1; i < argc; i++) {
        std::string arg = argv[i];
        if (arg=="--help" || arg=="-h") {
            showUsage(argv[0]);
        } else if (arg=="--file" || arg=="-f"){
            i++;
            if (i>=argc) return -1;
            data.filePath=argv[i];
        } else if (arg=="--regex" || arg=="-r") {
            i++;
            if (i>=argc) return -1;
            data.regex=argv[i];            
        } else {
            return -1;
        }
  }
  return 0;
}

std::string convertToAcademicRE(const std::string& inputRegex) {
    Parser r(inputRegex.data(), inputRegex.length());
    node_ptr R = r.Parse();
    if (!R) {
        throw std::string{"Parse error"};
    }
    StateMachine M = R->to_machine_dfs();
    fixStates(M);
    return M.ConvertToRegularExpr();
}

int main(int argc, char *argv[]){
    if (argc < 3) {
        showUsage(argv[0]);
        return 1;
    }
    InputData data;
    if (parseFlags(argc,argv,data)==-1){
        showUsage(argv[0]);
        return 1;
    }
    if (!data.filePath.empty()){
        std::ifstream testFile(data.filePath);
        if (testFile.is_open()){
            std::string line;
            while (std::getline(testFile, line)){
                std::istringstream iss(line);
                std::string regex;
                while (iss >> regex) {
                    std::string convertedRegex;
                    try {
                        convertedRegex = convertToAcademicRE(regex);
                    } catch (std::string error_message){
                        std::cerr << error_message << std::endl;
                        continue;
                    }
//                    std::cout<<"Original regex: "<<regex<<std::endl;
                    std::cout<<convertedRegex<<std::endl;
                }
            }
        }else {
            std::cerr<<"Cant open input file\n";
            return 1;
        }
    }
    if (!data.regex.empty()){
        std::string convertedRegex;
        try {
            convertedRegex = convertToAcademicRE(data.regex);
        } catch (std::string error_message){
            std::cerr << error_message << std::endl;
        }
//        std::cout<<"Original regex: "<<data.regex<<std::endl;
        std::cout<<convertedRegex<<std::endl;
    }
}
