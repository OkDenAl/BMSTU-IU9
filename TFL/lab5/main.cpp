#include "LRParser.h"
#include "SLRTable.h"
#include "argparse.hpp"
#include "iostream"
#include "sstream"
#include "fstream"

int main(int argc, char* argv[]) {
    argparse::ArgumentParser program("lab5");
    program.add_argument("-g", "--grammar")
        .required()
        .help("specify the grammar input file.");
    auto& sentence = program.add_mutually_exclusive_group(true);
    sentence.add_argument("-i", "--input").help("specify the sentence directly.");
    sentence.add_argument("-f", "--file").help("specify the sentence input file.");

    auto& screenshot = program.add_mutually_exclusive_group();
    screenshot.add_argument("-s", "--step").help("specify the screen step.").scan<'i', int>();
    screenshot.add_argument("-a", "--all").flag().help("enable full parse trace.");
    screenshot.add_argument("-l", "--last").flag().help("only last screen.");

    try {
        program.parse_args(argc, argv);
    } catch (const std::exception& err) {
        std::cerr << err.what() << std::endl;
        std::cerr << program;
        std::exit(1);
    }

    std::vector<std::string> in;
    std::string grammar_src = program.get<std::string>("-g");
    if (auto fn = program.present("-i")) {
        std::stringstream ss(*fn);
        std::string token;
        while (ss >> token) {
            in.push_back(token);
        }
    } else {
        std::string input_src = program.get<std::string>("-f");
        std::fstream fs(input_src);
        std::string token;
        while (fs >> token) {
            in.push_back(token);
        }
        fs.close();
    }
    in.push_back("@");

    int step = NO_TRACE;
    if (auto fn = program.present<int>("-s")) {
        step = *fn;
    } else if (program["-a"] == true) {
        step = FULL_TRACE;
    } else if (program["-l"] == true) {
        step = LAST_TRACE;
    }

    Grammar gr(grammar_src);
    SLRTable t(gr);
    t.PrintTable(); // For printing table need to uncomment
    LRParser parser(t);
    if (parser.parse(in, step)) {
        std::cout<<"SUCCESSFULLY PARSED"<<std::endl;
    } else {
        std::cout<<"CAN`T PARSE THE INPUT"<<std::endl;
    }
}
