#include <iostream>
#include <string>
#include "CodeGenerator.hpp"

#include "driver.hpp"

using namespace std;

enum class Mode
{
    LEX,
    PARSE,
    SEMANTIC,
    GENERATE,
    GENERATEFILE
};

static const map<string, Mode> flag_to_mode = {
    {"-l", Mode::LEX},
    {"-p", Mode::PARSE},
    {"-c", Mode::SEMANTIC},
    {"-i", Mode::GENERATE},
};

int main(int argc, char const *argv[])
{
    Mode mode;
    string source_file;

    if (argc == 2)
    {
        mode = Mode::GENERATEFILE;
        source_file = argv[1];
    }
    else if (argc == 3)
    {
        if (flag_to_mode.count(argv[1]) == 0)
        {
            cerr << "Invalid mode: " << argv[1] << endl;
            return -1;
        }
        mode = flag_to_mode.at(argv[1]);
        source_file = argv[2];
    }
    else
    {
        cerr << "Usage: " << argv[0] << " [-l|-p|-c] <source_file>" << endl;
        return -1;
    }

    VSOP::Driver driver = VSOP::Driver(source_file);

    int res;
    switch (mode)
    {
    case Mode::LEX:
        res = driver.lex();
        driver.print_tokens();
        return res;

    case Mode::PARSE:
        res = driver.parse();

        if (res == 0)
            cout << driver.result->print() << endl;
        return res;
    case Mode::SEMANTIC:
        res = driver.parse();

        if (res == 0){
            if(!driver.result->checkSemantics(nullptr, nullptr))
                res = -1;
        }
        if (res == 0){
            cout << driver.result->print() << endl;
        }
        return res;    
    case Mode::GENERATE:
        res = driver.parse();
        if (res == 0){
            if(!driver.result->checkSemantics(nullptr, nullptr))
                res = -1;
        }
        if (res == 0){
            std::string currentFileName = driver.result->pos.fileName;
            size_t lastPeriodIndex = currentFileName.rfind('.');
            if (lastPeriodIndex != std::string::npos && lastPeriodIndex != 0) {
                // Erase the extension
                currentFileName.erase(lastPeriodIndex);
            }
            CodeGenerator generator(currentFileName);
            driver.result->codegen(generator);
            generator.printLLVMCode();
        }
        return res; 
case Mode::GENERATEFILE:
    res = driver.parse();
    if (res == 0){
        if(!driver.result->checkSemantics(nullptr, nullptr))
            res = -1;
    }
    if (res == 0){
        std::string currentFileName = driver.result->pos.fileName;
        size_t lastPeriodIndex = currentFileName.rfind('.');
        if (lastPeriodIndex != std::string::npos && lastPeriodIndex != 0) {
            // Erase the extension
            currentFileName.erase(lastPeriodIndex);
        }
        CodeGenerator generator(currentFileName);
        driver.result->codegen(generator);


        std::string llFileName = currentFileName + ".ll";
        std::string executableName = currentFileName;

        generator.printLLVMCodeToFile(llFileName);

        std::string llcCommand = "clang -lm -o " + executableName + " " + llFileName;
        system(llcCommand.c_str());
        
    }
    return res;            
    }


    return 0;
}
