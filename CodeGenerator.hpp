#ifndef CODE_GENERATOR_H
#define CODE_GENERATOR_H

#include <llvm/IR/Value.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

class CodeGenerator {
public:
    llvm::LLVMContext context;
    llvm::Module* module;
    llvm::IRBuilder<> builder;

    CodeGenerator(std::string fileName)
        : module(new llvm::Module(fileName, context)), builder(context) {}

    llvm::Type* handleCustomType(const std::string& name) {
        llvm::StructType* structType = llvm::StructType::getTypeByName(context, name);
        if (structType) return structType;
        return llvm::StructType::create(context, name);
    }

    void printLLVMCode(){
        module->print(llvm::outs(), nullptr);
    }

    void printLLVMCodeToFile(std::string fileName){
        std::error_code EC;
        llvm::raw_fd_ostream dest(fileName, EC);
        if (EC) {
            std::cerr << "Could not open file: " << EC.message();
        }

        module->print(dest, nullptr);
        dest.flush();  
    }

};

#endif

