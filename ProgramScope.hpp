#ifndef PROGRAM_SCOPE_H
#define PROGRAM_SCOPE_H

#include <map>
#include "ASTClasses.hpp"

class Type;

class ProgramScope {

public:
    bool inFieldInitializer;
    std::map<std::string, Type*> symbolToTypeMap;
    ProgramScope* parentScope;

    ProgramScope(ProgramScope* parent = nullptr) : parentScope(parent) {}

   bool addSymbol(const std::string& name, Type* type) {
        ProgramScope* currentScope = this;
        if (currentScope) {
            if ( currentScope->symbolToTypeMap.find(name) != currentScope->symbolToTypeMap.end()) {
                // Symbol already exists in the curren scope
                return false;
            }
        }
        symbolToTypeMap[name] = type;
        return true;
    }

    Type* lookup(const std::string& name) {
        ProgramScope* currentScope = this;
        while (currentScope != nullptr) {
            auto it = currentScope->symbolToTypeMap.find(name);
            if (it != currentScope->symbolToTypeMap.end()) {
                return it->second;
            }
            currentScope = currentScope->parentScope;
        }
        return nullptr;
    }

    void setParentScope(ProgramScope* parent){
        parentScope = parent;
    }

};
#endif
