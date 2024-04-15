#ifndef PROGRAM_SCOPE_H
#define PROGRAM_SCOPE_H

#include <map>

class Type;

class ProgramScope {
private:
    std::map<std::string, Type*> symbolToTypeMap;
    ProgramScope* parentScope;

public:
    ProgramScope(ProgramScope* parent = nullptr) : parentScope(parent) {}

    bool addSymbol(const std::string& name, Type* type) {
        if (symbolToTypeMap.find(name) == symbolToTypeMap.end()) {
            symbolToTypeMap[name] = type;
            return true;
        }
        return false;
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
};
#endif
