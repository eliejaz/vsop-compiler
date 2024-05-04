#ifndef CLASS_SYMBOL_TABLE_H
#define CLASS_SYMBOL_TABLE_H

#include <map>
#include <string>

class Class;

class ClassSymbolTable {
public:
    std::map<std::string, Class*> classes;

     bool addClass(const std::string& name, Class* classDef) {
        if (classes.find(name) != classes.end()) {
            return false;
        }
        classes[name] = classDef;
        return true;
    }

    Class* getClass(const std::string& name) {
        auto it = classes.find(name);
        if (it != classes.end()) {
            return it->second;
        }
        return nullptr;
    }

    bool hasClass(const std::string& name) {
        return classes.find(name) != classes.end();
    }

};

#endif
