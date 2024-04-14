#include <iostream>
#include <sstream>
#include <vector>
#include <set>
#include <map>
#include <string>
#include <iomanip>
#include "ClassSymbolTable.hpp"

struct Position {
    std::string fileName;
    int line;
    int column;
    Position(const std::string& fileName="", int line = 0, int column = 0) : fileName(fileName), line(line), column(column) {}
};

class ASTNode {

public:
    Position pos;
    virtual ~ASTNode() { }
    virtual std::string print() const = 0;
    void setPosition(const Position& position) { pos = position; }
    void printSemanticError(const std::string& message) const{
        std::ostringstream oss;
        oss << pos.fileName << ":"
         << pos.line << ":" 
         << pos.column << ": "
         << "Semantic error: "
         << message
         <<std::endl;
         std::cerr << oss.str();
    }
    

};

template<typename T>
std::string joinASTNodes(const std::vector<T*>& nodes) {
    static_assert(std::is_base_of<ASTNode, T>::value, "T must be derived from ASTNode");
    std::ostringstream result;
    for (auto it = nodes.begin(); it != nodes.end(); ++it) {
        if (it != nodes.begin()) {
            result << ", ";
        }
        result << (*it)->print();
    }
    return "[" + result.str() + "]";
}

class Type : public ASTNode {
public:
    enum class TypeName {
        Int32,
        Bool,
        String,
        Unit,
        Custom
    };
private:
    TypeName typeName;
    std::string customTypeName;

public:
    Type(TypeName typeName) : typeName(typeName) {}

    // Constructor for TYPEIDENTIFIER types.
    Type(std::string customTypeName) : typeName(TypeName::Custom), customTypeName(customTypeName) {}
    TypeName getType(){return typeName;}
    void SetTypeCustom(std::string name) {customTypeName = name;}
    std::string getTypeName() const{
        switch (typeName) {
            case TypeName::Int32:
                return "int32";
            case TypeName::Bool:
                return "bool";
            case TypeName::String:
                return "string";
            case TypeName::Unit:
                return "unit";
            case TypeName::Custom:
                return customTypeName;
            default:
                return "unknown";
        }
    }
    std::string print() const override  {
        return getTypeName();
    }

};

class Expression : public ASTNode {
public:
    Type* type;
    virtual bool CheckSemantics() {return true;}
    virtual ~Expression() { }
};

class Literal : public Expression {
public:
    virtual ~Literal() { }
};

class Field : public ASTNode {
private:
    std::string name;
    Type* type;
    Expression* initExpr;

public:
    Field(const std::string& name, Type* type, Expression* initExpr = nullptr)
        : name(name), type(type), initExpr(initExpr) {}

    ~Field() {
        delete type;
        delete initExpr;
    }

    std::string print() const override {
        std::ostringstream oss;
        oss << "Field(" << name << ", " << type->print();
        if (initExpr) {
            oss << ", " << initExpr->print();
        }
        oss << ")";
        return oss.str();
    }

    Type* getType(){return type;}
    std::string getName(){return name;}

    bool CheckSemantics(){
        bool noError = true;
        
        //check type
        if(initExpr && type->getType() != initExpr->type->getType()){
            noError = false;
            std::ostringstream oss;
            oss << "Type mismatch for -> " << name << " : " << type->getTypeName() << ", but got : " << initExpr->type->getTypeName();
            printSemanticError(oss.str());
        }
        return noError;
    }

};

class Formal : public ASTNode {
private:
    std::string name;
    Type* type;

public:
    Formal(const std::string& name, Type* type)
        : name(name), type(type) {}

    ~Formal() {
            delete type;
        }

    std::string print() const override {
        return name + ": " + type->print();
    }
    Type* getType(){return type;}
    std::string getName(){return name;}
};

class Block : public Expression {
private:
    std::vector<Expression*> expressions;

public:
    Block(const std::vector<Expression*>& expressions)
        : expressions(expressions) {}

    ~Block() {
        for (auto expr : expressions) {
            delete expr;
         }
    }

    std::string print() const override {
        return joinASTNodes(expressions);
    }

    bool CheckSemantics() override {
        bool noError = true; 
        for(auto* exp : expressions){
            noError &= exp->CheckSemantics();
        }

        //type of the last expression
        type = new Type(expressions.back()->type->getType());

        return noError;
    }
};

class If : public Expression {
private:
    Expression* condition;
    Expression* thenBranch;
    Expression* elseBranch;

public:
    If(Expression* condition, Expression* thenBranch, Expression* elseBranch = nullptr)
        : condition(condition), thenBranch(thenBranch), elseBranch(elseBranch) {}

    ~If() {
        delete condition;
        delete thenBranch;
        delete elseBranch;
    }

    std::string print() const override {
        std::ostringstream oss;
        oss << "If(" << condition->print() << ", " << thenBranch->print();
        if (elseBranch) {
            oss << ", " << elseBranch->print();
        }
        oss << ")";
        return oss.str();
    }

    bool CheckSemantics() override{
        bool noError = true;
        std::ostringstream oss;
        
        noError &= condition->CheckSemantics();
        noError &= thenBranch->CheckSemantics();
        noError &= elseBranch->CheckSemantics();

        if(condition->type->getType() != Type::TypeName::Bool){
            oss << "Type mismatch for the condiction in 'If' type is : '" << condition->type->getTypeName() << "', expected : 'bool'";
            printSemanticError(oss.str());
        }

        //assign type

        if(thenBranch->type->getType() == Type::TypeName::Custom && elseBranch->type->getType() == Type::TypeName::Custom){
            //TO DO find common ancestor
            type = new Type(Type::TypeName::Custom);
        } else if (thenBranch->type->getType() == elseBranch->type->getType()) {
            type = new Type(elseBranch->type->getType());
        } else if (thenBranch->type->getType() == Type::TypeName::Unit || elseBranch->type->getType() == Type::TypeName::Unit){
            type = new Type(Type::TypeName::Unit);
        } else {
            oss << "Type mismatch for in 'If', both branch don't agree, type are : '" << thenBranch->type->getTypeName() << "' and '" << elseBranch->type->getTypeName() << "'";
            printSemanticError(oss.str());            
        }

        return noError;
    }
};

class While : public Expression {
private:
    Expression* condition;
    Expression* body;

public:
    While(Expression* condition, Expression* body)
        : condition(condition), body(body) {}

    ~While() {
        delete condition;
        delete body;
    }

    std::string print() const override {
        return "While(" + condition->print() + ", " + body->print() + ")";
    }

    bool CheckSemantincs(){
        bool noError = true;
        std::ostringstream oss;
        //Down first
        noError &= condition->CheckSemantics();
        noError &= body->CheckSemantics();

        //assign type
        type = new Type(Type::TypeName::Unit);

        //type check
        if(condition->type->getType() != Type::TypeName::Bool){
            oss << "Type mismatch for condiction in 'While' type is : '" << condition->type->getTypeName() << "', expected : 'bool'";
            printSemanticError(oss.str());
        }

        return noError;
    }
};

class Let : public Expression {
private:
    std::string name;
    Type* type;
    Expression* initExpr;
    Expression* scopeExpr;

public:
    Let(const std::string& name, Type* type, Expression* scopeExpr, Expression* initExpr = nullptr)
        : name(name), type(type), initExpr(initExpr), scopeExpr(scopeExpr) {}

    ~Let() {
        delete type;
        delete initExpr;
        delete scopeExpr;
    }

    std::string print() const override {
        std::ostringstream oss;
        oss << "Let(" << name << ", " << type->print();
        if (initExpr) {
            oss << ", " << initExpr->print();
        }
        oss << ", " << scopeExpr->print() << ")";
        return oss.str();
    }
    Type* getType(){return type;}
    std::string getName(){return name;}

    bool CheckSemantics() override{
        bool noError = true;

        //Down first
        noError &= initExpr->CheckSemantics();
        noError &= scopeExpr->CheckSemantics();

        Expression::type = new Type(scopeExpr->type->getType());

        //type check
        if(type->getType() != initExpr->type->getType() && initExpr != nullptr){
            noError = false;
            std::ostringstream oss;
            oss << "Type mismatch in 'Let' type is : '" << initExpr->type->getTypeName() << "', expected : " << type->getTypeName();
            printSemanticError(oss.str());
        }

        return noError;
    }

};

class UnaryOp : public Expression {
public:
    enum class Op { Negate, Not, IsNull };

private:
    Op op;
    Expression* expr;

    std::string opToString() const {
        switch (op) {
            case Op::Negate: return "-";
            case Op::Not: return "not";
            case Op::IsNull: return "isnull";
            default: return "unknown";
        }
    }

public:
    UnaryOp(Op op, Expression* expr)
        : op(op), expr(expr) {}

    ~UnaryOp() {
        delete expr;
    }

    std::string print() const override {
        return "UnOp(" + opToString() + ", " + expr->print() + ")";
    }

    bool CheckSemantics() override {
        bool noError = true;
        std::ostringstream oss;
        //Down first
        noError &= expr->CheckSemantics();

        //assign type
        switch (op) {
            case Op::Negate: type = new Type(Type::TypeName::Int32);; break;
            case Op::Not: type = new Type(Type::TypeName::Bool);; break;
            case Op::IsNull: type = new Type(Type::TypeName::Bool);; break;
        }

        //type check
        switch (op) {
            case Op::Negate:
            {
                if(expr->type->getType() != Type::TypeName::Int32){
                    noError = false;
                    oss << "Type mismatch for Unary operator : " << opToString() << " . Type is : '" << expr->type->getTypeName() << " expected : 'int32'";
                    printSemanticError(oss.str());
                }
            }; break;
            case Op::Not:
            {
                if(expr->type->getType() != Type::TypeName::Bool){
                    noError = false;
                    oss << "Type mismatch for Unary operator : " << opToString() << " . Type is : '" << expr->type->getTypeName() << " expected : 'bool'";
                    printSemanticError(oss.str());
                }
            }; break;
            case Op::IsNull: //all type can be used
            default: return noError;
        }        

        return noError;
    }
};


class BinaryOp : public Expression {
public:
    enum class Op { Add, Subtract, Multiply, Divide, Power, Equal, LessThan, LessEqual, And };

private:
    Op op;
    Expression* left;
    Expression* right;

    std::string opToString() const {
        switch (op) {
            case Op::Add: return "+";
            case Op::Subtract: return "-";
            case Op::Multiply: return "*";
            case Op::Divide: return "/";
            case Op::Power: return "^";
            case Op::Equal: return "=";
            case Op::LessThan: return "<";
            case Op::LessEqual: return "<=";
            case Op::And: return "and";
            default: return "unknown";
        }
    }

public:
    BinaryOp(Op op, Expression* left, Expression* right)
        : op(op), left(left), right(right) {}

    ~BinaryOp() {
        delete left;
        delete right;
    }

    std::string print() const override {
        return "BinOp(" + opToString() + ", " + left->print() + ", " + right->print() + ")";
    }

    bool CheckSemantics() override {
        bool noError = true;
        std::ostringstream oss;

        //Down first
        noError &= left->CheckSemantics();
        noError &= right->CheckSemantics();



        //assign type
        switch (op) {
            case Op::Add:
            case Op::Subtract: 
            case Op::Multiply: 
            case Op::Divide:
            case Op::Power: 
            case Op::LessThan: 
            case Op::LessEqual: type = new Type(Type::TypeName::Int32); break;
            case Op::Equal: type = new Type(left->type->getType()); break;
            case Op::And: type = new Type(Type::TypeName::Bool); break;
        }
        
        //type check
        switch (op)
        {
            case Op::Add:
            case Op::Subtract:
            case Op::Multiply:
            case Op::Divide:
            case Op::Power:
            case Op::LessThan:
            case Op::LessEqual:
            {
                if(left->type->getType() != Type::TypeName::Int32 || right->type->getType() != Type::TypeName::Int32){
                    noError = false;
                    oss << "Type mismatch for binary operator : " << opToString() << " . Types are : '" << left->type->getTypeName() << "' and '" << right->type->getTypeName() << "'";
                    printSemanticError(oss.str());
                }
            }; break;
            case Op::Equal: return "=";
            {
                if(left->type != right->type){
                    noError = false;
                    oss << "Type mismatch for binary operator : " << opToString() << " . Types are : '" << left->type->getTypeName() << "' and '" << right->type->getTypeName() << "'";
                    printSemanticError(oss.str());
                }
            }; break;
            case Op::And: return "and";
            {
                if(left->type->getType() != Type::TypeName::Bool || right->type->getType() != Type::TypeName::Bool ){
                    noError = false;
                    oss << "Type mismatch for binary operator : " << opToString() << " . Types are : '" << left->type->getTypeName() << "' and '" << right->type->getTypeName() << "'";
                    printSemanticError(oss.str());
                }
            }; break;
            default: return noError;
            return noError;
        }

        return noError;
    }
};

class Assign : public Expression {
private:
    std::string name;
    Expression* expr;

public:
    Assign(const std::string& name, Expression* expr)
        : name(name), expr(expr) {}

    ~Assign() {
        delete expr;
    }

    std::string print() const override {
        return "Assign(" + name + ", " + expr->print() + ")";
    }
    std::string getName(){return name;}

    bool CheckSemantics() override {
        bool noError = true;

        //Down first
        noError &= expr->CheckSemantics();

        //TODO check ID

        type = new Type(expr->type->getType());

        return noError;
    }
};


class New : public Expression {
private:
    std::string typeName;

public:
    New(const std::string& typeName) : typeName(typeName) {}

    std::string print() const override {
        return "New(" + typeName + ")";
    }

    bool CheckSemantics() override {
        bool noError = true;

        type = new Type(Type::TypeName::Custom);
        type->SetTypeCustom(typeName);

        return noError;
    }
};

class ObjectId : public Expression {
private:
    std::string id;

public:
    ObjectId(const std::string& id = "self") : id(id) {}

    std::string print() const override {
        return id;
    }

    //TODO SEMANTICS

};

class Call : public Expression {
private:
    Expression* caller;
    std::string methodName;
    std::vector<Expression*> args;

public:
    Call(const std::string& methodName, const std::vector<Expression*>& args, Expression* caller = new ObjectId("self"))
        : caller(caller), methodName(methodName), args(args) {}

    ~Call() {
        delete caller;
        for (auto expr : args) {
            delete expr;
        }
    }

    std::string print() const override {
        return "Call(" + caller->print() + ", " + methodName + ", " + joinASTNodes(args) + ")";
    }

    bool CheckSemantics() override {
        bool noError = true;

        //TODO FIND METHOD
        //type = new Type(/*method type*/);

        return noError;
    }
};

class IntegerLiteral : public Literal {
private:
    int value;

public:
    IntegerLiteral(int value) : value(value) {type = new Type(Type::TypeName::Int32);}

    std::string print() const override {
        return std::to_string(value);
    }
};

class StringLiteral : public Literal {
private:
    std::string value;

public:
    StringLiteral(const std::string& value) : value(value) {type = new Type(Type::TypeName::String);}

    std::string print() const override {
        return value;
    }
};

class BooleanLiteral : public Literal {
private:
    bool value;

public:
    BooleanLiteral(bool value) : value(value) {type = new Type(Type::TypeName::Bool);}

    std::string print() const override {
        return value ? "true" : "false";
    }
};

class UnitLiteral : public Literal {
public:
    UnitLiteral() {type = new Type(Type::TypeName::Unit);}
    std::string print() const override {
        return "()";
    }
};


class Method : public ASTNode {
private: 
    std::string name;
    std::vector<Formal*> formals;
    Type* returnType;
    Block* body; 

public:
    Method(const std::string& name, std::vector<Formal*> formals, 
           Type* returnType, Block* body)
        : name(name), formals(formals), returnType(returnType), body(body) {}

    ~Method() {
        delete body;
        for (auto formal : formals) {
            delete formal;
        }

    }
    Type* getReturnType(){
        return returnType;
    }
    std::string getName(){return name;}
    std::vector<Formal*> getFormals(){return formals;}
    
    std::string print() const override {
        std::ostringstream oss;
        oss << "Method(" << name << ", ";
        oss << joinASTNodes(formals) << ", ";
        oss << returnType->print() << ", ";
        oss << body->print() << ")";
        return oss.str();
    }

    bool CheckSemantics(){
        bool noError = true;
        
        noError &= body->CheckSemantics();

        return noError;
    }
};

class Class : public ASTNode {
private:
    std::string name;
    std::string parent;
    std::vector<Field*> fields;
    std::vector<Method*> methods;

public:
    Class(const std::string& name, 
          std::vector<Field*> fields, 
          std::vector<Method*> methods,
          const std::string& parent = "Object")
        : name(name), parent(parent), fields(fields), methods(methods) {}

    ~Class() {
        for (auto field : fields) {
            delete field;
        }
        for (auto method : methods) {
            delete method;
        }

     }

    std::string getName(){return name;}
    std::string getParent(){return parent;}
    std::vector<Method*> getMethods(){return methods;}

    std::string print() const override {
        std::ostringstream oss;
        oss << "Class(" << name << ", " << parent << ", ";
        oss << joinASTNodes(fields) << ", ";
        oss << joinASTNodes(methods) << ")";
        return oss.str();
    }

    bool checkSemantics(const ClassSymbolTable& classSymbols) {
        bool noError = true;
        noError &= checkTypeDefinitions(classSymbols);
        noError &= checkFieldRedefinitions();
        noError &= checkMethodSignatures(classSymbols);

        //Field
        for (auto* fl : fields){
            noError &= fl->CheckSemantics();
        }

        //methode
        for (auto* mtd : methods){
            noError &= mtd->CheckSemantics();
        }


        return noError;
    }
    bool areSignaturesEqual(Method* m1, Method* m2) {
        if (m1->getReturnType()->getTypeName() != m2->getReturnType()->getTypeName()) {
            return false;
        }

        const auto& formals1 = m1->getFormals();
        const auto& formals2 = m2->getFormals();
        if (formals1.size() != formals2.size()) {
            return false;
        }
        for (size_t i = 0; i < formals1.size(); ++i) {
            if (formals1[i]->getType()->getTypeName() != formals2[i]->getType()->getTypeName()) {
                return false;
            }
        }

        return true;
    }
    
    bool checkFieldRedefinitions() {
        bool noError = true;
        std::set<std::string> fieldNames;
        for (auto* field : fields) {
            if (!fieldNames.insert(field->getName()).second) {
                std::cerr << "Semantic error: Redefinition of field '" << field->getName()
                        << "' in class '" << name << "'." << std::endl;
                noError = false;
            }
        }
        return noError;
    }

    bool checkMethodSignatures(const ClassSymbolTable& classSymbols) {
        bool noError = true;
        std::map<std::string, Method*> methodSignatures;
        for (auto* method : methods) {
            auto result = methodSignatures.insert({method->getName(), method});
            if (!result.second && !areSignaturesEqual(result.first->second, method)) {
                std::cerr << "Method '" << method->getName()
                        << "' is redefined with a different signature in class '" << name << "'.";
                noError = false;
            }
        }

        // Check against parent class methods
        Class* parentClass = classSymbols.getClass(parent);
        while (parentClass) {
            for (auto* parentMethod : parentClass->getMethods()) {
                auto it = methodSignatures.find(parentMethod->getName());
                if (it != methodSignatures.end() && !areSignaturesEqual(it->second, parentMethod)) {
                    std::ostringstream oss;
                    oss << "Overriding method '" << parentMethod->getName()
                            << "' with different signature in class " << name << ".";
                    printSemanticError(oss.str());
                    noError = false;
                }
            }
            parentClass = classSymbols.getClass(parentClass->getParent());
        }
        return noError;
    }

    bool checkTypeDefinitions(const ClassSymbolTable& classSymbols) {
        bool noError = true;
        for (auto* field : fields) {
            if (field->getType()->getType() == Type::TypeName::Custom && !classSymbols.getClass(field->getType()->getTypeName())) {
                std::ostringstream oss;
                oss << "Semantic error: Unknown type " << field->getType()->getTypeName()
                        << " used in field " << field->getName() << " in class " << name << ".";
                printSemanticError(oss.str());
                noError = false;
            }
        }
        for (auto* method : methods) {
            if (method->getReturnType()->getType() == Type::TypeName::Custom && !classSymbols.getClass(method->getReturnType()->getTypeName())) {
                std::ostringstream oss;
                oss << "Unknown return type " << method->getReturnType()->getTypeName()
                        << " of method " << method->getName() << " in class " << name << ".";
                printSemanticError(oss.str());
                noError = false;
            }
        }
        return noError;
    }

};

class Program : public ASTNode {
private:
    std::vector<Class*> classes;

public:
    Program(std::vector<Class*> classes)
            : classes(classes) {}
    
    ~Program() {
        for (auto classe : classes) {
            delete classe;
        }
    }

    std::string print() const override {
        return joinASTNodes(classes);
    }

   
    bool checkSemantics() const {
        bool noError = true;
        ClassSymbolTable classSymbols;

        // Add all classes to the symbol table, report redefinitions
        for (auto* cls : classes) {
            if (!classSymbols.addClass(cls->getName(), cls)) {
                std::ostringstream oss;
                oss << cls->getName() << " is redefined.";
                printSemanticError(oss.str());
                noError = false;
            }
        }

        // Check inheritance and detect cycles
        for (auto* cls : classes) {
            std::set<std::string> visited;
            std::string current = cls->getName();
            while (current != "Object") {
                if (visited.find(current) != visited.end()) {
                    std::ostringstream oss;
                    oss << "Inheritance cycle detected involving class " << current << ".";
                    printSemanticError(oss.str());
                    noError = false;
                    break;
                }
                visited.insert(current);

                Class* parentClass = classSymbols.getClass(current);
                if (!parentClass) {
                    std::ostringstream oss;
                    oss << "Class " << current << " extends an undefined class.";
                    printSemanticError(oss.str());
                    noError = false;
                    break;
                }
                current = parentClass->getParent();
            }
        }

        for (auto* cls : classes) {
            noError &= cls->checkSemantics(classSymbols);
        }

        return noError;
    }

};



//Test the AST
// int main() {
//     IntegerLiteral integerLiteral(42);

//     std::vector<Expression*> printMethodArgs;
//     printMethodArgs.push_back(&integerLiteral);

//     Call printCall("print", printMethodArgs);

//     std::vector<Expression*> blockExpr;
//     blockExpr.push_back(&printCall);
//     Block mainBlock(blockExpr);

//     Type* formalType = new Type(Type::TypeName::String);
//     Formal formal("value1", formalType);
//     std::vector<Formal*> formals;
//     formals.push_back(&formal);

//     Type* methodReturnType = new Type(Type::TypeName::Unit);
//     Method mainMethod("main", formals, methodReturnType, &mainBlock);

//     std::vector<Method*> methods;
//     methods.push_back(&mainMethod);

//     Class myClass("MyClass", std::vector<Field*>(), methods);

//     std::vector<Class*> classes;
//     classes.push_back(&myClass);

//     Program program(classes);

//     // Print the AST
//     std::cout << program.print() << std::endl;

//     return 0;
// }