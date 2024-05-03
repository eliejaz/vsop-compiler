#ifndef AST_CLASSES_H
#define AST_CLASSES_H

#include <iostream>
#include <sstream>
#include <vector>
#include <set>
#include <map>
#include <string>
#include <iomanip>
#include "ClassSymbolTable.hpp"
#include "ProgramScope.hpp"
#include "CodeGenerator.hpp"
#include <llvm/Support/raw_ostream.h>

struct Position
{
    std::string fileName;
    int line;
    int column;
    Position(const std::string &fileName = "", int line = 0, int column = 0) : fileName(fileName), line(line), column(column) {}
};

class ASTNode
{
public:
    Position pos;
    ProgramScope* scope;
    virtual ~ASTNode() {delete scope;}
    virtual std::string print() const = 0;
    void setPosition(const Position &position) { pos = position; }
    void printSemanticError(const std::string &message) const
    {
        std::ostringstream oss;
        oss << pos.fileName << ":"
            << pos.line << ":"
            << pos.column << ": "
            << "semantic error: "
            << message
            << std::endl;
        std::cerr << oss.str();
    }
    virtual bool checkSemantics(__attribute__((unused))ClassSymbolTable* classSymbols = nullptr, ProgramScope* parentScope = nullptr) { 
    scope = parentScope;
    return true; 
    }

};

template <typename T>
std::string joinASTNodes(const std::vector<T *> &nodes)
{
    static_assert(std::is_base_of<ASTNode, T>::value, "T must be derived from ASTNode");
    std::ostringstream result;
    for (auto it = nodes.begin(); it != nodes.end(); ++it)
    {
        if (it != nodes.begin())
        {
            result << ", ";
        }
        result << (*it)->print();
    }
    return "[" + result.str() + "]";
}

class Type : public ASTNode
{
public:
    enum class TypeName
    {
        Int32,
        Bool,
        String,
        Unit,
        Custom,
        UNDEFINED
    };

private:
    TypeName typeName;
    std::string customTypeName;

public:
    Type(TypeName typeName) : typeName(typeName) {}

    // Constructor for TYPEIDENTIFIER types.
    Type(std::string customTypeName) : typeName(TypeName::Custom), customTypeName(customTypeName) {}
    TypeName getType() { return typeName; }
    void SetTypeCustom(std::string name) { customTypeName = name; }
    std::string getStringTypeName() const
    {
        switch (typeName)
        {
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
            return "UNDEFINED";
        }
    }
    std::string print() const override { return getStringTypeName(); }
    bool isSubtypeOf(const Type* base, ClassSymbolTable* classSymbols) const;
    bool isCompatibleWith(const Type* other, ClassSymbolTable* classSymbols) const;
    llvm::Type* typeToLLVM(CodeGenerator& generator);
};

class Expression : public ASTNode
{
public:
    Type *type = new Type(Type::TypeName::UNDEFINED);
    virtual ~Expression() {}
    std::string tryAddTypeToPrint(std::string str) const{
        if(type && type->getType() != Type::TypeName::UNDEFINED){
            return str + " : " + type->getStringTypeName();
        }
        return str;
    }
    virtual llvm::Value* codegen(__attribute__((unused))CodeGenerator& generator){
        return nullptr;
    }
};

class Literal : public Expression
{
public:
    virtual ~Literal() {}
};

class Block : public Expression
{
private:
    std::vector<Expression *> expressions;

public:
    Block(const std::vector<Expression *> &expressions)
        : expressions(expressions) {}

    ~Block()
    {
        for (auto expr : expressions)
        {
            delete expr;
        }
    }

    std::string print() const override
    {
        return tryAddTypeToPrint(joinASTNodes(expressions));
    }
    bool checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope) override;
    llvm::Value* codegen(CodeGenerator& generator) override;
};

class If : public Expression
{
private:
    Expression *condition;
    Expression *thenBranch;
    Expression *elseBranch;

public:
    If(Expression *condition, Expression *thenBranch, Expression *elseBranch = nullptr)
        : condition(condition), thenBranch(thenBranch), elseBranch(elseBranch) {}

    ~If()
    {
        delete condition;
        delete thenBranch;
        delete elseBranch;
    }

    std::string print() const override
    {
        std::ostringstream oss;
        oss << "If(" << condition->print() << ", " << thenBranch->print();
        if (elseBranch)
        {
            oss << ", " << elseBranch->print();
        }
        oss << ")";

        return tryAddTypeToPrint(oss.str());
    }
    bool checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope) override;
};

class While : public Expression
{
private:
    Expression *condition;
    Expression *body;

public:
    While(Expression *condition, Expression *body)
        : condition(condition), body(body) {}

    ~While()
    {
        delete condition;
        delete body;
    }

    std::string print() const override
    {
        return tryAddTypeToPrint("While(" + condition->print() + ", " + body->print() + ")");
    }

    bool checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope) override;
};

class Let : public Expression
{
private:
    std::string name;
    Type *letType;
    Expression *initExpr;
    Expression *scopeExpr;

public:
    Let(const std::string &name, Type *letType, Expression *scopeExpr, Expression *initExpr = nullptr)
        : name(name), letType(letType), initExpr(initExpr), scopeExpr(scopeExpr) {}

    ~Let()
    {
        delete type;
        delete initExpr;
        delete scopeExpr;
    }

    std::string print() const override
    {
        std::ostringstream oss;
        oss << "Let(" << name << ", " << letType->print();
        if (initExpr)
        {
            oss << ", " << initExpr->print();
        }
        oss << ", " << scopeExpr->print() << ")";
        return tryAddTypeToPrint(oss.str());
    }
    Type *getLetType() { return letType; }
    std::string getName() { return name; }
    bool checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope) override;
};

class UnaryOp : public Expression
{
public:
    enum class Op
    {
        Negate,
        Not,
        IsNull
    };

private:
    Op op;
    Expression *expr;

    std::string opToString() const
    {
        switch (op)
        {
        case Op::Negate:
            return "-";
        case Op::Not:
            return "not";
        case Op::IsNull:
            return "isnull";
        default:
            return "unknown";
        }
    }

public:
    UnaryOp(Op op, Expression *expr)
        : op(op), expr(expr) {}

    ~UnaryOp() { delete expr; }
    std::string print() const override
    {
        return tryAddTypeToPrint("UnOp(" + opToString() + ", " + expr->print() + ")");
    }
    bool checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope) override;
};

class BinaryOp : public Expression
{
public:
    enum class Op
    {
        Add,
        Subtract,
        Multiply,
        Divide,
        Power,
        Equal,
        LessThan,
        LessEqual,
        And
    };

private:
    Op op;
    Expression *left;
    Expression *right;

    std::string opToString() const
    {
        switch (op)
        {
        case Op::Add:
            return "+";
        case Op::Subtract:
            return "-";
        case Op::Multiply:
            return "*";
        case Op::Divide:
            return "/";
        case Op::Power:
            return "^";
        case Op::Equal:
            return "=";
        case Op::LessThan:
            return "<";
        case Op::LessEqual:
            return "<=";
        case Op::And:
            return "and";
        default:
            return "unknown";
        }
    }

public:
    BinaryOp(Op op, Expression *left, Expression *right)
        : op(op), left(left), right(right) {}

    ~BinaryOp()
    {
        delete left;
        delete right;
    }

    std::string print() const override
    {
        return tryAddTypeToPrint("BinOp(" + opToString() + ", " + left->print() + ", " + right->print() + ")");
    }
    bool checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope) override;
};

class Assign : public Expression
{
private:
    std::string name;
    Expression *expr;

public:
    Assign(const std::string &name, Expression *expr)
        : name(name), expr(expr) {}

    ~Assign()
    {
        delete expr;
    }

    std::string print() const override
    {
        return tryAddTypeToPrint("Assign(" + name + ", " + expr->print() + ")");
    }
    std::string getName() { return name; }
    bool checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope) override;
};

class New : public Expression
{
private:
    std::string typeName;

public:
    New(const std::string &typeName) : typeName(typeName) {}

    std::string print() const override
    {
        return tryAddTypeToPrint("New(" + typeName + ")");
    }

    bool checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope) override;
};

class ObjectId : public Expression
{
private:
    std::string id;

public:
    ObjectId(const std::string &id = "self") : id(id) {}

    std::string print() const override
    {
        return tryAddTypeToPrint(id);
    }
    std::string getId(){return id;}
    bool checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope) override;
};

class Call : public Expression
{
private:
    Expression *caller;
    std::string methodName;
    std::vector<Expression *> args;

public:
    Call(const std::string &methodName, const std::vector<Expression *> &args, Expression *caller = new ObjectId("self"))
        : caller(caller), methodName(methodName), args(args) {}

    ~Call()
    {
        delete caller;
        for (auto expr : args)
        {
            delete expr;
        }
    }

    std::string print() const override
    {
        return tryAddTypeToPrint("Call(" + caller->print() + ", " + methodName + ", " + joinASTNodes(args) + ")");
    }
    bool checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope) override;
};

class IntegerLiteral : public Literal
{
private:
    int value;

public:
    IntegerLiteral(int value) : value(value) { }

    std::string print() const override
    {
        return tryAddTypeToPrint(std::to_string(value));
    }
    bool checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope) override;
    llvm::Value* codegen(CodeGenerator& generator) override;
};

class StringLiteral : public Literal
{
private:
    std::string value;

public:
    StringLiteral(const std::string &value) : value(value) { }

    std::string print() const override
    {
        return tryAddTypeToPrint(value);
    }
    bool checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope) override;
    llvm::Value* codegen(CodeGenerator& generator) override;
};

class BooleanLiteral : public Literal
{
private:
    bool value;

public:
    BooleanLiteral(bool value) : value(value) {}

    std::string print() const override
    {
        return tryAddTypeToPrint(value ? "true" : "false");
    }
    bool checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope) override;
    llvm::Value* codegen(CodeGenerator& generator) override;

};

class UnitLiteral : public Literal
{
public:
    UnitLiteral() { }
    std::string print() const override
    {
        return tryAddTypeToPrint("()");
    }
    bool checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope) override;
    llvm::Value* codegen(CodeGenerator& generator) override;
};

class Field : public ASTNode
{
private:
    std::string name;
    Type *type;
    Expression *initExpr;

public:
    Field(const std::string &name, Type *type, Expression *initExpr = nullptr)
        : name(name), type(type), initExpr(initExpr) {}

    ~Field()
    {
        delete type;
        delete initExpr;
    }

    std::string print() const override
    {
        std::ostringstream oss;
        oss << "Field(" << name << ", " << type->print();
        if (initExpr)
        {
            oss << ", " << initExpr->print();
        }
        oss << ")";
        return oss.str();
    }

    Type *getType() { return type; }
    std::string getName() { return name; }
    bool checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope) override;
};

class Formal : public ASTNode
{
private:
    std::string name;
    Type *type;

public:
    Formal(const std::string &name, Type *type)
        : name(name), type(type) {}

    ~Formal() { delete type; }

    std::string print() const override
    {
        return name + ": " + type->print();
    }

    Type *getType() { return type; }
    std::string getName() { return name; }
    bool checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope) override;
};

class Method : public ASTNode
{
private:
    std::string name;
    std::vector<Formal *> formals;
    Type *returnType;
    Block *body;

private:
    bool checkMethodTypeDefinitions(ClassSymbolTable* classSymbols);
    bool checkFormalSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope);

public:
    Class *caller;
    Method(const std::string &name, std::vector<Formal *> formals,
           Type *returnType, Block *body)
        : name(name), formals(formals), returnType(returnType), body(body) {}

    ~Method()
    {
        delete body;
        for (auto formal : formals)
        {
            delete formal;
        }
    }
    Type *getReturnType()
    {
        return returnType;
    }
    std::string getName() { return name; }
    std::vector<Formal *> getFormals() { return formals; }

    std::string print() const override
    {
        std::ostringstream oss;
        oss << "Method(" << name << ", ";
        oss << joinASTNodes(formals) << ", ";
        oss << returnType->print() << ", ";
        oss << body->print() << ")";
        return oss.str();
    }
    bool checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope) override;

   llvm::Value* codegen(CodeGenerator& generator);
};

class Class : public ASTNode
{
private:
    std::string name;
    std::string parent;
    std::vector<Field *> fields;
    std::vector<Method *> methods;
    Class* parentClass;

private:
    bool areSignaturesEqual(Method *m1, Method *m2);
    bool checkInheritanceSemantic(ClassSymbolTable* classSymbols);
    bool checkMethodSignatures(ClassSymbolTable* classSymbols);
    bool checkClassFieldsSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope);

public:
    Class(const std::string &name,
          std::vector<Field *> fields,
          std::vector<Method *> methods,
          const std::string &parent = "Object")
        : name(name), parent(parent), fields(fields), methods(methods) {}

    ~Class()
    {
        for (auto field : fields)
            delete field;
        for (auto method : methods)
            delete method;
    }

    std::string getName() { return name; }
    std::string getParent() { return parent; }
    std::vector<Method *> getMethods() { return methods; }
    std::string print() const override
    {
        std::ostringstream oss;
        oss << "Class(" << name << ", " << parent << ", ";
        oss << joinASTNodes(fields) << ", ";
        oss << joinASTNodes(methods) << ")";
        return oss.str();
    }

    bool checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope) override;

    Method* getMethod(const std::string& methodName, ClassSymbolTable* classSymbols) {
        for (Method* method : methods) {
            if (method->getName() == methodName) {
                return method;
            }
        }
        Class* parentClass = classSymbols->getClass(parent);

        while (parentClass) {
            Method* method =  parentClass->getMethod(methodName, classSymbols);
            if(method)
                return method;
            parentClass = classSymbols->getClass(parentClass->getParent());
        }

        return nullptr;
    }

    void collectMethods(std::vector<Method*>& allMethods);
    void collectParentFields(std::vector<Field *>& allFields);
    void codegen(CodeGenerator& generator);
    llvm::Function* createClassNewFunction(CodeGenerator& generator, llvm::StructType* classType, llvm::GlobalVariable* vTable, const std::string& className);
};

class Program : public ASTNode
{
private:
    std::vector<Class *> classes;

private:
    Class *createObjectClass() const;
    bool checkMainClass(ClassSymbolTable* classSymbols);

public:
    Program(std::vector<Class *> classes)
        : classes(classes) {}

    ~Program()
    {
        for (auto classe : classes)
        {
            delete classe;
        }
    }

    std::string print() const override
    {
        return joinASTNodes(classes);
    }

    bool checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope) override;

    void codegen(CodeGenerator& generator);
};
#endif
