#include <iostream>
#include <sstream>
#include <vector>
#include <string>
#include <iomanip>

class ASTNode {
public:
    virtual ~ASTNode() { }
    virtual std::string print() const = 0;
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

class Expression : public ASTNode {
public:
    virtual ~Expression() { }
};

class Literal : public Expression {
public:
    virtual ~Literal() { }
};

class Type : public ASTNode {
public:
    enum class TypeName {
        Int32,
        Bool,
        String,
        Unit,
        Custom
    };

    TypeName typeName;
    std::string customTypeName;
    Type(TypeName typeName) : typeName(typeName) {}

    // Constructor for TYPEIDENTIFIER types.
    Type(std::string customTypeName) : typeName(TypeName::Custom), customTypeName(customTypeName) {}

    std::string print() const override  {
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
};

class UnaryOp : public Expression {
private:
    std::string op;
    Expression* expr;

public:
    UnaryOp(const std::string& op, Expression* expr)
        : op(op), expr(expr) {}

    ~UnaryOp() {
        delete expr;
    }

    std::string print() const override {
        return "UnOp(" + op + ", " + expr->print() + ")";
    }
};

class BinaryOp : public Expression {
private:
    std::string op;
    Expression* left;
    Expression* right;

public:
    BinaryOp(const std::string& op, Expression* left, Expression* right)
        : op(op), left(left), right(right) {}

    ~BinaryOp() {
        delete left;
        delete right;
    }

    std::string print() const override {
        return "BinOp(" + op + ", " + left->print() + ", " + right->print() + ")";
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
};


class New : public Expression {
private:
    std::string typeName;

public:
    New(const std::string& typeName) : typeName(typeName) {}

    std::string print() const override {
        return "New(" + typeName + ")";
    }
};

class ObjectId : public Expression {
private:
    std::string id;

public:
    ObjectId(const std::string& id) : id(id) {}

    std::string print() const override {
        return id;
    }
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
};

class IntegerLiteral : public Literal {
private:
    int value;

public:
    IntegerLiteral(int value) : value(value) {}

    std::string print() const override {
        return std::to_string(value);
    }
};

class StringLiteral : public Literal {
private:
    std::string value;

public:
    StringLiteral(const std::string& value) : value(value) {}

    std::string print() const override {
        return value;
    }
};

class BooleanLiteral : public Literal {
private:
    bool value;

public:
    BooleanLiteral(bool value) : value(value) {}

    std::string print() const override {
        return value ? "true" : "false";
    }
};

class UnitLiteral : public Literal {
public:
    UnitLiteral() = default;
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

    std::string print() const override {
        std::ostringstream oss;
        oss << "Method(" << name << ", ";
        oss << joinASTNodes(formals) << ", ";
        oss << returnType->print() << ", ";
        oss << body->print() << ")";
        return oss.str();
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

    std::string print() const override {
        std::ostringstream oss;
        oss << "Class(" << name << ", " << parent << ", ";
        oss << joinASTNodes(fields) << ", ";
        oss << joinASTNodes(methods) << ")";
        return oss.str();
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