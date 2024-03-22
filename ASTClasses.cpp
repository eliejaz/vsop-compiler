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

class Field : public ASTNode {
private:
    std::string name;
    std::string type;
    Expression* initExpr;

public:
    Field(const std::string& name, const std::string& type, Expression* initExpr = nullptr)
        : name(name), type(type), initExpr(initExpr) {}

    ~Field() {
    }

    std::string print() const override {
        std::ostringstream oss;
        oss << "Field(" << name << ", " << type;
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
    std::string type;

public:
    Formal(const std::string& name, const std::string& type)
        : name(name), type(type) {}

    std::string print() const override {
        return name + ": " + type;
    }
};

class Block : public Expression {
private:
    std::vector<Expression*> expressions;

public:
    Block(const std::vector<Expression*>& expressions)
        : expressions(expressions) {}

    ~Block() {
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

    ~If() {}

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

    ~While() {}

    std::string print() const override {
        return "While(" + condition->print() + ", " + body->print() + ")";
    }
};

class Let : public Expression {
private:
    std::string name;
    std::string type;
    Expression* initExpr;
    Expression* scopeExpr;

public:
    Let(const std::string& name, const std::string& type, Expression* scopeExpr, Expression* initExpr = nullptr)
        : name(name), type(type), initExpr(initExpr), scopeExpr(scopeExpr) {}

    ~Let() {}

    std::string print() const override {
        std::ostringstream oss;
        oss << "Let(" << name << ", " << type;
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

    ~UnaryOp() {}

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

    ~BinaryOp() {}

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

    ~Assign() {}

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

    ~Call() {}

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
    std::string returnType;
    Block* body; 

public:
    Method(const std::string& name, std::vector<Formal*> formals, 
           const std::string& returnType, Block* body)
        : name(name), formals(formals), returnType(returnType), body(body) {}

    ~Method() {}

    std::string print() const override {
        std::ostringstream oss;
        oss << "Method(" << name << ", ";
        oss << joinASTNodes(formals) << ", ";
        oss << returnType << ", ";
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

    ~Class() { }

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
    
    ~Program() {}

    std::string print() const override {
        return joinASTNodes(classes);
    }
};

class Error : public Expression {
private:
    std::string message;

public:
    Error(std::string message): message(message) {}

    ~Error() {}

    std::string print() const override {
        return message;
    }
};


//Test the AST
// int main() {
//     // Direct instantiation of AST nodes
//     IntegerLiteral integerLiteral(42);

//     std::vector<Expression*> printMethodArgs;
//     printMethodArgs.push_back(&integerLiteral);

//     // Note: In a non-unique_ptr version, managing dynamic memory (if needed)
//     // would require careful allocation and deletion to prevent leaks.
//     Call printCall("print", printMethodArgs);

//     std::vector<Expression*> blockExpr;
//     blockExpr.push_back(&printCall);
//     Block mainBlock(blockExpr);

//     Formal formal("value1", "type1");
//     std::vector<Formal*> formals;
//     formals.push_back(&formal);

//     Method mainMethod("main", formals, "void", &mainBlock);

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