#include "ASTClasses.hpp"

/* Type */
bool Type::isSubtypeOf(const Type* base, const ClassSymbolTable& classSymbols) const {
    if (customTypeName == base->getStringTypeName()) {
        return true;
    }
    Class* thisClass = classSymbols.getClass(customTypeName);
    while (thisClass != nullptr) {
        if (thisClass->getName() == base->getStringTypeName()) {
            return true;
        }
        thisClass = classSymbols.getClass(thisClass->getParent());
    }
    return false;
}

bool Type::isCompatibleWith(const Type* other, const ClassSymbolTable& classSymbols) const {
    if (typeName == TypeName::Unit || other->typeName == TypeName::Unit) {
        // Everything is compatible with Unit type
        return true;
    }
    if (typeName == other->typeName && typeName != TypeName::Custom) {
        // Primitive types match exactly
        return true;
    }
    if (typeName == TypeName::Custom && other->typeName == TypeName::Custom) {
        return isSubtypeOf(other, classSymbols);
    }
    return false;
}

/* Block */
bool Block::CheckSemantics()
{
    bool noError = true;
    for (auto *exp : expressions)
    {
        noError &= exp->CheckSemantics();
    }

    // type of the last expression
    type = new Type(expressions.back()->type->getType());

    return noError;
}

/* If */
bool If::CheckSemantics()
{
    bool noError = true;
    std::ostringstream oss;

    noError &= condition->CheckSemantics();
    noError &= thenBranch->CheckSemantics();
    noError &= elseBranch->CheckSemantics();

    if (condition->type->getType() != Type::TypeName::Bool)
    {
        oss << "Type mismatch for the condiction in 'If' type is : '" << condition->type->getStringTypeName() << "', expected : 'bool'";
        printSemanticError(oss.str());
    }

    // assign type

    if (thenBranch->type->getType() == Type::TypeName::Custom && elseBranch->type->getType() == Type::TypeName::Custom)
    {
        // TO DO find common ancestor
        type = new Type(Type::TypeName::Custom);
    }
    else if (thenBranch->type->getType() == elseBranch->type->getType())
    {
        type = new Type(elseBranch->type->getType());
    }
    else if (thenBranch->type->getType() == Type::TypeName::Unit || elseBranch->type->getType() == Type::TypeName::Unit)
    {
        type = new Type(Type::TypeName::Unit);
    }
    else
    {
        oss << "Type mismatch for in 'If', both branch don't agree, type are : '" << thenBranch->type->getStringTypeName() << "' and '" << elseBranch->type->getStringTypeName() << "'";
        printSemanticError(oss.str());
    }

    return noError;
}

/* While */
bool While::CheckSemantincs()
{
    bool noError = true;
    std::ostringstream oss;
    // Down first
    noError &= condition->CheckSemantics();
    noError &= body->CheckSemantics();

    // assign type
    type = new Type(Type::TypeName::Unit);

    // type check
    if (condition->type->getType() != Type::TypeName::Bool)
    {
        oss << "Type mismatch for condiction in 'While' type is : '" << condition->type->getStringTypeName() << "', expected : 'bool'";
        printSemanticError(oss.str());
    }

    return noError;
}

/* UnaryOp */
bool Let::CheckSemantics()
{
    bool noError = true;

    // Down first
    noError &= initExpr->CheckSemantics();
    noError &= scopeExpr->CheckSemantics();

    Expression::type = new Type(scopeExpr->type->getType());

    // type check
    if (type->getType() != initExpr->type->getType() && initExpr != nullptr)
    {
        noError = false;
        std::ostringstream oss;
        oss << "Type mismatch in 'Let' type is : '" << initExpr->type->getStringTypeName() << "', expected : " << type->getStringTypeName();
        printSemanticError(oss.str());
    }

    return noError;
}

/* UnaryOp */
bool UnaryOp::CheckSemantics()
{
    bool noError = true;
    std::ostringstream oss;
    // Down first
    noError &= expr->CheckSemantics();

    // assign type
    switch (op)
    {
    case Op::Negate:
        type = new Type(Type::TypeName::Int32);
        ;
        break;
    case Op::Not:
        type = new Type(Type::TypeName::Bool);
        ;
        break;
    case Op::IsNull:
        type = new Type(Type::TypeName::Bool);
        ;
        break;
    }

    // type check
    switch (op)
    {
    case Op::Negate:
    {
        if (expr->type->getType() != Type::TypeName::Int32)
        {
            noError = false;
            oss << "Type mismatch for Unary operator : " << opToString() << " . Type is : '" << expr->type->getStringTypeName() << " expected : 'int32'";
            printSemanticError(oss.str());
        }
    };
    break;
    case Op::Not:
    {
        if (expr->type->getType() != Type::TypeName::Bool)
        {
            noError = false;
            oss << "Type mismatch for Unary operator : " << opToString() << " . Type is : '" << expr->type->getStringTypeName() << " expected : 'bool'";
            printSemanticError(oss.str());
        }
    };
    break;
    case Op::IsNull: // all type can be used
    default:
        return noError;
    }

    return noError;
}

/* BinaryOp */
bool BinaryOp::CheckSemantics()
{
    bool noError = true;
    std::ostringstream oss;

    // Down first
    noError &= left->CheckSemantics();
    noError &= right->CheckSemantics();

    // assign type
    switch (op)
    {
    case Op::Add:
    case Op::Subtract:
    case Op::Multiply:
    case Op::Divide:
    case Op::Power:
    case Op::LessThan:
    case Op::LessEqual:
        type = new Type(Type::TypeName::Int32);
        break;
    case Op::Equal:
        type = new Type(left->type->getType());
        break;
    case Op::And:
        type = new Type(Type::TypeName::Bool);
        break;
    }

    // type check
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
        if (left->type->getType() != Type::TypeName::Int32 || right->type->getType() != Type::TypeName::Int32)
        {
            noError = false;
            oss << "Type mismatch for binary operator : " << opToString() << " . Types are : '" << left->type->getStringTypeName() << "' and '" << right->type->getStringTypeName() << "'";
            printSemanticError(oss.str());
        }
    };
    break;
    case Op::Equal:
        return "=";
        {
            if (left->type != right->type)
            {
                noError = false;
                oss << "Type mismatch for binary operator : " << opToString() << " . Types are : '" << left->type->getStringTypeName() << "' and '" << right->type->getStringTypeName() << "'";
                printSemanticError(oss.str());
            }
        };
        break;
    case Op::And:
        return "and";
        {
            if (left->type->getType() != Type::TypeName::Bool || right->type->getType() != Type::TypeName::Bool)
            {
                noError = false;
                oss << "Type mismatch for binary operator : " << opToString() << " . Types are : '" << left->type->getStringTypeName() << "' and '" << right->type->getStringTypeName() << "'";
                printSemanticError(oss.str());
            }
        };
        break;
    default:
        return noError;
        return noError;
    }

    return noError;
}

/* Assign */
bool Assign::CheckSemantics()
{
    bool noError = true;

    // Down first
    noError &= expr->CheckSemantics();

    // TODO check ID

    type = new Type(expr->type->getType());

    return noError;
}

/* New */
bool New::CheckSemantics()
{
    bool noError = true;

    type = new Type(Type::TypeName::Custom);
    type->SetTypeCustom(typeName);

    return noError;
}

/* ObjectId */
bool ObjectId::CheckSemantics()
{
    bool noError = true;

    // TODO FIND METHOD
    // type = new Type(/*method type*/);

    return noError;
}

/* Call */
bool Call::CheckSemantics()
{
    bool noError = true;

    // TODO FIND METHOD
    // type = new Type(/*method type*/);

    return noError;
}

/* Field */
bool Field::checkSemantics(const ClassSymbolTable &classSymbols)
{
    if (type->getType() == Type::TypeName::Custom && !classSymbols.getClass(type->getStringTypeName()))
    {
        std::ostringstream oss;
        oss << "Semantic error: Unknown type " << type->getStringTypeName()
            << " used in field " << name << " in class " << name << ".";
        printSemanticError(oss.str());
        return false;
    }
    initExpr->CheckSemantics();
    return true;
}
/* Formal */
bool Formal::checkSemantics(const ClassSymbolTable &classSymbols)
{
    bool noError = true;

    if (type->getType() == Type::TypeName::Custom && !classSymbols.hasClass(type->getStringTypeName()))
    {
        std::ostringstream oss;
        oss << "Unknown type '" << type->getStringTypeName() << "' used in formal parameter '" << name << "'.";
        printSemanticError(oss.str());
        noError = false;
    }

    return noError;
}

/* Method */
bool Method::checkMethodTypeDefinitions(const ClassSymbolTable &classSymbols)
{
    if (returnType->getType() == Type::TypeName::Custom && !classSymbols.getClass(returnType->getStringTypeName()))
    {
        std::ostringstream oss;
        oss << "Unknown return type " << returnType->getStringTypeName()
            << " of method " << name << " in class " << name << ".";
        printSemanticError(oss.str());
        return false;
    }
    return true;
}

bool Method::checkFormalParameterRedefinitions()
{
    std::set<std::string> parameterNames;
    bool noError = true;
    for (auto *formal : formals)
    {
        if (!parameterNames.insert(formal->getName()).second)
        {
            std::cerr << "Semantic error: Redefinition of formal parameter '" << formal->getName()
                      << "' in method '" << name << "'.\n";
            noError = false;
        }
    }
    return noError;
}

bool Method::checkFormalSemantics(const ClassSymbolTable &classSymbols)
{
    bool noError = true;
    for (auto *formal : formals)
    {
        noError &= formal->checkSemantics(classSymbols);
    }
    return noError;
}

bool Method::checkSemantics(const ClassSymbolTable &classSymbols)
{
    bool noError = true;
    noError &= checkMethodTypeDefinitions(classSymbols);
    noError &= checkFormalParameterRedefinitions();
    noError &= checkFormalSemantics(classSymbols);
    noError &= body->CheckSemantics();
    return noError;
}

/* Class */

bool Class::areSignaturesEqual(Method *m1, Method *m2)
{
    if (m1->getReturnType()->getStringTypeName() != m2->getReturnType()->getStringTypeName())
    {
        return false;
    }

    const auto &formals1 = m1->getFormals();
    const auto &formals2 = m2->getFormals();
    if (formals1.size() != formals2.size())
    {
        return false;
    }
    for (size_t i = 0; i < formals1.size(); ++i)
    {
        if (formals1[i]->getType()->getStringTypeName() != formals2[i]->getType()->getStringTypeName())
        {
            return false;
        }
    }

    return true;
}

bool Class::checkInheritanceSemantic(const ClassSymbolTable &classSymbols)
{
    std::set<std::string> visited;
    std::string current = name;
    bool noError = true;
    while (current != "Object")
    {
        if (visited.find(current) != visited.end())
        {
            std::ostringstream oss;
            oss << "Inheritance cycle detected involving class " << current << ".";
            printSemanticError(oss.str());
            noError = false;
            break;
        }
        visited.insert(current);

        Class *parentClass = classSymbols.getClass(current);
        if (!parentClass)
        {
            std::ostringstream oss;
            oss << "Class " << current << " extends an undefined class.";
            printSemanticError(oss.str());
            noError = false;
            break;
        }
        current = parentClass->getParent();
    }
    return noError;
}

bool Class::checkFieldRedefinitions()
{
    bool noError = true;
    std::set<std::string> fieldNames;
    for (auto *field : fields)
    {
        if (!fieldNames.insert(field->getName()).second)
        {
            std::cerr << "Semantic error: Redefinition of field '" << field->getName()
                      << "' in class '" << name << "'." << std::endl;
            noError = false;
        }
    }
    return noError;
}

bool Class::checkMethodSignatures(const ClassSymbolTable &classSymbols)
{
    bool noError = true;
    std::map<std::string, Method *> methodSignatures;
    for (auto *method : methods)
    {
        auto result = methodSignatures.insert({method->getName(), method});
        if (!result.second && !areSignaturesEqual(result.first->second, method))
        {
            std::cerr << "Method '" << method->getName()
                      << "' is redefined with a different signature in class '" << name << "'.";
            noError = false;
        }
    }

    // Check against parent class methods
    Class *parentClass = classSymbols.getClass(parent);
    while (parentClass)
    {
        for (auto *parentMethod : parentClass->getMethods())
        {
            auto it = methodSignatures.find(parentMethod->getName());
            if (it != methodSignatures.end() && !areSignaturesEqual(it->second, parentMethod))
            {
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

bool Class::checkClassDefinitionsSemantics(const ClassSymbolTable &classSymbols)
{
    bool noError = true;
    for (auto *field : fields)
    {
        noError &= field->checkSemantics(classSymbols);
    }
    for (auto *method : methods)
    {
        noError &= method->checkSemantics(classSymbols);
    }
    return noError;
}

bool Class::checkSemantics(const ClassSymbolTable &classSymbols)
{
    bool noError = true;
    noError &= checkInheritanceSemantic(classSymbols);
    noError &= checkFieldRedefinitions();
    noError &= checkMethodSignatures(classSymbols);
    noError &= checkClassDefinitionsSemantics(classSymbols);

    return noError;
}

/* Program */
Class *Program::createObjectClass() const
{
    std::vector<Method *> methods = {
        new Method("print", {new Formal("s", new Type(Type::TypeName::String))}, new Type("Object"), new Block({})),
        new Method("printBool", {new Formal("b", new Type(Type::TypeName::Bool))}, new Type("Object"), new Block({})),
        new Method("printInt32", {new Formal("i", new Type(Type::TypeName::Int32))}, new Type("Object"), new Block({})),
        new Method("inputLine", {}, new Type(Type::TypeName::String), new Block({})),
        new Method("inputBool", {}, new Type(Type::TypeName::Bool), new Block({})),
        new Method("inputInt32", {}, new Type(Type::TypeName::Int32), new Block({}))
        };
    return new Class("Object", {}, methods, "");
}

bool Program::checkSemantics()
{
    bool noError = true;
    ClassSymbolTable classSymbols;
    Class *objectClass = createObjectClass();
    classSymbols.addClass(objectClass->getName(), objectClass);

    // Add all classes to the symbol table, report redefinitions
    for (auto *cls : classes)
    {
        if (!classSymbols.addClass(cls->getName(), cls))
        {
            std::ostringstream oss;
            oss << cls->getName() << " is redefined.";
            printSemanticError(oss.str());
            noError = false;
        }
    }

    for (auto *cls : classes)
    {
        noError &= cls->checkSemantics(classSymbols);
    }

    return noError;
}