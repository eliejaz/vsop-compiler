#include "ASTClasses.hpp"

/* Other */
void ASTNode::printScope(ProgramScope *parentScope)
{
    ProgramScope *currentScope = parentScope;
    int level = 0;
    while (currentScope != nullptr)
    {
        std::cout << currentScope->scopeLevelName <<std::endl;
        for (const auto &pair : currentScope->symbolToTypeMap)
        {
            std::cout << "  " << pair.first << " : " << pair.second->getStringTypeName() << std::endl;
        }
        currentScope = currentScope->parentScope;
        level++;
    }
}

/* Type */
bool Type::isSubtypeOf(const Type *base, ClassSymbolTable *classSymbols) const
{
    if (customTypeName == base->getStringTypeName())
    {
        return true;
    }
    Class *thisClass = classSymbols->getClass(customTypeName);

    while (thisClass)
    {
        if (thisClass->getName() == base->getStringTypeName())
        {
            return true;
        }
        thisClass = classSymbols->getClass(thisClass->getParent());
    }
    return false;
}

bool Type::isCompatibleWith(const Type *other, ClassSymbolTable *classSymbols) const
{
    // if (typeName == TypeName::Unit || other->typeName == TypeName::Unit)
    // {
    //     // Everything is compatible with Unit type
    //     return true;
    // }
    if (typeName == other->typeName && typeName != TypeName::Custom)
    {
        // Primitive types match exactly
        return true;
    }
    if (typeName == TypeName::Custom && other->typeName == TypeName::Custom)
    {
        return isSubtypeOf(other, classSymbols);
    }

    return false;
}

/* Block */
bool Block::checkSemantics(ClassSymbolTable *classSymbols, ProgramScope *parentScope)
{
    ProgramScope *blockScope = new ProgramScope(parentScope, "block");
    blockScope->scopeNode = this;
    bool noError = true;
    for (auto *exp : expressions)
    {
        noError &= exp->checkSemantics(classSymbols, blockScope);
    }

    // type of the last expression
    type = expressions.back()->type;

    scope = blockScope;

    return noError;
}

/* If */
bool If::checkSemantics(ClassSymbolTable *classSymbols, ProgramScope *parentScope)
{
    bool noError = true;
    std::ostringstream oss;

    noError &= condition->checkSemantics(classSymbols, parentScope);
    noError &= thenBranch->checkSemantics(classSymbols, parentScope);
    if (elseBranch)
        noError &= elseBranch->checkSemantics(classSymbols, parentScope);

    if (condition->type->getType() != Type::TypeName::Bool)
    {
        noError = false;

        oss << "Type mismatch for the condiction in 'If' type is : '" << condition->type->getStringTypeName() << "', expected : 'bool'";
        printSemanticError(oss.str());
    }

    // assign type
    if (!elseBranch || thenBranch->type->getType() == Type::TypeName::Unit || elseBranch->type->getType() == Type::TypeName::Unit)
    {
        type = new Type(Type::TypeName::Unit);
    }
    else if (thenBranch->type->isCompatibleWith(elseBranch->type, classSymbols))
    {
        type = elseBranch->type;
    }
    else if (elseBranch->type->isCompatibleWith(thenBranch->type, classSymbols))
    {
        type = thenBranch->type;
    }
    else
    {
        noError = false;

        oss << "Type mismatch for in 'If', both branch don't agree, type are : '" << thenBranch->type->getStringTypeName() << "' and '" << elseBranch->type->getStringTypeName() << "'";
        printSemanticError(oss.str());
    }

    scope = parentScope;

    return noError;
}

/* While */
bool While::checkSemantics(ClassSymbolTable *classSymbols, ProgramScope *parentScope)
{
    bool noError = true;
    std::ostringstream oss;
    // Down first
    noError &= condition->checkSemantics(classSymbols, parentScope);
    noError &= body->checkSemantics(classSymbols, parentScope);

    type = new Type(Type::TypeName::Unit);

    // type check
    if (condition->type->getType() != Type::TypeName::Bool)
    {
        noError = false;
        oss << "Type mismatch for condiction in 'While' type is : '" << condition->type->getStringTypeName() << "', expected : 'bool'";
        printSemanticError(oss.str());
    }
    scope = parentScope;

    return noError;
}

/*  Let */
bool Let::checkSemantics(ClassSymbolTable *classSymbols, ProgramScope *parentScope)
{
    bool noError = true;
    ProgramScope *letScope = new ProgramScope(parentScope, "let");
    letScope->scopeNode = this;
    letScope->addSymbol(name, letType);

    if (letType->getType() == Type::TypeName::Custom && !classSymbols->hasClass(letType->getStringTypeName()))
    {
        std::ostringstream oss;
        oss << "Unknown type '" << letType->getStringTypeName() << "' used in Let  '" << name << "'.";
        printSemanticError(oss.str());
        noError = false;
    }
    if (initExpr)
    {
        noError &= initExpr->checkSemantics(classSymbols, letScope);
        if (initExpr && initExpr->type && !initExpr->type->isCompatibleWith(letType, classSymbols))
        {
            noError = false;
            std::ostringstream oss;
            oss << "Type mismatch in 'Let' type is : '" << initExpr->type->getStringTypeName() << "', expected : " << letType->getStringTypeName();
            printSemanticError(oss.str());
        }
    }
    noError &= scopeExpr->checkSemantics(classSymbols, letScope);

    type = scopeExpr->type;

    scope = letScope;

    return noError;
}

/* UnaryOp */
bool UnaryOp::checkSemantics(ClassSymbolTable *classSymbols, ProgramScope *parentScope)
{
    bool noError = true;
    std::ostringstream oss;
    // Down first
    noError &= expr->checkSemantics(classSymbols, parentScope);

    // assign type
    switch (op)
    {
    case Op::Negate:
        type = new Type(Type::TypeName::Int32);
        break;
    case Op::Not:
        type = new Type(Type::TypeName::Bool);
        break;
    case Op::IsNull:
        type = new Type(Type::TypeName::Bool);
        break;
    }

    if (!expr->type)
    {
        return false;
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
    case Op::IsNull:
    {
        if (expr->type->getType() != Type::TypeName::Custom)
        {
            noError = false;
            oss << "Type mismatch for Unary operator : " << opToString() << " . Type is : '" << expr->type->getStringTypeName() << " expected : 'Object'";
            printSemanticError(oss.str());
        }
    };
    break;
    default:
        return noError;
    }

    scope = parentScope;

    return noError;
}

/* BinaryOp */
bool BinaryOp::checkSemantics(ClassSymbolTable *classSymbols, ProgramScope *parentScope)
{
    bool noError = true;
    std::ostringstream oss;

    // Down first
    noError &= left->checkSemantics(classSymbols, parentScope);
    noError &= right->checkSemantics(classSymbols, parentScope);
    scope = parentScope;
    if (!left->type || !right->type)
    {
        return false;
    }
    // assign type
    switch (op)
    {
    case Op::Add:
    case Op::Subtract:
    case Op::Multiply:
    case Op::Divide:
    case Op::Power:
        type = new Type(Type::TypeName::Int32);
        break;
    case Op::Equal:
    case Op::LessThan:
    case Op::LessEqual:
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
    {
        if (!left->type->isCompatibleWith(right->type, classSymbols))
        {
            noError = false;
            oss << "Type mismatch for binary operator : " << opToString() << " . Types are : '" << left->type->getStringTypeName() << "' and '" << right->type->getStringTypeName() << "'";
            printSemanticError(oss.str());
        }
    };
    break;
    case Op::And:
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
bool Assign::checkSemantics(ClassSymbolTable *classSymbols, ProgramScope *parentScope)
{
    bool noError = true;
    noError &= expr->checkSemantics(classSymbols, parentScope);
    Type* parentType = parentScope->lookup(name);
    if (!parentType)
    {
        std::ostringstream oss;
        oss << "use of unbound variable " << name << ".";
        printSemanticError(oss.str());
        noError = false;
    }
    type = expr->type;

    ProgramScope* currentScope = parentScope;
        while (currentScope != nullptr) {
            auto it = currentScope->symbolToTypeMap.find(name);
            if (it != currentScope->symbolToTypeMap.end()) {
                if(!type->isCompatibleWith(it->second, classSymbols)){
                    std::ostringstream oss;
                    oss << "This literal hast type: " << type->getStringTypeName() << " expected type: " << it->second->getStringTypeName()  << ".";
                    printSemanticError(oss.str());
                    noError = false;
                }
                if(type->typeName == Type::TypeName::Custom){
                    it->second->customTypeName = type->customTypeName;
                    it->second->typeClass = type->typeClass;
                }
                type = it->second;
                break;
            }
            currentScope = currentScope->parentScope;
        }

    scope = parentScope;

    return noError;
}

/* New */
bool New::checkSemantics(ClassSymbolTable *classSymbols, ProgramScope *parentScope)
{
    bool noError = true;

    if (!classSymbols->hasClass(typeName))
    {
        std::ostringstream oss;
        oss << "Instantiating unkown class name " << typeName << ".";
        printSemanticError(oss.str());
        noError = false;
    }
    
    type = new Type(Type::TypeName::Custom);
    type->SetTypeCustom(typeName);
    Class* ct = classSymbols->getClass(type->getStringTypeName());

    type->typeClass = ct;
    scope = parentScope;

    return noError;
}

/* ObjectId */
bool ObjectId::checkSemantics(__attribute__((unused)) ClassSymbolTable *classSymbols, ProgramScope *parentScope)
{
    bool noError = true;
    type = parentScope->lookup(id);
    if (!type)
    {
        noError = false;
        std::ostringstream oss;
        oss << "Use of unbounded variable: " << id << ".";
        printSemanticError(oss.str());
    }
    if (type && parentScope->lookupLevelName(id) == "class" && parentScope->inFieldInitializer)
    {
        noError = false;
        std::ostringstream oss;
        oss << "cannot use class fields in field initializers: " << id << ".";
        printSemanticError(oss.str());
    }
    if (id == "self" && parentScope->inFieldInitializer)
    {
        std::ostringstream oss;
        oss << "Canot use self.Call() in field initialising";
        printSemanticError(oss.str());
        noError = false;
    }

    scope = parentScope;
    return noError;
}

/* Call */
bool Call::checkSemantics(ClassSymbolTable *classSymbols, ProgramScope *parentScope)
{
    bool noError = true;
    std::ostringstream oss;

    noError &= caller->checkSemantics(classSymbols, parentScope);

    Type *callerType = caller->type;

    if (!callerType)
    {
        oss << "Type of caller is undefined.";
        printSemanticError(oss.str());
        return false;
    }

    // Get the class of the caller if the caller is of a custom type
    Class *callerClass = nullptr;
    if (callerType->getType() == Type::TypeName::Custom)
    {
        callerClass = classSymbols->getClass(callerType->getStringTypeName());
        if (!callerClass)
        {
            oss << "Class " << callerType->getStringTypeName() << " not found.";
            printSemanticError(oss.str());
            return false;
        }
    }

    // Check if the method exists within the caller class or parent classes
    Method *method = callerClass ? callerClass->getMethod(methodName, classSymbols) : nullptr;
    if (!method)
    {
        oss << "Method " << methodName << " not found in class " << callerType->getStringTypeName() << ".";
        printSemanticError(oss.str());
        return false;
    }

    // Check the argument types
    const std::vector<Formal *> &formals = method->getFormals();
    if (formals.size() != args.size())
    {
        oss << "Argument count mismatch in call to method " << methodName << ". Expected " << formals.size() << " but got " << args.size() << ".";
        printSemanticError(oss.str());
        return false;
    }

    for (size_t i = 0; i < args.size(); ++i)
    {
        noError &= args[i]->checkSemantics(classSymbols, parentScope);
        if (args[i]->type->getType() != formals[i]->getType()->getType())
        {
            oss << "Type mismatch for argument " << i + 1 << " in call to method " << methodName << ". Expected " << formals[i]->getType()->getStringTypeName() << " but got " << args[i]->type->getStringTypeName() << ".";
            printSemanticError(oss.str());
            noError = false;
        }
    }

    // Set the type of the call expression to the return type of the method
    type = method->getReturnType();

    return noError;
}

/* IntegerLiteral */
bool IntegerLiteral::checkSemantics(__attribute__((unused)) ClassSymbolTable *classSymbols, __attribute__((unused)) ProgramScope *parentScope)
{
    type = new Type(Type::TypeName::Int32);
    return true;
}

/* StringLiteral */
bool StringLiteral::checkSemantics(__attribute__((unused)) ClassSymbolTable *classSymbols, __attribute__((unused)) ProgramScope *parentScope)
{
    type = new Type(Type::TypeName::String);
    return true;
}

/* BooleanLiteral */
bool BooleanLiteral::checkSemantics(__attribute__((unused)) ClassSymbolTable *classSymbols, __attribute__((unused)) ProgramScope *parentScope)
{
    type = new Type(Type::TypeName::Bool);
    return true;
}

/* UnitLiteral */
bool UnitLiteral::checkSemantics(__attribute__((unused)) ClassSymbolTable *classSymbols, __attribute__((unused)) ProgramScope *parentScope)
{
    type = new Type(Type::TypeName::Unit);
    return true;
}
/* Field */
bool Field::checkSemantics(ClassSymbolTable *classSymbols, ProgramScope *parentScope)
{
    bool noError = true;
    if (type->getType() == Type::TypeName::Custom && !classSymbols->getClass(type->getStringTypeName()))
    {
        std::ostringstream oss;
        oss << "Unknown type " << type->getStringTypeName()
            << " used in field " << name << " in class " << name << ".";
        printSemanticError(oss.str());
        noError = false;
    }
    if (initExpr)
    {
        parentScope->inFieldInitializer = true;
        noError &= initExpr->checkSemantics(classSymbols, parentScope);
        parentScope->inFieldInitializer = false;

        if (initExpr->type && !initExpr->type->isCompatibleWith(type, classSymbols))
        {
            noError = false;
            std::ostringstream oss;
            oss << "Type mismatch in Field : '" << name << "' return type is : '" << initExpr->type->getStringTypeName() << "', expected : " << type->getStringTypeName();
            printSemanticError(oss.str());
        }
    }

    scope = parentScope;
    return noError;
}
/* Formal */
bool Formal::checkSemantics(ClassSymbolTable *classSymbols, ProgramScope *parentScope)
{
    bool noError = true;

    if (type->getType() == Type::TypeName::Custom && !classSymbols->hasClass(type->getStringTypeName()))
    {
        std::ostringstream oss;
        oss << "Unknown type '" << type->getStringTypeName() << "' used in formal parameter '" << name << "'.";
        printSemanticError(oss.str());
        noError = false;
    }

    scope = parentScope;

    return noError;
}

/* Method */
bool Method::checkMethodTypeDefinitions(ClassSymbolTable *classSymbols)
{
    bool noError = true;
    if (returnType->getType() == Type::TypeName::Custom && !classSymbols->getClass(returnType->getStringTypeName()))
    {
        std::ostringstream oss;
        oss << "Unknown return type " << returnType->getStringTypeName()
            << " of method " << name << " in class " << name << ".";
        printSemanticError(oss.str());
        noError = false;
    }

    return noError;
}

bool Method::checkFormalSemantics(ClassSymbolTable *classSymbols, ProgramScope *parentScope)
{
    bool noError = true;
    for (auto *formal : formals)
    {
        if (!parentScope->addSymbol(formal->getName(), formal->getType()))
        {
            std::ostringstream oss;
            oss << "Redefinition of formal parameter '" << formal->getName()
                << "' in method '" << name << "' .";
            printSemanticError(oss.str());
            noError = false;
        }
        if(formal->getType()->typeName == Type::TypeName::Custom){
            formal->getType()->typeClass = classSymbols->getClass(formal->getType()->getStringTypeName());
        }
        noError &= formal->checkSemantics(classSymbols, parentScope);
    }
    return noError;
}

bool Method::checkSemantics(ClassSymbolTable *classSymbols, ProgramScope *parentScope)
{
    bool noError = true;
    // Create a new scope for this method
    ProgramScope *methodScope = new ProgramScope(parentScope, "method");
    methodScope->scopeNode = this;

    noError &= checkMethodTypeDefinitions(classSymbols);
    noError &= checkFormalSemantics(classSymbols, methodScope);
    noError &= body->checkSemantics(classSymbols, methodScope);

    if (!body->type->isCompatibleWith(returnType, classSymbols))
    {
        noError = false;
        std::ostringstream oss;
        oss << "Type mismatch in Method : '" << name << "' return type is : '" << body->type->getStringTypeName() << "', expected : " << returnType->getStringTypeName();
        printSemanticError(oss.str());
    }
    scope = methodScope;
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

bool Class::checkInheritanceSemantic(ClassSymbolTable *classSymbols)
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

        Class *parentClass = classSymbols->getClass(current);
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

bool Class::checkMethodSignatures(ClassSymbolTable *classSymbols)
{
    bool noError = true;
    std::map<std::string, Method *> methodSignatures;
    for (auto *method : methods)
    {
        method->caller = this;
        auto result = methodSignatures.insert({method->getName(), method});
        if (!result.second)
        {
            std::ostringstream oss;
            oss << "Method '" << method->getName()
                << "' is redefined in class '" << name << "'.";
            printSemanticError(oss.str());
            noError = false;
        }
    }

    // Check against parent class methods
    Class *prnt = classSymbols->getClass(parent);
    parentClass = prnt;
    while (prnt)
    {
        for (auto *parentMethod : prnt->getMethods())
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
        prnt = classSymbols->getClass(prnt->getParent());
    }
    return noError;
}

bool Class::checkClassFieldsSemantics(ClassSymbolTable *classSymbols, ProgramScope *parentScope)
{
    bool noError = true;
    for (auto *field : fields)
    {
        noError &= field->checkSemantics(classSymbols, parentScope);
    }
    // a second time to add them to scope.
    for (auto *field : fields)
    {
        if (parentScope->lookup(field->getName()))
        {
            std::ostringstream oss;
            oss << "Redefinition of field '" << field->getName() << "' is present in parent clas. ";
            printSemanticError(oss.str());
            noError = false;
        }
        if (field->getType() && !parentScope->addSymbol(field->getName(), field->getType()))
        {
            std::ostringstream oss;
            oss << "Redefinition of field '" << field->getName() << "' in class '" << name << "'.";
            printSemanticError(oss.str());

            noError = false;
        }
    }

    return noError;
}

bool Class::checkSemantics(ClassSymbolTable *classSymbols, ProgramScope *parentScope)
{
    // This class was already parsed
    if (scope)
        return true;

    bool noError = true;
    noError &= checkInheritanceSemantic(classSymbols);
    noError &= checkMethodSignatures(classSymbols);

    scope = new ProgramScope(parentScope, "class");
    scope->scopeNode = this;

    // If the class has a parent, we need to inherit the scope from the parent
    if (parent != "Object")
    {
        Class *parentClass = classSymbols->getClass(parent);
        if (parentClass)
        {
            if (parentClass->scope)
            {
                scope->setParentScope(parentClass->scope);
            }
            else
            {
                noError &= parentClass->checkSemantics(classSymbols, nullptr);
                scope->setParentScope(parentClass->scope);
            }
        }
    }

    noError &= checkClassFieldsSemantics(classSymbols, scope);

    Type *currentType = new Type(Type::TypeName::Custom);
    currentType->SetTypeCustom(name);
    currentType->typeClass = this;
    scope->addSymbol("self", currentType);

    for (auto *method : methods)
    {
        noError &= method->checkSemantics(classSymbols, scope);
    }

    return noError;
}

/* Program */
Class *Program::createObjectClass() const
{
    Type *objectType = new Type("Object");

    std::vector<Method *> methods = {
        new Method("print", {new Formal("s", new Type(Type::TypeName::String))}, objectType, new Block({})),
        new Method("printBool", {new Formal("b", new Type(Type::TypeName::Bool))}, objectType, new Block({})),
        new Method("printInt32", {new Formal("i", new Type(Type::TypeName::Int32))}, objectType, new Block({})),
        new Method("inputLine", {}, new Type(Type::TypeName::String), new Block({})),
        new Method("inputBool", {}, new Type(Type::TypeName::Bool), new Block({})),
        new Method("inputInt32", {}, new Type(Type::TypeName::Int32), new Block({}))};
    Class* objectClass =  new Class("Object", {}, methods, "");

    for (auto *method : objectClass->getMethods())
    {method->caller = objectClass;
    }
    objectType->typeClass = objectClass;
    objectClass->allMethods = methods;
    return objectClass;
}

bool Program::checkMainClass(ClassSymbolTable *classSymbols)
{
    bool noError = true;

    Class *mainClass = classSymbols->getClass("Main");
    if (!mainClass)
    {
        std::ostringstream oss;
        oss << "There is no Main class in the program file.";
        printSemanticError(oss.str());
        return false;

    }
    Method *mainMethod = mainClass->getMethod("main", classSymbols);
    if (!mainMethod)
    {
        std::ostringstream oss;
        oss << "There is no main function in Main class in the program file.";
        printSemanticError(oss.str());
        return false;

    }

    if (mainMethod->getReturnType()->getType() != Type::TypeName::Int32)
    {
        noError = false;
        std::ostringstream oss;
        oss << "The main method return type should be Int32.";
        printSemanticError(oss.str());
    }
    if (mainMethod->getFormals().size() > 0)
    {
        noError = false;
        std::ostringstream oss;
        oss << "The main method should not have arguments";
        printSemanticError(oss.str());
    }
    return noError;
}
bool Program::checkSemantics(ClassSymbolTable *classSymbols, ProgramScope *parentScope)
{
    parentScope = new ProgramScope();
    bool noError = true;
    classSymbols = new ClassSymbolTable();
    Class *objectClass = createObjectClass();
    classSymbols->addClass(objectClass->getName(), objectClass);

    // Add all classes to the symbol table, report redefinitions
    for (auto *cls : classes)
    {
        if (!classSymbols->addClass(cls->getName(), cls))
        {
            std::ostringstream oss;
            oss << cls->getName() << " is redefined.";
            printSemanticError(oss.str());
            noError = false;
        }
    }
    noError &= checkMainClass(classSymbols);

    for (auto *cls : classes)
    {
        noError &= cls->checkSemantics(classSymbols, nullptr);
    }

    scope = parentScope;
    return noError;
}