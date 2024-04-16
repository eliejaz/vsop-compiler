#include "ASTClasses.hpp"

/* Type */
bool Type::isSubtypeOf(const Type* base, ClassSymbolTable* classSymbols) const {
    if (customTypeName == base->getStringTypeName()) {
        return true;
    }
    Class* thisClass = classSymbols->getClass(customTypeName);
    while (thisClass != nullptr) {
        if (thisClass->getName() == base->getStringTypeName()) {
            return true;
        }
        thisClass = classSymbols->getClass(thisClass->getParent());
    }
    return false;
}

bool Type::isCompatibleWith(const Type* other, ClassSymbolTable* classSymbols) const {
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
bool Block::checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope)
{
    ProgramScope* blockScope= new ProgramScope(parentScope);
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
bool If::checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope)
{
    bool noError = true;
    std::ostringstream oss;

    noError &= condition->checkSemantics(classSymbols, parentScope);
    noError &= thenBranch->checkSemantics(classSymbols, parentScope);
    noError &= elseBranch->checkSemantics(classSymbols, parentScope);

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
        type = elseBranch->type;
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

    scope = parentScope;

    return noError;
}

/* While */
bool While::checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope)
{
    bool noError = true;
    std::ostringstream oss;
    // Down first
    noError &= condition->checkSemantics(classSymbols, parentScope);
    noError &= body->checkSemantics(classSymbols, parentScope);

    // assign type
    type = new Type(Type::TypeName::Unit);

    // type check
    if (condition->type->getType() != Type::TypeName::Bool)
    {
        oss << "Type mismatch for condiction in 'While' type is : '" << condition->type->getStringTypeName() << "', expected : 'bool'";
        printSemanticError(oss.str());
    }
    scope = parentScope;

    return noError;
}

/* UnaryOp */
bool Let::checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope)
{
    bool noError = true;
    parentScope->addSymbol(name, letType);

    if (initExpr)
    {
    noError &= initExpr->checkSemantics(classSymbols, parentScope);
        }
    noError &= scopeExpr->checkSemantics(classSymbols, parentScope);

    // type check
    if (letType->isCompatibleWith(initExpr->type, classSymbols) && initExpr != nullptr)
    {
        noError = false;
        std::ostringstream oss;
        oss << "Type mismatch in 'Let' type is : '" << initExpr->type->getStringTypeName() << "', expected : " << letType->getStringTypeName();
        printSemanticError(oss.str());
    }
    type = scopeExpr->type;

    scope = parentScope;

    return noError;
}

/* UnaryOp */
bool UnaryOp::checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope)
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

    scope = parentScope;

    return noError;
}

/* BinaryOp */
bool BinaryOp::checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope)
{
    bool noError = true;
    std::ostringstream oss;

    // Down first
    noError &= left->checkSemantics(classSymbols, parentScope);
    noError &= right->checkSemantics(classSymbols, parentScope);

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
        type = left->type;
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

    scope = parentScope;

    return noError;
}

/* Assign */
bool Assign::checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope)
{
    bool noError = true;
    noError &= expr->checkSemantics(classSymbols, parentScope);
    type = expr->type;
    parentScope->addSymbol(name, type);
    
    scope = parentScope;

    return noError;
}

/* New */
bool New::checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope)
{
    bool noError = true;

    if (!classSymbols->hasClass(typeName))
    {
        std::ostringstream oss;
        oss << "Semantic error: Instantiating unkown class name " << typeName << ".";
        printSemanticError(oss.str());
        return false;
    }
    type = new Type(Type::TypeName::Custom);
    type->SetTypeCustom(typeName);
    scope = parentScope;

    return noError;
}

/* ObjectId */
bool ObjectId::checkSemantics(__attribute__((unused))ClassSymbolTable* classSymbols, ProgramScope* parentScope)
{
    type = parentScope->lookup(id);
    scope = parentScope;

    return true;
}

/* Call */
bool Call::checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope)
{
    bool noError = true;
    std::ostringstream oss;

    // First, check the semantics of the caller expression
    noError &= caller->checkSemantics(classSymbols, parentScope);

    // Determine the type of the caller
    Type* callerType = caller->type;
    if (!callerType) {
        oss << "Semantic error: Type of caller is undefined.";
        printSemanticError(oss.str());
        return false;  // Cannot proceed if the type of caller is unknown
    }

    // Get the class of the caller if the caller is of a custom type
    Class* callerClass = nullptr;
    if (callerType->getType() == Type::TypeName::Custom) {
        callerClass = classSymbols->getClass(callerType->getStringTypeName());
        if (!callerClass) {
            oss << "Semantic error: Class " << callerType->getStringTypeName() << " not found.";
            printSemanticError(oss.str());
            return false;
        }
    }

    // Check if the method exists within the caller class or parent classes
    Method* method = callerClass ? callerClass->getMethod(methodName, classSymbols) : nullptr;
    if (!method) {
        oss << "Semantic error: Method " << methodName << " not found in class " << callerType->getStringTypeName() << ".";
        printSemanticError(oss.str());
        return false;
    }

    // Check the argument types
    const std::vector<Formal*>& formals = method->getFormals();
    if (formals.size() != args.size()) {
        oss << "Semantic error: Argument count mismatch in call to method " << methodName << ". Expected " << formals.size() << " but got " << args.size() << ".";
        printSemanticError(oss.str());
        return false;
    }

    for (size_t i = 0; i < args.size(); ++i) {
        noError &= args[i]->checkSemantics(classSymbols, parentScope);
        if (args[i]->type->getType() != formals[i]->getType()->getType()) {
            oss << "Semantic error: Type mismatch for argument " << i + 1 << " in call to method " << methodName << ". Expected " << formals[i]->getType()->getStringTypeName() << " but got " << args[i]->type->getStringTypeName() << ".";
            printSemanticError(oss.str());
            noError = false;
        }
    }

    // Set the type of the call expression to the return type of the method
    type = method->getReturnType();

    return noError;
}


/* Field */
bool Field::checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope)
{
    if (type->getType() == Type::TypeName::Custom && !classSymbols->getClass(type->getStringTypeName()))
    {
        std::ostringstream oss;
        oss << "Semantic error: Unknown type " << type->getStringTypeName()
            << " used in field " << name << " in class " << name << ".";
        printSemanticError(oss.str());
        return false;
    }
    if(initExpr)
        initExpr->checkSemantics(classSymbols, parentScope);
    scope = parentScope;
    return true;
}
/* Formal */
bool Formal::checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope)
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
bool Method::checkMethodTypeDefinitions(ClassSymbolTable* classSymbols)
{
    if (returnType->getType() == Type::TypeName::Custom && !classSymbols->getClass(returnType->getStringTypeName()))
    {
        std::ostringstream oss;
        oss << "Unknown return type " << returnType->getStringTypeName()
            << " of method " << name << " in class " << name << ".";
        printSemanticError(oss.str());
        return false;
    }
    return true;
}


bool Method::checkFormalSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope)
{
    bool noError = true;
    for (auto *formal : formals)
    {
        if (!parentScope->addSymbol(formal->getName(), formal->getType())) {
            std::cerr << "Semantic error: Redefinition of formal parameter '" << formal->getName()
                      << "' in method '" << name << "' .\n";
            noError = false;
        }
        noError &= formal->checkSemantics(classSymbols, parentScope);
    }
    return noError;
}

bool Method::checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope)
{
    bool noError = true;
    // Create a new scope for this method
    ProgramScope* methodScope= new ProgramScope(parentScope);

    noError &= checkMethodTypeDefinitions(classSymbols);
    noError &= checkFormalSemantics(classSymbols, methodScope);
    noError &= body->checkSemantics(classSymbols, methodScope);
    scope = parentScope;
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

bool Class::checkInheritanceSemantic(ClassSymbolTable* classSymbols)
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


bool Class::checkMethodSignatures(ClassSymbolTable* classSymbols)
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
    Class *parentClass = classSymbols->getClass(parent);
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
        parentClass = classSymbols->getClass(parentClass->getParent());
    }
    return noError;
}

bool Class::checkClassDefinitionsSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope)
{
    bool noError = true;
    for (auto *field : fields)
    {
        if (!parentScope->addSymbol(field->getName(), field->getType())) {
            std::cerr << "Semantic error: Redefinition of field '" << field->getName() << "' in class '" << name << "'." << std::endl;
            noError = false;
        }
        noError &= field->checkSemantics(classSymbols, parentScope);
    }
    for (auto *method : methods)
    {
        noError &= method->checkSemantics(classSymbols, parentScope);
    }
    return noError;
}

bool Class::checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope)
{
    bool noError = true;
    noError &= checkInheritanceSemantic(classSymbols);
    noError &= checkMethodSignatures(classSymbols);

    scope = new ProgramScope(parentScope);

    // If the class has a parent, we need to inherit the scope from the parent
    if (!parent.empty() && parent != "Object") {
        Class* parentClass = classSymbols->getClass(parent);
        if (parentClass) {
            ProgramScope* parentClassScope = new ProgramScope(parentScope);
            noError &= parentClass->checkSemantics(classSymbols, parentClassScope);
            scope->setParentScope(parentClassScope);
        } 
    }

    Type* currentType = new Type(Type::TypeName::Custom);
    currentType->SetTypeCustom(name);
    scope->addSymbol("self", currentType);

    noError &= checkClassDefinitionsSemantics(classSymbols, scope);

    return noError;
}

/* Program */
Class *Program::createObjectClass() const
{
    Type* objectType = new Type("Object");
    std::vector<Method *> methods = {
        new Method("print", {new Formal("s", new Type(Type::TypeName::String))}, objectType, new Block({})),
        new Method("printBool", {new Formal("b", new Type(Type::TypeName::Bool))}, objectType, new Block({})),
        new Method("printInt32", {new Formal("i", new Type(Type::TypeName::Int32))}, objectType, new Block({})),
        new Method("inputLine", {}, new Type(Type::TypeName::String), new Block({})),
        new Method("inputBool", {}, new Type(Type::TypeName::Bool), new Block({})),
        new Method("inputInt32", {}, new Type(Type::TypeName::Int32), new Block({}))
        };
    return new Class("Object", {}, methods, "");
}

bool Program::checkSemantics(ClassSymbolTable* classSymbols, ProgramScope* parentScope)
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

    for (auto *cls : classes)
    {
        ProgramScope* classScope = new ProgramScope(parentScope);
        noError &= cls->checkSemantics(classSymbols, classScope);
    }

    scope = parentScope;
    return noError;
}