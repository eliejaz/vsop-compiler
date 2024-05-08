#include "ASTClasses.hpp"

/* Type */
llvm::Type *Type::typeToLLVM(CodeGenerator &generator)
{
    switch (typeName)
    {
    case TypeName::Int32:
        return llvm::Type::getInt32Ty(generator.context);
    case TypeName::Bool:
        return llvm::Type::getInt1Ty(generator.context);
    case TypeName::String:
        return llvm::Type::getInt8PtrTy(generator.context);
    case TypeName::Unit:
        return llvm::Type::getInt1Ty(generator.context);
    case TypeName::Custom:
        return generator.handleCustomType(customTypeName)->getPointerTo();
    default:
        return llvm::Type::getVoidTy(generator.context);
    }
}

llvm::Value *Type::getDefaultValue(CodeGenerator &generator)
{
    switch (typeName)
    {
    case TypeName::Int32:
        return llvm::ConstantInt::get(generator.context, llvm::APInt(32, 0));
    case TypeName::Bool:
        return llvm::ConstantInt::get(generator.context, llvm::APInt(1, 0));
    case TypeName::String:
        {
            // Create a constant empty string ""
            llvm::Constant* strConstant = llvm::ConstantDataArray::getString(generator.context, "", true);
            llvm::GlobalVariable* globalStr = new llvm::GlobalVariable(
                *generator.module, 
                strConstant->getType(), 
                true, 
                llvm::GlobalValue::PrivateLinkage, 
                strConstant,
                ".str.empty");

            // Properly get the pointer to the first character of the string
            llvm::ArrayType* arrayType = llvm::ArrayType::get(llvm::Type::getInt8Ty(generator.context), 1);
            llvm::Constant* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(generator.context), 0);
            std::vector<llvm::Constant*> indices = {zero, zero};  // index into the array
            llvm::Constant* gep = llvm::ConstantExpr::getGetElementPtr(arrayType, globalStr, indices);

            return gep;
        }
    case TypeName::Unit:
        return llvm::ConstantInt::get(generator.context, llvm::APInt(1, 0));
    case TypeName::Custom:
        return llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(typeToLLVM(generator)));
    default:
        return llvm::UndefValue::get(llvm::Type::getVoidTy(generator.context));
    }
}

/* If */
llvm::Value *If::codegen(CodeGenerator &generator)
{
    llvm::Function *function = generator.builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *thenBlock = llvm::BasicBlock::Create(generator.context, "then", function);
    llvm::BasicBlock *elseBlock = llvm::BasicBlock::Create(generator.context, "else");
    llvm::BasicBlock *mergeBlock = llvm::BasicBlock::Create(generator.context, "ifcont");

    llvm::Value *condValue = condition->codegen(generator);
    generator.builder.CreateCondBr(condValue, thenBlock, elseBlock);

    llvm::Type *ifType = type->typeToLLVM(generator);

    // Then block
    generator.builder.SetInsertPoint(thenBlock);
    llvm::Value *thenValue = thenBranch->codegen(generator);
    if(type->typeName == Type::TypeName::Custom)
        thenValue = generator.builder.CreateBitCast(thenValue, ifType, "thenCast");
    thenBlock = generator.builder.GetInsertBlock();
    generator.builder.CreateBr(mergeBlock);

    // Else block
    function->getBasicBlockList().push_back(elseBlock);
    generator.builder.SetInsertPoint(elseBlock);
    llvm::Value *elseValue = elseBranch ? elseBranch->codegen(generator) : nullptr;
    if (elseValue)
    {   if(type->typeName == Type::TypeName::Custom)
            elseValue = generator.builder.CreateBitCast(elseValue, ifType, "elseCast");
    }
    elseBlock = generator.builder.GetInsertBlock();
    generator.builder.CreateBr(mergeBlock);

    // Merge block
    function->getBasicBlockList().push_back(mergeBlock);
    generator.builder.SetInsertPoint(mergeBlock);

    llvm::PHINode *phiNode = generator.builder.CreatePHI(ifType, 2, "iftmp");
    phiNode->addIncoming(thenValue, thenBlock);

    if (elseValue)
    {
        phiNode->addIncoming(elseValue, elseBlock);
    }

    type->llvmValue = phiNode;

    return phiNode;
}

/* While */
llvm::Value *While::codegen(CodeGenerator &generator)
{

    llvm::Function *function = generator.builder.GetInsertBlock()->getParent();

    // Create the condition value
    llvm::Value *condValue = condition->codegen(generator);
    llvm::BasicBlock *loopBlock = llvm::BasicBlock::Create(generator.context, "loop", function);
    llvm::BasicBlock *afterBlock = llvm::BasicBlock::Create(generator.context, "afterloop");


    // Initial branch decision
    generator.builder.CreateCondBr(condValue, loopBlock, afterBlock);

    // Loop block content

    generator.builder.SetInsertPoint(loopBlock);

    body->codegen(generator);


    // Reevaluate condition at the end of the loop
    condValue = condition->codegen(generator);
    generator.builder.CreateCondBr(condValue, loopBlock, afterBlock);
    // Exit point
    function->getBasicBlockList().push_back(afterBlock);
    generator.builder.SetInsertPoint(afterBlock);

    // While return unit type
    type->llvmValue = llvm::Constant::getNullValue(llvm::Type::getInt32Ty(generator.context));

    return type->llvmValue;
}

/* Let */
llvm::Value *Let::codegen(CodeGenerator &generator)
{
    llvm::Function *parentFunction = generator.builder.GetInsertBlock()->getParent();
    llvm::IRBuilder<> TmpBuilder(&(parentFunction->getEntryBlock()),
                                 parentFunction->getEntryBlock().begin());
    llvm::AllocaInst *alloca = TmpBuilder.CreateAlloca(letType->typeToLLVM(generator), nullptr, llvm::Twine(name));
    letType->llvmValue = alloca;

    llvm::Value *initVal;
    if (initExpr)
    {
        initVal = initExpr->codegen(generator);   
        if(letType->typeName == Type::TypeName::Custom)
            initVal = generator.builder.CreateBitCast(initVal, letType->typeToLLVM(generator), "letCast");
    }else{
        initVal = letType->getDefaultValue(generator);
    }


    generator.builder.CreateStore(initVal, alloca);

    type->llvmValue = scopeExpr->codegen(generator);

    if(type->typeName == Type::TypeName::Custom)
        type->llvmValue = generator.builder.CreateBitCast(type->llvmValue, type->typeToLLVM(generator), "letCast");

    return type->llvmValue ;
}

/* UnaryOp */
llvm::Value *UnaryOp::codegen(CodeGenerator &generator)
{
    llvm::Value *operand = expr->codegen(generator);

    switch (op)
    {
    case Op::Negate:
        type->llvmValue = generator.builder.CreateNeg(operand, "negtmp");
        break;
    case Op::Not:
        type->llvmValue = generator.builder.CreateNot(operand, "nottmp");
        break;
    case Op::IsNull:
        type->llvmValue = generator.builder.CreateIsNull(operand, "isnulltmp");
        break;
    }
    return type->llvmValue;
}

/* BinaryOp */
llvm::Value *BinaryOp::codegen(CodeGenerator &generator) {
    llvm::Value *L = left->codegen(generator);
    llvm::Value *R = nullptr;
    if(op != Op::And)
       R = right->codegen(generator);


    switch (op) {
    case Op::Add:
        type->llvmValue = generator.builder.CreateAdd(L, R, "addtmp");
        break;
    case Op::Subtract:
        type->llvmValue = generator.builder.CreateSub(L, R, "subtmp");
        break;
    case Op::Multiply:
        type->llvmValue = generator.builder.CreateMul(L, R, "multmp");
        break;
    case Op::Divide:
        type->llvmValue = generator.builder.CreateSDiv(L, R, "divtmp");
        break;
    case Op::Equal:
        type->llvmValue = generator.builder.CreateICmpEQ(L, R, "eqtmp");
        break;
    case Op::LessThan:
        type->llvmValue = generator.builder.CreateICmpSLT(L, R, "lttmp");
        break;
    case Op::LessEqual:
        type->llvmValue = generator.builder.CreateICmpSLE(L, R, "letmp");
        break;
    case Op::And:
        {
            llvm::Function *function = generator.builder.GetInsertBlock()->getParent();

            llvm::BasicBlock *afterLBlock = generator.builder.GetInsertBlock();

            llvm::BasicBlock *evalRBlock = llvm::BasicBlock::Create(generator.context, "evalR", function);
            llvm::BasicBlock *mergeBlock = llvm::BasicBlock::Create(generator.context, "mergeAnd", function);

            // Conditional branch based on the result of L
            generator.builder.CreateCondBr(L, evalRBlock, mergeBlock);

            // Evaluate R only if L is true
            generator.builder.SetInsertPoint(evalRBlock);
            llvm::Value *R = right->codegen(generator);
            generator.builder.CreateBr(mergeBlock);

            generator.builder.SetInsertPoint(mergeBlock);
            llvm::PHINode *phiNode = generator.builder.CreatePHI(llvm::Type::getInt1Ty(generator.context), 2, "andResult");
            phiNode->addIncoming(R, evalRBlock);
            phiNode->addIncoming(llvm::ConstantInt::get(generator.context, llvm::APInt(1, 0)), afterLBlock);

            type->llvmValue = phiNode;
            break;
        }
    case Op::Power:
        type->llvmValue = nullptr;
        break;
    }

    return type->llvmValue;
}


/* Assign */
llvm::Value *Assign::codegen(CodeGenerator &generator)
{

    llvm::Value *var = nullptr;
    ProgramScope *currentScope = scope;

    // Get the first set var llvm
    Type *type = currentScope->lookup(name);
    std::string levelName = currentScope->lookupLevelName(name);
    if ( levelName == "class")
    {
        Class *scopeClass = dynamic_cast<Class *>(currentScope->lookupScopeNode(name));

        for (size_t i = 0; i < scopeClass->allFields.size(); i++)
        {

            if (scopeClass->allFields[i]->getName() == name)
            {

                var = generator.builder.CreateStructGEP(
                    scope->lookup("self")->llvmValue->getType()->getPointerElementType(),
                    scope->lookup("self")->llvmValue,
                    i + 1, name + "ptr");

                break;
            }
        }
    } 
    else
    {
        var = codegenPointer(generator, name);
    }




    llvm::Value *exprVal = expr->codegen(generator);

    if(type->typeName == Type::TypeName::Custom)
        exprVal = generator.builder.CreateBitCast(exprVal, type->typeToLLVM(generator), "assignCast");

    generator.builder.CreateStore(exprVal, var);

    type->llvmValue = var;
    return exprVal;
}

llvm::Value *Assign::codegenPointer(CodeGenerator &generator, std::string id){
    Type *objectIdType = scope->lookup(id);
    return objectIdType->llvmValue;
}

/* New */
llvm::Value *New::codegen(CodeGenerator &generator)
{
    std::string callerName = type->getStringTypeName();
    llvm::Function *newFunc = generator.module->getFunction(callerName + "___new");

    type->llvmValue = generator.builder.CreateCall(newFunc, {}, callerName + "Instance");
    return type->llvmValue;
}

/* Call */
llvm::Value *Call::codegen(CodeGenerator &generator)
{

    llvm::Value *thisObj = caller->codegen(generator);

    Class *callerClass =caller->type->typeClass;

    std::string finalMethodName = "";
    int methodIndex = 0;

    for (size_t i = 0; i < callerClass->allMethods.size(); i++)
    {
        if (callerClass->allMethods[i]->getName() == methodName)
        {
            methodIndex = i;
            finalMethodName = callerClass->allMethods[i]->caller->getName() + "__" + methodName;
            break;
        }
    }



    llvm::Value *vTablePtr = generator.builder.CreateStructGEP(thisObj->getType()->getPointerElementType(), thisObj, 0);
    llvm::Value *vTableValue = generator.builder.CreateLoad(vTablePtr->getType()->getPointerElementType(), vTablePtr, "vTableValue");
    llvm::Value *funcPtrVal = generator.builder.CreateStructGEP(vTableValue->getType()->getPointerElementType(), vTableValue, methodIndex, "methodValue");

    llvm::Value *calleeMethod = generator.builder.CreateLoad(funcPtrVal->getType()->getPointerElementType(), funcPtrVal, "calleeMethod");

    llvm::Function *calleeFunction = generator.module->getFunction(llvm::StringRef(finalMethodName));
    llvm::FunctionType *calleeFuncType = calleeFunction->getFunctionType();

    llvm::Value *thisArg = generator.builder.CreateBitCast(thisObj, calleeFuncType->getParamType(0), "thisCast");
    std::vector<llvm::Value *> argVals = {thisArg};

    for (size_t i = 0; i < args.size(); i++)
    {
        llvm::Value *argVal = args[i]->codegen(generator);
        llvm::Type *paramType = calleeFuncType->getParamType(i + 1);
        argVal = generator.builder.CreateBitCast(argVal, paramType, "arg" + std::to_string(i));
        argVals.push_back(argVal);
    }

    // Create the call instruction
    type->llvmValue = generator.builder.CreateCall(calleeFuncType, calleeMethod, argVals, "callResult");
    return type->llvmValue;
}

/* ObjectId */

llvm::Value *ObjectId::codegen(CodeGenerator &generator)
{
    Type *objectIdType = scope->lookup(id);
    std::string lookUpName = scope->lookupLevelName(id);
        if (lookUpName == "class")
    {
        Class *scopeClass = dynamic_cast<Class *>(scope->lookupScopeNode(id));

        for (size_t i = 0; i < scopeClass->allFields.size(); i++)
        {

            if (scopeClass->allFields[i]->getName() == id)
            {

                llvm::Value* fieldPtr = generator.builder.CreateStructGEP(
                    scope->lookup("self")->llvmValue->getType()->getPointerElementType(),
                    scope->lookup("self")->llvmValue,
                    i + 1, id + "ptr");
                type->llvmValue = generator.builder.CreateLoad(fieldPtr->getType()->getPointerElementType(), fieldPtr, "fieldValue");

                break;
            }
        }
    }else if (lookUpName == "let" ||objectIdType->llvmValue->getType()->isPointerTy()){
        return generator.builder.CreateLoad(objectIdType->typeToLLVM(generator), objectIdType->llvmValue);

    }
    else{
        type->llvmValue = objectIdType->llvmValue;

    }
    return objectIdType->llvmValue;
}

/* IntegerLiteral */
llvm::Value *IntegerLiteral::codegen(CodeGenerator &generator)
{
    return llvm::ConstantInt::get(generator.context, llvm::APInt(32, value, true));
}

/* StringLiteral */
llvm::Value *StringLiteral::codegen(CodeGenerator &generator)
{
    std::string processedString = value.substr(1, value.size() - 2);
    std::string resultString;

    for (size_t i = 0; i < processedString.size(); ++i)
    {
        if (processedString[i] == '\\')
        {
            ++i;
            switch (processedString[i])
            {
            case 'x':
            {
                if (i + 2 < processedString.size() && std::isxdigit(processedString[i + 1]) && std::isxdigit(processedString[i + 2]))
                {
                    std::string hexStr = processedString.substr(i + 1, 2);
                    char ch = static_cast<char>(std::stoi(hexStr, nullptr, 16));
                    resultString += ch;
                    i += 2;
                }
                else
                {
                    resultString += "\\x";
                }
                break;
            }
            default:
                resultString += '\\';
                resultString += processedString[i];
            }
        }
        else
        {
            resultString += processedString[i];
        }
    }

    return generator.builder.CreateGlobalStringPtr(resultString, "globalString");
}

/* BooleanLiteral */
llvm::Value *BooleanLiteral::codegen(CodeGenerator &generator)
{
    return llvm::ConstantInt::get(generator.context, llvm::APInt(1, value, true));
}

/* UnitLiteral */
llvm::Value *UnitLiteral::codegen(CodeGenerator &generator)
{
    return llvm::ConstantInt::get(generator.context, llvm::APInt(1, 0));
}

/* Block */
llvm::Value *Block::codegen(CodeGenerator &generator)
{
    llvm::Value *finalValue;
    for (auto &expr : expressions)
    {
        finalValue = expr->codegen(generator);
    }
    return finalValue;
}

/* Method */
llvm::Value *Method::createFunctionType(CodeGenerator &generator, llvm::StructType *classType)
{
    std::vector<llvm::Type *> paramTypes;
    paramTypes.push_back(classType->getPointerTo());
    for (auto &formal : formals)
    {
        llvm::Type *type = formal->getType()->typeToLLVM(generator);
        paramTypes.push_back(type);
    }
    llvm::FunctionType *funcType = llvm::FunctionType::get(returnType->typeToLLVM(generator), paramTypes, false);
    llvm::Value * toRet = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, caller->getName() + "__" + name, generator.module);

    return toRet;
}

llvm::Value *Method::codegen(CodeGenerator &generator)
{

    llvm::Function *function = generator.module->getFunction(llvm::StringRef(caller->getName() + "__" + name));
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(generator.context, "entry", function);
    generator.builder.SetInsertPoint(entry);

    auto argIter = function->arg_begin();
    argIter->setName("self");
    scope->lookup("self")->llvmValue = &*argIter;

    for (auto &formal : formals)
    {
        (++argIter)->setName(formal->getName());
        scope->lookup(formal->getName())->llvmValue = &*argIter;
    }
    llvm::Value *returnValue = body->codegen(generator);
    generator.builder.CreateRet(returnValue);

    return function;
}

/* CLass */

void Class::collectMethods(std::vector<Method *> &allMethods)
{
    if (parentClass)
    {
        parentClass->collectMethods(allMethods);
    }
    for (auto &method : methods)
    {
        bool overridden = false;
        for (size_t i = 0; i < allMethods.size(); i++)
        {
            if (method->getName() == allMethods[i]->getName())
            {
                allMethods[i] = method;
                overridden = true;
                break;
            }
        }
        if (!overridden)
        {
            allMethods.push_back(method);
        }
    }
}

void Class::collectParentFields(std::vector<Field *> &allFields)
{
    if (parentClass)
    {
        parentClass->collectParentFields(allFields);
    }
    // Add own fields
    allFields.insert(allFields.end(), fields.begin(), fields.end());
}

void Class::codegen(CodeGenerator &generator)
{   

    std::vector<llvm::Type *> classFieldTypes;

    std::vector<Field *> collectAllFields;
    collectParentFields(collectAllFields);
    allFields = collectAllFields;

    std::string vtableName = name + "VTable";

    llvm::StructType *vtableType = generator.handleCustomType(vtableName);
    llvm::StructType *classType = generator.handleCustomType(name);
    llvm::FunctionType *funcType = llvm::FunctionType::get(classType->getPointerTo(), false);
    llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, name + "___new", generator.module);

    classFieldTypes.push_back(vtableType->getPointerTo());

    // Add types for each field in the current class
    for (auto &field : allFields)
    {
        llvm::Type *fieldType = field->getType()->typeToLLVM(generator);
        classFieldTypes.push_back(fieldType);
    }

    // Define the class type with fields


    classType->setBody(classFieldTypes);

    for (auto &method : methods)
    {
        method->createFunctionType(generator, classType);
    }

    std::vector<Method *> collectAllMethods;
    collectMethods(collectAllMethods);
    allMethods = collectAllMethods;

    std::vector<llvm::Constant *> vtableMethods;
    std::vector<llvm::Type *> methodTypes;
    for (auto &method : allMethods)
    {
        std::string callerName = method->caller->getName();

        llvm::Function *methodFunc = generator.module->getFunction(llvm::StringRef(callerName + "__" + method->getName()));
        vtableMethods.push_back(methodFunc);
        methodTypes.push_back(methodFunc->getType());
    }

    vtableType->setBody(methodTypes);
    std::string globalVtableName = name + "___vtable";
    generator.module->getOrInsertGlobal(globalVtableName, vtableType);
    llvm::GlobalVariable *vTable = generator.module->getNamedGlobal(globalVtableName);
    vTable->setInitializer(llvm::ConstantStruct::get(vtableType, vtableMethods));

    createClassNewFunction(generator, classType, vTable, name);

    for (auto &method : methods)
    {
        method->codegen(generator);
    }
}

llvm::Function *Class::createClassNewFunction(CodeGenerator &generator, llvm::StructType *classType, llvm::GlobalVariable *vTable, const std::string &className)
{
    llvm::Function *func = generator.module->getFunction(llvm::StringRef(className + "___new"));
    llvm::BasicBlock *entryBlock = llvm::BasicBlock::Create(generator.context, "entry", func);
    generator.builder.SetInsertPoint(entryBlock);

    llvm::DataLayout dataLayout(generator.module);
    uint64_t allocSize = dataLayout.getTypeAllocSize(classType);
    llvm::Value *objSize = llvm::ConstantInt::get(generator.context, llvm::APInt(64, allocSize));

    llvm::Value *mallocResult = generator.builder.CreateCall(generator.module->getFunction("malloc"), objSize, "mallocResult");
    llvm::Value *instance = generator.builder.CreateBitCast(mallocResult, classType->getPointerTo(), "instanceCast");
    llvm::Value *vTablePtr = generator.builder.CreateStructGEP(classType, instance, 0, "vtablePtr");
    generator.builder.CreateStore(vTable, vTablePtr);

    int fieldIndex = 1; // Start after vtable
    for (auto &field : allFields)
    {
        llvm::Value *defaultValue;
        if (field->getInitExpr())
        {
            defaultValue = field->getInitExpr()->codegen(generator);
        }
        else
        {
            defaultValue = field->getType()->getDefaultValue(generator);
        }
        llvm::Value *fieldPtr = generator.builder.CreateStructGEP(classType, instance, fieldIndex, field->getName() + "Ptr");
        generator.builder.CreateStore(defaultValue, fieldPtr);

        field->getType()->llvmValue = instance;
        fieldIndex++;
    }

    generator.builder.CreateRet(instance);
    return func;
}



/* Program */
void Program::codegen(CodeGenerator &generator)
{

    for (auto &klass : classes)
    {
        klass->codegen(generator);
    }

    llvm::Function *mainFunc = llvm::Function::Create(
        llvm::FunctionType::get(llvm::Type::getInt32Ty(generator.context), false),
        llvm::Function::ExternalLinkage,
        "main",
        generator.module);

    // create main entry point function
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(generator.context, "entry", mainFunc);
    generator.builder.SetInsertPoint(entry);

    llvm::Function *mainNew = generator.module->getFunction("Main___new");
    llvm::Value *mainInstance = generator.builder.CreateCall(mainNew, {}, "mainInstance");

    // Call the main() method
    llvm::Function *mainMethod = generator.module->getFunction("Main__main");
    generator.builder.CreateCall(mainMethod, {mainInstance});

    generator.builder.CreateRet(llvm::ConstantInt::get(generator.context, llvm::APInt(32, 0, true)));

    generator.applyOptimizations();
}