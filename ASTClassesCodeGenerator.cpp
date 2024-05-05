#include "ASTClasses.hpp"

llvm::Value* If::codegen(CodeGenerator& generator) {
    llvm::Function* function = generator.builder.GetInsertBlock()->getParent();
    llvm::BasicBlock* thenBlock = llvm::BasicBlock::Create(generator.context, "then", function);
    llvm::BasicBlock* elseBlock = llvm::BasicBlock::Create(generator.context, "else");
    llvm::BasicBlock* mergeBlock = llvm::BasicBlock::Create(generator.context, "ifcont");

    llvm::Value* condValue = condition->codegen(generator);
    generator.builder.CreateCondBr(condValue, thenBlock, elseBlock);

    // Then block
    generator.builder.SetInsertPoint(thenBlock);
    llvm::Value* thenValue = thenBranch->codegen(generator);
    generator.builder.CreateBr(mergeBlock);
    thenBlock = generator.builder.GetInsertBlock();

    // Else block
    function->getBasicBlockList().push_back(elseBlock);
    generator.builder.SetInsertPoint(elseBlock);
    llvm::Value* elseValue = elseBranch ? elseBranch->codegen(generator) : nullptr;
    generator.builder.CreateBr(mergeBlock);
    elseBlock = generator.builder.GetInsertBlock();

    // Merge block
    function->getBasicBlockList().push_back(mergeBlock);
    generator.builder.SetInsertPoint(mergeBlock);
    llvm::PHINode* phiNode = generator.builder.CreatePHI(llvm::Type::getInt32Ty(generator.context), 2, "iftmp");

    phiNode->addIncoming(thenValue, thenBlock);
    if (elseValue) {
        phiNode->addIncoming(elseValue, elseBlock);
    }

    type->llvmValue = phiNode;

    return phiNode;
}

llvm::Value* While::codegen(CodeGenerator& generator) {
    llvm::Function* function = generator.builder.GetInsertBlock()->getParent();
    llvm::BasicBlock* loopBlock = llvm::BasicBlock::Create(generator.context, "loop", function);
    llvm::BasicBlock* afterBlock = llvm::BasicBlock::Create(generator.context, "afterloop", function);

    llvm::Value* condValue = condition->codegen(generator);
    generator.builder.CreateCondBr(condValue, loopBlock, afterBlock);

    generator.builder.SetInsertPoint(loopBlock);
    body->codegen(generator);
    condValue = condition->codegen(generator);
    generator.builder.CreateCondBr(condValue, loopBlock, afterBlock);

    generator.builder.SetInsertPoint(afterBlock);
    type->llvmValue = llvm::Constant::getNullValue(llvm::Type::getVoidTy(generator.context));
    return type->llvmValue ;
}

llvm::Value* Let::codegen(CodeGenerator& generator) {
    llvm::AllocaInst* alloca = generator.builder.CreateAlloca(letType->typeToLLVM(generator), nullptr, name);
    if (initExpr) {
        llvm::Value* initVal = initExpr->codegen(generator);
        generator.builder.CreateStore(initVal, alloca);
    }
    scopeExpr->codegen(generator);
    type->llvmValue = alloca;
    return alloca;
}
llvm::Value* UnaryOp::codegen(CodeGenerator& generator) {
    llvm::Value* operand = expr->codegen(generator);
    
    switch (op) {
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

llvm::Value* BinaryOp::codegen(CodeGenerator& generator) {
    llvm::Value* L = left->codegen(generator);
    llvm::Value* R = right->codegen(generator);
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
            type->llvmValue = generator.builder.CreateAnd(L, R, "andtmp");
            break;
    }
    return type->llvmValue;
}

llvm::Value* Assign::codegen(CodeGenerator& generator) {
    // llvm::Value* var = generator.scope->getVariable(name);
    // llvm::Value* exprVal = expr->codegen(generator);
    // return generator.builder.CreateStore(exprVal, var);
    return nullptr;
}

llvm::Value* New::codegen(CodeGenerator& generator) {
    std::string callerName = type->getStringTypeName();
    llvm::Function* mainNew = generator.module->getFunction(callerName + "___new");
    type->llvmValue = generator.builder.CreateCall(mainNew, {}, callerName + "Instance");
    return type->llvmValue;

}


llvm::Value* Call::codegen(CodeGenerator& generator) {
    llvm::Value* thisObj = caller->codegen(generator);
    Class* callerClass = caller->type->typeClass;

    std::string finalMethodName = ""; 
    int methodIndex = 0;

    for (size_t i = 0; i < callerClass->allMethods.size(); i++) {

        if (callerClass->allMethods[i]->getName() == methodName) {
            methodIndex = i;
            finalMethodName = callerClass->allMethods[i]->caller->getName() + "__" + methodName;
            break;
        }
    }

    llvm::Value* vTablePtr = generator.builder.CreateStructGEP(thisObj->getType()->getPointerElementType(), thisObj, 0);

    llvm::Value* vTableValue = generator.builder.CreateLoad(vTablePtr->getType()->getPointerElementType(),vTablePtr, "vTableValue");
    
    llvm::Value* funcPtrVal = generator.builder.CreateStructGEP(vTableValue->getType()->getPointerElementType(), vTableValue, methodIndex, "methodValue");


    llvm::Value* calleeMethod = generator.builder.CreateLoad(funcPtrVal->getType()->getPointerElementType(), funcPtrVal, "calleeMethod");

    llvm::Function* calleeFunction = generator.module->getFunction(llvm::StringRef(finalMethodName));
    llvm::FunctionType* calleeFuncType = calleeFunction->getFunctionType();

    llvm::Value* thisArg = generator.builder.CreateBitCast(thisObj, calleeFuncType->getParamType(0), "thisCast");
    std::vector<llvm::Value*> argVals = {thisArg};

    for (size_t i = 0; i < args.size(); i++) {
        llvm::Value* argVal = args[i]->codegen(generator);
        llvm::Type* paramType = calleeFuncType->getParamType(i + 1);

        llvm::Value* castedArgVal = generator.builder.CreateBitCast(argVal, paramType, "arg" + std::to_string(i));
        argVals.push_back(castedArgVal);
    }

    // Create the call instruction
    llvm::Value* result = generator.builder.CreateCall(calleeFuncType, calleeMethod, argVals, "callResult");
    return result;
}


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
        return llvm::Type::getVoidTy(generator.context);
    case TypeName::Custom:
        return generator.handleCustomType(customTypeName);
    default:
        return llvm::Type::getVoidTy(generator.context);
    }
}

llvm::Value* ObjectId::codegen(CodeGenerator& generator){
    Type* objectIdType = scope->lookup(id);
    type->llvmValue = objectIdType->llvmValue;
    return objectIdType->llvmValue;
}
/* Block */
llvm::Value* Block::codegen(CodeGenerator& generator)
{
    llvm::Value* finalValue;
    for(auto& expr : expressions){
        finalValue = expr->codegen(generator);
    }
    return finalValue;
}

/* IntegerLiteral */
llvm::Value *IntegerLiteral::codegen(CodeGenerator &generator)
{
    return llvm::ConstantInt::get(generator.context, llvm::APInt(32, value, true));
}

/* StringLiteral */
llvm::Value *StringLiteral::codegen(CodeGenerator &generator)
{
    return generator.builder.CreateGlobalStringPtr(value, "globalString");
}

/* BooleanLiteral */
llvm::Value *BooleanLiteral::codegen(CodeGenerator &generator)
{
    return llvm::ConstantInt::get(generator.context, llvm::APInt(1, value, true));
}

/* UnitLiteral */
llvm::Value *UnitLiteral::codegen(CodeGenerator &generator)
{
    return llvm::UndefValue::get(llvm::Type::getVoidTy(generator.context));
}

/* Method */
llvm::Value* Method::createFunctionType(CodeGenerator& generator){
    llvm::StructType* classType = llvm::StructType::getTypeByName(generator.context, caller->getName());
    std::vector<llvm::Type*> paramTypes;
    paramTypes.push_back(classType->getPointerTo());
    for (auto& formal : formals) {
        llvm::Type* type = formal->getType()->typeToLLVM(generator);
        paramTypes.push_back(type);
    }
    llvm::FunctionType* funcType = llvm::FunctionType::get(returnType->typeToLLVM(generator), paramTypes, false);

    llvm::Function* function = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, caller->getName() + "__" + name, generator.module);

llvm::Value* Method::codegen(CodeGenerator& generator) {

    llvm::Function* function = generator.module->getFunction(llvm::StringRef(caller->getName()+ "__" + name));
    llvm::BasicBlock* entry = llvm::BasicBlock::Create(generator.context, "entry", function);
    generator.builder.SetInsertPoint(entry);

    auto argIter = function->arg_begin();
    argIter->setName("self");
    scope->lookup("self")->llvmValue = &*argIter;

    for (auto& formal : formals) {
        (++argIter)->setName(formal->getName());
        scope->lookup(formal->getName())->llvmValue = &*argIter;
        
    }
    llvm::Value* returnValue = body->codegen(generator);
    generator.builder.CreateRet(returnValue);

    return function;
}

/* CLass */

void Class::collectMethods(std::vector<Method*>& allMethods) {
    if (parentClass) {
        parentClass->collectMethods(allMethods);
    }
    for (auto& method : methods) {
        bool overridden = false;
        for (size_t i = 0; i < allMethods.size(); i++) {
            if (method->getName() == allMethods[i]->getName()) {
                allMethods[i] = method;
                overridden = true;
                break;
            }
        }
        if (!overridden) {
            allMethods.push_back(method);
        }
    }
}

void Class::collectParentFields(std::vector<Field *>& allFields) {
    if (parentClass) {
        parentClass->collectParentFields(allFields);
    }
    // Add own fields
    allFields.insert(allFields.end(), fields.begin(), fields.end());
}

void Class::codegen(CodeGenerator& generator) {

    std::vector<llvm::Type*> classFieldTypes;

    std::vector<Field *> collectAllFields;
    collectParentFields(collectAllFields);
    allFields = collectAllFields;

    std::string vtableName = name + "VTable";
    llvm::StructType* vtableType = llvm::StructType::create(generator.context, vtableName);
    classFieldTypes.push_back(vtableType->getPointerTo());

    // Add types for each field in the current class
    for (auto& field : allFields) {
        llvm::Type* fieldType = field->getType()->typeToLLVM(generator);
        classFieldTypes.push_back(fieldType);
    }

    // Define the class type with fields
    llvm::StructType* classType = llvm::StructType::create(generator.context, name);
    classType->setBody(classFieldTypes);

    for (auto& method : methods) {
       method->createFunctionType(generator);
    }
    std::vector<Method *> collectAllMethods;
    collectMethods(collectAllMethods);
    allMethods = collectAllMethods;

    std::vector<llvm::Constant*> vtableMethods;
    std::vector<llvm::Type *> methodTypes;
    for (auto& method : allMethods) {
        std::string callerName = method->caller->getName();

        llvm::Function* methodFunc = generator.module->getFunction(llvm::StringRef(callerName+ "__" + method->getName()));
        vtableMethods.push_back(methodFunc);
        methodTypes.push_back(methodFunc->getType());
    }


    vtableType->setBody(methodTypes);
    std::string globalVtableName = name + "___vtable";
    generator.module->getOrInsertGlobal(globalVtableName, vtableType);
    llvm::GlobalVariable *vTable = generator.module->getNamedGlobal(globalVtableName);
    vTable->setInitializer(llvm::ConstantStruct::get(vtableType, vtableMethods));

    createClassNewFunction(generator, classType, vTable, name);

    for (auto& method : methods) {
       method->codegen(generator);
    }
}

llvm::Function* Class::createClassNewFunction(CodeGenerator& generator, llvm::StructType* classType, llvm::GlobalVariable* vTable, const std::string& className) {

    llvm::FunctionType* funcType = llvm::FunctionType::get(classType->getPointerTo(), false);
    llvm::Function* func = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, className + "___new", generator.module);

    // Function entry block
    llvm::BasicBlock* block = llvm::BasicBlock::Create(generator.context, "entry", func);
    generator.builder.SetInsertPoint(block);

    // Calculate the size of the object manually https://stackoverflow.com/questions/14608250/how-can-i-find-the-size-of-a-type
    llvm::DataLayout dataLayout(generator.module);
    uint64_t allocSize = dataLayout.getTypeAllocSize(classType);

    // Allocate memory for the object on the heap using malloc
    llvm::Value* objSize = generator.builder.getInt64(allocSize);
    llvm::Value* mallocResult = generator.builder.CreateCall(generator.module->getFunction("malloc"), objSize, "malloc_result");
    llvm::Value* instance = generator.builder.CreateBitCast(mallocResult, classType->getPointerTo(), "instance_cast");

    // Store the vtable pointer
    llvm::Value* vTablePtr = generator.builder.CreateStructGEP(classType, instance, 0, "vtable_ptr");
    generator.builder.CreateStore(vTable, vTablePtr);

    // ToDo Initialize fields

    generator.builder.CreateRet(instance);

    return func;
}



/* Program */
void Program::codegen(CodeGenerator& generator)
{
    for (auto &klass : classes)
    {
        klass->codegen(generator);
    }

    llvm::Function* mainFunc = llvm::Function::Create(
        llvm::FunctionType::get(llvm::Type::getInt32Ty(generator.context), false),
        llvm::Function::ExternalLinkage,
        "main",
        generator.module
    );

    // create main entry point function
    llvm::BasicBlock* entry = llvm::BasicBlock::Create(generator.context, "entry", mainFunc);
    generator.builder.SetInsertPoint(entry);

    llvm::Function* mainNew = generator.module->getFunction("Main___new");
    llvm::Value* mainInstance = generator.builder.CreateCall(mainNew, {}, "mainInstance");

    // Call the main() method 
    llvm::Function* mainMethod = generator.module->getFunction("Main__main");
    generator.builder.CreateCall(mainMethod, {mainInstance});

    // llvm::StructType* objectType = llvm::StructType::getTypeByName(generator.context, "Object");
    // llvm::Value* objectInstance = generator.builder.CreateBitCast(mainInstance, llvm::PointerType::get(objectType, 0), "objectInstanceCast");


    //  llvm::Value* str = generator.builder.CreateGlobalStringPtr("Done printing using object.\n", "globalString");
    //  llvm::Function* objectPrint = generator.module->getFunction("Object__print");
    // generator.builder.CreateCall(objectPrint, {objectInstance, str});

    // llvm::FunctionCallee printfFunc = generator.module->getOrInsertFunction("printf", llvm::FunctionType::get(llvm::Type::getInt32Ty(generator.context), llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(generator.context)), true));
    // generator.builder.CreateCall(printfFunc, {str});

    generator.builder.CreateRet(llvm::ConstantInt::get(generator.context, llvm::APInt(32, 0, true)));
}