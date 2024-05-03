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
        return llvm::Type::getVoidTy(generator.context);
    case TypeName::Custom:
        return generator.handleCustomType(customTypeName);
    default:
        return llvm::Type::getVoidTy(generator.context);
    }
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
llvm::Value* Method::codegen(CodeGenerator& generator) {
    llvm::StructType* classType = llvm::StructType::getTypeByName(generator.context, caller->getName());
    std::vector<llvm::Type*> paramTypes;
    paramTypes.push_back(classType->getPointerTo());
    for (auto& formal : formals) {
        llvm::Type* type = formal->getType()->typeToLLVM(generator);
        paramTypes.push_back(type);
    }
    llvm::FunctionType* funcType = llvm::FunctionType::get(returnType->typeToLLVM(generator), paramTypes, false);

    llvm::Function* function = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, caller->getName() + "_" + name, generator.module);

    llvm::BasicBlock* entry = llvm::BasicBlock::Create(generator.context, "entry", function);
    generator.builder.SetInsertPoint(entry);

    // if (body) {
    llvm::Value* returnValue = body->codegen(generator);
    generator.builder.CreateRet(returnValue);
    // }

    //generator.builder.CreateRetVoid();


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
    std::vector<Field *> allFields;
    collectParentFields(allFields);

    std::string vtableName = "_Vtable" + name;
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
       method->codegen(generator);
    }

    std::vector<Method*> allMethods;
    collectMethods(allMethods);
    std::vector<llvm::Constant*> vtableMethods;
    std::vector<llvm::Type *> methodTypes;
    for (auto& method : allMethods) {
        std::string callerName = method->caller->getName();

        llvm::Function* methodFunc = generator.module->getFunction(llvm::StringRef(callerName+ "_" + method->getName()));
        vtableMethods.push_back(methodFunc);
        methodTypes.push_back(methodFunc->getType());
    }

    vtableType->setBody(methodTypes);
    generator.module->getOrInsertGlobal(vtableName, vtableType);
    llvm::GlobalVariable *vTable = generator.module->getNamedGlobal(vtableName);
    vTable->setInitializer(llvm::ConstantStruct::get(vtableType, vtableMethods));

    createClassNewFunction(generator, classType, vTable, name);
}

llvm::Function* Class::createClassNewFunction(CodeGenerator& generator, llvm::StructType* classType, llvm::GlobalVariable* vTable, const std::string& className) {

    llvm::FunctionType* funcType = llvm::FunctionType::get(classType->getPointerTo(), false);
    llvm::Function* func = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, className + "_new", generator.module);

    // Function entry block
    llvm::BasicBlock* block = llvm::BasicBlock::Create(generator.context, "entry", func);
    generator.builder.SetInsertPoint(block);

    llvm::Value* instance = generator.builder.CreateAlloca(classType);
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

    llvm::Function* mainNew = generator.module->getFunction("Main_new");
    llvm::Value* mainInstance = generator.builder.CreateCall(mainNew, {}, "mainInstance");

    // Call the main() method 
    llvm::Function* mainMethod = generator.module->getFunction("Main_main");
    generator.builder.CreateCall(mainMethod, {mainInstance});

    llvm::StructType* objectType = llvm::StructType::getTypeByName(generator.context, "Object");
    llvm::Value* objectInstance = generator.builder.CreateBitCast(mainInstance, llvm::PointerType::get(objectType, 0), "objectInstanceCast");


     llvm::Value* str = generator.builder.CreateGlobalStringPtr("Done printing using object.\n", "doneString");
     llvm::Function* objectPrint = generator.module->getFunction("Object_print");
    generator.builder.CreateCall(objectPrint, {objectInstance, str});

    // llvm::FunctionCallee printfFunc = generator.module->getOrInsertFunction("printf", llvm::FunctionType::get(llvm::Type::getInt32Ty(generator.context), llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(generator.context)), true));
    // generator.builder.CreateCall(printfFunc, {str});

    generator.builder.CreateRet(llvm::ConstantInt::get(generator.context, llvm::APInt(32, 0, true)));
}