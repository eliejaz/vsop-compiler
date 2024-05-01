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
llvm::Value* Method::codegen(CodeGenerator& generator, std::string className) {
    llvm::StructType* classType = llvm::StructType::getTypeByName(generator.context, className);
    std::vector<llvm::Type*> paramTypes;
    paramTypes.push_back(classType->getPointerTo());
    for (auto& formal : formals) {
        llvm::Type* type = formal->getType()->typeToLLVM(generator);
        paramTypes.push_back(type);
    }
    llvm::FunctionType* funcType = llvm::FunctionType::get(returnType->typeToLLVM(generator), paramTypes, false);

    llvm::Function* function = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, className + "_" + name, generator.module);

    llvm::BasicBlock* entry = llvm::BasicBlock::Create(generator.context, "entry", function);
    generator.builder.SetInsertPoint(entry);

    // if (body) {
    //     body->codegen(generator);
    // llvm::Value* returnValue = body->codegen(generator, function, generator.builder);
    // generator.builder.CreateRet(returnValue);
    // }

    generator.builder.CreateRetVoid();


    return function;
}

/* CLass */
void Class::codegen(CodeGenerator& generator) {
    std::vector<llvm::Type*> classFieldTypes;

    // If the class has a parent, inherit fields and vtable pointer from the parent
    if (parent != "Object") {
        llvm::StructType* parentType = llvm::StructType::getTypeByName(generator.context, parent);
        classFieldTypes.push_back(parentType->getPointerTo());
    }

    std::string vtableName = "_Vtable" + name;
    llvm::StructType* vtableType = llvm::StructType::create(generator.context, vtableName);
    classFieldTypes.push_back(vtableType->getPointerTo());

    // Add types for each field in the current class
    for (auto& field : fields) {
        llvm::Type* fieldType = field->getType()->typeToLLVM(generator);
        classFieldTypes.push_back(fieldType);
    }

    // Define the class type with fields
    llvm::StructType* classType = llvm::StructType::create(generator.context, name);
    classType->setBody(classFieldTypes);

    std::vector<llvm::Constant*> vtableMethods;
    std::vector<llvm::Type *> methodTypes;

    for (auto& method : methods) {
       llvm::Function *methodFunc = static_cast<llvm::Function*>(method->codegen(generator, name));
       //generator.module->getFunction(llvm::StringRef(name+ "_" + method->getName()));

        vtableMethods.push_back(methodFunc);
        methodTypes.push_back(methodFunc->getType());
    }
    vtableType->setBody(methodTypes);
    generator.module->getOrInsertGlobal(vtableName, vtableType);
    llvm::GlobalVariable *vTable = generator.module->getNamedGlobal(vtableName);
    vTable->setInitializer(llvm::ConstantStruct::get(vtableType, vtableMethods));

    llvm::Function* classNewFunction = createClassNewFunction(generator, classType, vTable, name);

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

    // Initialize fields, if any specific initialization is required

    generator.builder.CreateRet(instance);

    return func;
}

/* Program */
void Program::codegen()
{
    CodeGenerator generator;

    for (auto &klass : classes)
    {
        klass->codegen(generator);
    }
    generator.printLLVMCode();
    std::string llvmOutput = pos.fileName.replace(pos.fileName.find(".txt"), sizeof(".txt") - 1, ".ll");
    generator.printLLVMCodeToFile(llvmOutput);

}