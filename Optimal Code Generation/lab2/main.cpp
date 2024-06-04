#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Verifier.h>

#include <llvm/Support/CommandLine.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/FileSystem.h>

using namespace llvm;

//int main()
//{
//    return 353 + 48;
//}

int main() {
    LLVMContext context;
    Module module("opt_lab2", context);

    IRBuilder<> builder(context);

    FunctionType *funcType = FunctionType::get(Type::getInt64Ty(context), false);
    Function* mainFunc = Function::Create(funcType, Function::ExternalLinkage, "main", module);
    BasicBlock* entryBlock = BasicBlock::Create(context, "entry", mainFunc);

    builder.SetInsertPoint(entryBlock);

    ConstantInt* constant_353 = ConstantInt::get(Type::getInt64Ty(context), 353);
    ConstantInt* constant_48 = ConstantInt::get(Type::getInt64Ty(context), 48);
    Value* result = builder.CreateAdd(constant_353, constant_48);

    builder.CreateRet(result);

    module.print(outs(), nullptr);

    return 0;
}