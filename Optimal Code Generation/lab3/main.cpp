#include <stdint.h>
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Verifier.h>

#include <llvm/Support/CommandLine.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/FileSystem.h>

using namespace std;
using namespace llvm;

#define TOK_EOF 1
#define TOK_IDENT 2
#define TOK_NUM 3
#define TOK_IF 6
#define TOK_ELSE 7
#define TOK_THEN 8
#define TOK_WHILE 9
#define TOK_DO 10
#define TOK_END 11

static string IdentifierStr;
static int NumVal;
static int CurToken;

static int GetToken() {

    static int posCh = ' ';

    while (isspace(posCh))
        posCh = getchar();
    if (isalpha(posCh)) {
        IdentifierStr = posCh;
        while (isalnum((posCh = getchar())))
            IdentifierStr += posCh;

        if (IdentifierStr == "if")
            return TOK_IF;
        if (IdentifierStr == "else")
            return TOK_ELSE;
        if (IdentifierStr == "then")
            return TOK_THEN;
        if (IdentifierStr == "while")
            return TOK_WHILE;
        if (IdentifierStr == "do")
            return TOK_DO;
        if (IdentifierStr == "end")
            return TOK_END;

        return TOK_IDENT;
    }

    if (isdigit(posCh)) {
        string NumStr;
        do {
            NumStr += posCh;
            posCh = getchar();
        } while (isdigit(posCh));

        NumVal = strtod(NumStr.c_str(), nullptr);
        return TOK_NUM;
    }

    if (posCh == EOF)
        return TOK_EOF;

    int thisChar = posCh;
    posCh = getchar();

    return thisChar;
}

void NextToken(){
    CurToken = GetToken();
}

class ExprAST {
public:
    virtual ~ExprAST() {}
    virtual llvm::Value *codegen() = 0;
};

class NumberExprAST : public ExprAST {
    int Val;
public:
    NumberExprAST(int Val) : Val(Val) {}
    llvm::Value *codegen() override;
};

class AssignAST : public ExprAST {
    string Name;
    ExprAST *assExpr;
public:
    AssignAST(const string &Name, ExprAST *Expr) {
        this->Name = Name;
        this->assExpr = Expr;
    }
    llvm::Value *codegen() override;
};

class VariableExprAST : public ExprAST {
    string Name;
public:
    VariableExprAST(const string &Name) : Name(Name) {}
    llvm::Value *codegen() override;
};

class IfExprAST : public ExprAST {
    ExprAST *Cond, *Then, *Else;
public:
    IfExprAST(ExprAST *Cond, ExprAST *Then, ExprAST *Else){
        this->Cond = Cond;
        this->Then = Then;
        this->Else = Else;
    }
    llvm::Value *codegen() override;
};

class WhileExprAST : public ExprAST {
    string VarName;
    ExprAST *Cond, *Body;
public:
    WhileExprAST(string VarName, ExprAST *Cond, ExprAST *Body){
        this->Cond = Cond;
        this->Body = Body;
    }
    llvm::Value *codegen() override;
};

class BinaryExprAST : public ExprAST {
    char Op;
    ExprAST *LHS, *RHS;

public:
    BinaryExprAST(char Op, ExprAST *LHS,
                  ExprAST *RHS)
            : Op(Op), LHS(LHS), RHS(RHS) {}
    llvm::Value *codegen() override;
};

static ExprAST *ParseExpression();

static ExprAST *ParseNumberExpr() {
    NumberExprAST *Result = new NumberExprAST(NumVal);
    NextToken();
    return Result;
}

static ExprAST *ParseParenExpr() {
    NextToken();
    ExprAST *V = ParseExpression();
    if (!V)
        return nullptr;

    if (CurToken != ')') {
        printf("%s\n", "expected ')'");
        return nullptr;
    }
    NextToken();
    return V;
}

static ExprAST *ParseVarExpr() {
    string Name = IdentifierStr;

    NextToken();

    if (CurToken != '=') {
        return new VariableExprAST(Name);
    }

    NextToken();

    ExprAST *varDecl = ParseExpression();
    if (!varDecl) {
        printf("expected expr after \"=\"!");
        return nullptr;
    }
    return new AssignAST(Name, varDecl);
}

static ExprAST *ParseIfExpr() {
    NextToken();
    if (CurToken != '(') {
        printf("%s %c %d\n,", "expected '(' after if, no ", CurToken, CurToken);
    }
    NextToken();
    ExprAST *Cond = ParseExpression();
    if (!Cond)
        return nullptr;

    if (CurToken != ')') {
        printf("%s %c %d\n,", "expected ')' after if", CurToken, CurToken);
    }

    NextToken();
    NextToken();

    ExprAST *Then = ParseExpression();
    NextToken();
    ExprAST *Else = ParseExpression();

    return new IfExprAST(Cond, Then, Else);
}

static ExprAST *ParseWhileExpr() {
    NextToken();
    if (CurToken != '(') {
        printf("%s %c %d\n,", "expected '(' after if, no ", CurToken, CurToken);
    }
    NextToken();
    std::string IdName = IdentifierStr;
    ExprAST *Cond = ParseExpression();
    if (!Cond)
        return nullptr;

    if (CurToken != ')') {
        printf("%s %c %d\n,", "expected ')' after if", CurToken, CurToken);
    }

    NextToken();
    if (CurToken != TOK_DO) {
        printf("%s %c %d\n,", "expected 'do' after while", CurToken, CurToken);
    }

    NextToken();
    ExprAST *Body = ParseExpression();

    if (CurToken != TOK_END) {
        printf("%s %c %d\n,", "expected 'end' after while body", CurToken, CurToken);
    }
    NextToken();

    return new WhileExprAST(IdName,Cond, Body);
}

static ExprAST *ParsePrimary() {
    switch (CurToken) {
        case TOK_IDENT:
            return ParseVarExpr();
        case TOK_NUM:
            return ParseNumberExpr();
        case '(':
            return ParseParenExpr();
        case TOK_IF:
            return ParseIfExpr();
        case TOK_WHILE:
            return ParseWhileExpr();
        default:
            printf("unknown token when expecting an expression: %c\n", CurToken);
            return nullptr;
    }
}


static ExprAST *ParseBinary (ExprAST *expr) {
    ExprAST *LHS = expr;
    while (1) {
        if (CurToken!='+' && CurToken!='-' && CurToken!='*')
            return LHS;
        char Op = CurToken;
        NextToken();
        ExprAST *RHS = ParsePrimary();
        if (!RHS)
            return nullptr;
        LHS = new BinaryExprAST(Op, LHS, RHS);
    }
}

static ExprAST *ParseExpression() {
    ExprAST *expr = ParsePrimary();
    if (!expr) return nullptr;
    return ParseBinary(expr);
}

static llvm::Module *MainModule;
static llvm::Function *MainFunction;
static llvm::LLVMContext MainContext;
static llvm::IRBuilder<> Builder(MainContext);
static map<string, llvm::Value *> NamedValues;

llvm::Value *AssignAST::codegen(){
    llvm::Value *v = assExpr->codegen();
    if (!v)
        return nullptr;

    llvm::AllocaInst *Alloca = Builder.CreateAlloca(llvm::Type::getInt32Ty(MainContext), 0, Name.c_str());
    Builder.CreateStore(v, Alloca);
    llvm::Value *CurVar = Builder.CreateLoad(Alloca);

    NamedValues[Name] = Alloca;
    return CurVar;
}

llvm::Value *NumberExprAST::codegen() {
    return llvm::ConstantInt::get(MainContext, llvm::APInt(32, Val, false));
}

llvm::Value *VariableExprAST::codegen() {
    llvm::Value *V = NamedValues[Name];
    if (!V) {
        printf("unknown variable name\n");
        return nullptr;
    }
    return Builder.CreateLoad(V, Name);
}

llvm::Value *BinaryExprAST::codegen() {
    llvm::Value *L = LHS->codegen();
    llvm::Value *R = RHS->codegen();
    if (!L || !R)
        return nullptr;
    switch (Op) {
        case '+':
            return Builder.CreateAdd(L, R, "addtmp");
        case '-':
            return Builder.CreateSub(L, R, "subtmp");
        case '*':
            return Builder.CreateMul(L, R, "multmp");
        default:
            printf("%s\n","invalid binary operator");
            return nullptr;
    }
}

llvm::Value *IfExprAST::codegen() {
    llvm::Value *CondV = Cond->codegen();
    if (!CondV)
        return nullptr;

    CondV = Builder.CreateICmpNE(
            CondV, llvm::ConstantInt::get(MainContext, llvm::APInt(32, 0, false)),
            "ifcond");

    llvm::Function *MainFunction = Builder.GetInsertBlock()->getParent();

    llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(MainContext, "then", MainFunction);
    llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(MainContext, "else");
    llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(MainContext, "ifcont");

    Builder.CreateCondBr(CondV, ThenBB, ElseBB);

    // Генерируем значение.
    Builder.SetInsertPoint(ThenBB);

    llvm::Value *ThenV = Then->codegen();
    if (!ThenV) return nullptr;

    Builder.CreateBr(MergeBB);
    // Кодогенерация 'Then' может изменить текущий блок, обновляем ThenBB для PHI.
    ThenBB = Builder.GetInsertBlock();

    // Генерируем блок else.
    MainFunction->getBasicBlockList().push_back(ElseBB);
    Builder.SetInsertPoint(ElseBB);
    llvm::Value *ElseV = Else->codegen();
    if (!ElseV) return nullptr;

    Builder.CreateBr(MergeBB);
    // Кодогенерация 'Else' может изменить текущий блок, обновляем ElseBB для PHI.
    ElseBB = Builder.GetInsertBlock();


    // Генерация блока слияния.
    MainFunction->getBasicBlockList().push_back(MergeBB);
    Builder.SetInsertPoint(MergeBB);
    llvm::PHINode *PN = Builder.CreatePHI(llvm::Type::getInt32Ty(MainContext), 2, "iftmp");

    PN->addIncoming(ThenV, ThenBB);
    PN->addIncoming(ElseV, ElseBB);
    return PN;
}

static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction,
                                          StringRef VarName) {
    IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                     TheFunction->getEntryBlock().begin());
    return TmpB.CreateAlloca(Type::getDoubleTy(MainContext), nullptr, VarName);
}



llvm::Value *WhileExprAST::codegen() {
    llvm::Value *CondV = Cond->codegen();
    if (!CondV)
        return nullptr;

    CondV = Builder.CreateICmpNE(
            CondV, llvm::ConstantInt::get(MainContext, llvm::APInt(32, 0, false)),
            "whilecond");

    llvm::Function *MainFunction = Builder.GetInsertBlock()->getParent();

    llvm::BasicBlock *WhileBB = llvm::BasicBlock::Create(MainContext, "whilebody", MainFunction);
    llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(MainContext, "whilecont");

    Builder.CreateCondBr(CondV, WhileBB, MergeBB);

    // Генерируем значение.
    Builder.SetInsertPoint(WhileBB);
    llvm::Value *WhileV = Body->codegen();
    if (!WhileV) return nullptr;

    Builder.CreateBr(MergeBB);
    WhileBB = Builder.GetInsertBlock();


    // Генерация блока слияния.
    MainFunction->getBasicBlockList().push_back(MergeBB);
    Builder.SetInsertPoint(MergeBB);
    llvm::PHINode *PN = Builder.CreatePHI(llvm::Type::getInt32Ty(MainContext), 2, "whiletmp");

    PN->addIncoming(WhileV, WhileBB);
    return PN;
}

static llvm::Value *Parse() {
    NextToken();

    ExprAST *expr;
    llvm::Value *RetVal = nullptr;
    while (CurToken != TOK_EOF) {
        expr = ParseExpression();
        if (expr) {
            RetVal = expr->codegen();
            if (!RetVal) NextToken();
        } else {
            return nullptr;
        }
    }

    return RetVal;
}


int main() {
    MainModule = new llvm::Module("main", MainContext);

    llvm::FunctionType *FT =
            llvm::FunctionType::get(llvm::Type::getVoidTy(MainContext),false);
    MainFunction =
            llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "main", MainModule);

    llvm::BasicBlock *BB = llvm::BasicBlock::Create(MainContext, "entry", MainFunction);
    Builder.SetInsertPoint(BB);

    llvm::Value *ret = Parse();
    Builder.CreateRet(ret);

    MainModule->print(llvm::outs(), nullptr);

    return 0;
}