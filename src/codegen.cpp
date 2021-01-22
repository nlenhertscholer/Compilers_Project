/*
CODEGEN.CPP
by Nesta Lenhert-Scholer and Bowen Bao
for MPCS 51300 - Compilers

This file contains all of the LLVM code for each AST node
*/

#include "ast.hpp"

// Bool for scoping
static bool fromFunction = false;

// Retrieve type in LLVM
static llvm::Type* getType(llvm::IRBuilder<>* B, Type* type) {
    llvm::Type* retType = nullptr;
    
    if(type->kind == Type::t_int) {
        retType = B->getInt32Ty();
    } else if(type->kind == Type::t_float) {
        retType = B->getFloatTy();
    } else if(type->kind == Type::t_bool) {
        retType = B->getInt1Ty();
    } else if(type->kind == Type::t_void) {
        retType = B->getVoidTy();
    } else {
        std::cerr << "error: unknown type to convert to llvm IR" << std::endl;
        exit(1);
    }

    if(type->ref) {
        retType = llvm::PointerType::getUnqual(retType);
    }

    return retType;
}

// Allocate a variable at the beginning of a function
static llvm::AllocaInst* createEntryBlockAlloca(llvm::Function *TheFunction,
                                          std::string VarName, Type* type) {
  
  llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                   TheFunction->getEntryBlock().begin());

  return TmpB.CreateAlloca(getType(&TmpB, type), nullptr, VarName);

}

static void createCIntFunction(std::vector<llvm::Type*> argTypes, std::vector<llvm::Value*> fArgs, 
                               std::string funcName) {

    llvm::Function* F = BaseNode::Module->getFunction(funcName);
    if(!F) {
        llvm::FunctionType* ftype = llvm::FunctionType::get(BaseNode::Builder->getVoidTy(), 
                                                            argTypes, false);
        F = llvm::Function::Create(ftype, llvm::Function::ExternalLinkage, funcName, 
                                   BaseNode::Module.get());
    }
    BaseNode::Builder->CreateCall(F, fArgs);
}

llvm::Value* BaseNode::codegen() {
    std::cerr << "error: call to codegen from method that doesn't support it" << std::endl;
    exit(1);
}

llvm::Value* VDecl::codegen() {
    return createEntryBlockAlloca(llvmFunctions.back(), this->varid, this->type);
}

llvm::Value* BoolLit::codegen() {
    return Builder->getInt1(this->value);
}

llvm::Value* FLit::codegen() {
    return llvm::ConstantFP::get(*Context, llvm::APFloat(this->value));
}

llvm::Value* ILit::codegen() {
    return Builder->getInt32(this->value);
}

llvm::Value* UOP::codegen() {
    llvm::Value* val = this->exp->codegen();

    switch(op) {
        case uop_not:
            // TODO: Make sure when this goes to 255, it is still true
            return Builder->CreateNot(val, "nottmp");
        case uop_minus:
            if(this->exp->type->kind == Type::t_float) 
                return Builder->CreateFNeg(val, "fuopminustmp");
            else {
                // TODO: Check this
                if(this->exp->type->check_overflow) {
                    createCIntFunction({Builder->getInt32Ty()}, {val}, "cint_uneg");
                }
                return Builder->CreateNeg(val, "iuopminustmp");
            }
    }
}

llvm::Value* Assignment::codegen() {
    for(auto it = inScopeVars.rbegin(); it != inScopeVars.rend(); it++) {
        auto search = it->find(this->varid);
        if(search != it->end()) {
            llvm::Value* val = this->exp->codegen();
            if(this->type->ref) {
                auto tmp = Builder->CreateLoad(search->second, this->varid);
                return Builder->CreateStore(val, tmp);
            }

            return Builder->CreateStore(val, search->second);
        }
    }

    std::cerr << "error: IR Variable " << this->varid << " not declared for assignment" << std::endl;
    exit(1);
}

llvm::Value* TypeCast::codegen() {
    auto expType = this->exp->type->kind;
    auto castType = this->type->kind;

    llvm::Value* exp = this->exp->codegen();

    switch(expType) {
        case Type::t_int:
            if(castType == Type::t_int)
                return exp;
            else if(castType == Type::t_float)
                return Builder->CreateSIToFP(exp, Builder->getFloatTy(), "sitofptmp");
        case Type::t_float:
            if(castType == Type::t_int)
                return Builder->CreateFPToSI(exp, Builder->getInt32Ty(), "fptositmp");
            else if(castType == Type::t_float)
                return exp;
        case Type::t_bool:
            return exp;
        default:
            std::cerr << "error: unexpected expression type to cast in IR" << std::endl;
            exit(1);
    }
}

llvm::Value* Binop::codegen() {
    llvm::Value* LHS = this->lhs->codegen();
    llvm::Value* RHS = this->rhs->codegen();

    if(!LHS || !RHS) {
        std::cerr << "error: Unable to codegen a side of a binop" << std::endl;
        exit(1);
    }

    auto type = this->lhs->type->kind;

    switch(this->op) {
        case mult:
            if(type == Type::t_float)
                return Builder->CreateFMul(LHS, RHS, "fmultmp");
            else {
                if(this->type->check_overflow) {
                    createCIntFunction({Builder->getInt32Ty(), Builder->getInt32Ty()}, {LHS, RHS},
                                       "cint_mult");
                }
                return Builder->CreateMul(LHS, RHS, "imultmp");
            }
        case div:
            if(type == Type::t_float)
                return Builder->CreateFDiv(LHS, RHS, "fdivtmp");
            else
                if(this->type->check_overflow) {
                    createCIntFunction({Builder->getInt32Ty(), Builder->getInt32Ty()}, {LHS, RHS},
                                       "cint_div");
                }
                return Builder->CreateSDiv(LHS, RHS, "idivtmp");
        case plus:
            if(type == Type::t_float)
                return Builder->CreateFAdd(LHS, RHS, "faddtmp");
            else {
                if(this->type->check_overflow) {
                    createCIntFunction({Builder->getInt32Ty(), Builder->getInt32Ty()}, {LHS, RHS},
                                       "cint_add");
                }
                return Builder->CreateAdd(LHS, RHS, "iaddtmp");
            }
        case minus:
            if(type == Type::t_float)
                return Builder->CreateFSub(LHS, RHS, "fsubtmp");
            else {
                if(this->type->check_overflow) {
                    createCIntFunction({Builder->getInt32Ty(), Builder->getInt32Ty()}, {LHS, RHS},
                                       "cint_minus");
                }
                return Builder->CreateSub(LHS, RHS, "isubtmp");
            }
        case eq:
            if(type == Type::t_float)
                return Builder->CreateFCmpUEQ(LHS, RHS, "fcmpeqtmp");
            else
                return Builder->CreateICmpEQ(LHS, RHS, "icmpeqtmp");
        case lt:
            if(type == Type::t_float)
                return Builder->CreateFCmpULT(LHS, RHS, "fcmplttmp");
            else
                return Builder->CreateICmpSLT(LHS, RHS, "icmplttmp");
        case gt:
            if(type == Type::t_float)
                return Builder->CreateFCmpUGT(LHS, RHS, "fcmpgttmp");
            else
                return Builder->CreateICmpSGT(LHS, RHS, "icmpgttmp");
        case bi_and:
            return Builder->CreateAnd(LHS, RHS, "biandtmp");
        case bi_or:
            return Builder->CreateOr(LHS, RHS, "biortmp"); 
    }

}

llvm::Value* VarId::codegen() {

    for(auto it = inScopeVars.rbegin(); it != inScopeVars.rend(); it++) {
        auto search = it->find(this->varid);
        if(search != it->end()) {
            if(this->type->ref) {
                auto tmp = Builder->CreateLoad(search->second, search->first);
                return Builder->CreateLoad(tmp, search->first);
            }
            return Builder->CreateLoad(search->second, search->first);
        }
    }

    std::cerr << "error: IR Variable " << this->varid << " not declared" << std::endl;
    exit(1);
}

llvm::Value* FuncCall::codegen() {
    llvm::Function* F = Module->getFunction(this->globid);

    if(!F) {
        std::cerr << "error: Call to undeclared function \"" << this->globid << "\"" << std::endl;
        exit(1);
    }

    // Find this function for its variable declarations to check type
    auto it = functions.find(this->globid);
    if(it == functions.end()) {
        std::cerr << "error: unable to find function in functions for function call" << std::endl;
        exit(1);
    }
    auto func = it->second;
    type = func.first;
    VDecls* vdecls = dynamic_cast<VDecls*>(func.second);
    TDecls* tdecls = dynamic_cast<TDecls*>(func.second);

    std::vector<llvm::Value*> fArgs;
    if(this->exps) {
        int i = 0;
        for(auto arg: this->exps->exps) {
            if(vdecls && vdecls->vdecls[i]->type->ref) {
                for(auto it = inScopeVars.rbegin(); it != inScopeVars.rend(); it++) {
                    auto search = it->find(((VarId*)arg)->varid);
                    if(search != it->end()) {
                        if(arg->type->ref) {
                            auto tmp = Builder->CreateLoad(search->second, search->first);
                            fArgs.push_back(tmp);
                            break;
                        }
                        fArgs.push_back(search->second);
                        break;
                    }
                }
            } else {
                fArgs.push_back(arg->codegen());
            }
            i++;
        }
    }

    if(this->type->kind != Type::t_void)
        return Builder->CreateCall(F, fArgs, this->globid);
    else
        return Builder->CreateCall(F, fArgs);
}   


llvm::Value* Stmts::codegen() {
    for(auto s: this->stmts) {
        s->codegen();
    }
}

llvm::Value* Blk::codegen() {

    if(fromFunction) {
        fromFunction = false;
    } else {
        inScopeVars.emplace_back();
    }

    if(this->stmts) {
        this->stmts->codegen();
    }
    // Check to see if end of basic block has a terminator
    llvm::Function* func = llvmFunctions.back();
    llvm::BasicBlock* bb = &func->getBasicBlockList().back();
    if(!bb->getTerminator()) {
        llvm::BasicBlock* entryBlock = &func->getEntryBlock();
        llvm::BranchInst* temp = (llvm::BranchInst*)entryBlock->getTerminator();
        if(temp && temp->isConditional() && temp->getNumSuccessors() == 2) {
            auto endBlock = temp->getSuccessor(1);
            Builder->CreateBr(endBlock);
        } else if(temp && temp->isUnconditional()) {
            auto endBlock = temp->getSuccessor(0);
            Builder->CreateBr(endBlock);
        }
    }

    inScopeVars.pop_back();
}

llvm::Value* Ret::codegen() {
    llvm::Function* func = llvmFunctions.back();
    llvm::Type* retType = func->getReturnType();

    if(retType->isIntegerTy(1) && (!this->exp || this->exp->type->kind != Type::t_bool)) {
        std::cerr << "error: Function \"" << func->getName().str() << "\" expected to return an "
                  << "boolean type" << std::endl;
        exit(1); 
    } else if(retType->isFloatTy() && (!this->exp || this->exp->type->kind != Type::t_float)) {
        std::cerr << "error: Function \"" << func->getName().str() << "\" expected to return an "
                  << "float type" << std::endl;
        exit(1);
    } else if(retType->isIntegerTy() && ((llvm::IntegerType*)retType)->getBitWidth() > 1 
              && (!this->exp || this->exp->type->kind != Type::t_int)) {
        std::cerr << "error: Function \"" << func->getName().str() << "\" expected to return an "
                  << "integer type" << std::endl;
        exit(1);
    } else if(retType->isVoidTy()) {

        if(this->exp) {
            std::cerr << "error: Function \"" << func->getName().str() << "\" expected no return type"
                    << std::endl;
            exit(1);
        }
        return nullptr;
    }
    Builder->CreateRet(this->exp->codegen());
}

llvm::Value* VDeclStmt::codegen() {
    // TODO: deal with ref variables
    // TODO: Might be wierd if code is further down the line? Check

    llvm::Value* rhs = this->exp->codegen();
    
    llvm::AllocaInst* alloca = static_cast<llvm::AllocaInst*>(this->vdecl->codegen());

    if(this->vdecl->type->ref) {
        for(auto it = inScopeVars.rbegin(); it != inScopeVars.rend(); it++) {
            auto search = it->find(((VarId*)this->exp)->varid);
            if(search != it->end()) {
                if(this->exp->type->ref) { 
                    auto tmp = Builder->CreateLoad(search->second, search->first);
                    Builder->CreateStore(tmp, alloca);
                    break;
                }
                Builder->CreateStore(search->second, alloca);
            }
        }
    } else {
        Builder->CreateStore(rhs, alloca);
    }
    inScopeVars.back().emplace(this->vdecl->varid, alloca);
}

llvm::Value* ExpStmt::codegen() {
    this->exp->codegen();
}

llvm::Value* WhileStmt::codegen() {

    // Create necessary blocks
    llvm::Function* func = llvmFunctions.back();
    llvm::BasicBlock* condBlock = llvm::BasicBlock::Create(*Context, "while.cond", func);
    llvm::BasicBlock* bodyBlock = llvm::BasicBlock::Create(*Context, "while.body");
    llvm::BasicBlock* endBlock = llvm::BasicBlock::Create(*Context, "while.end");

    // Branch into the condition
    Builder->CreateBr(condBlock);

    // Generate condition block
    Builder->SetInsertPoint(condBlock);
    llvm::Value* cond = this->exp->codegen();
    Builder->CreateCondBr(cond, bodyBlock, endBlock);

    // Generate Body Block
    func->getBasicBlockList().push_back(bodyBlock);     // Add the body block to function
    Builder->SetInsertPoint(bodyBlock);
    this->stmt->codegen();
    if(!bodyBlock->getTerminator()) {
        Builder->CreateBr(condBlock);
    }

    // End block
    func->getBasicBlockList().push_back(endBlock);
    Builder->SetInsertPoint(endBlock);

}

llvm::Value* IfStmt::codegen() {

    // Generate code for expression
    llvm::Value* cond = this->exp->codegen();
    if(!cond) {
        std::cerr << "error: unable to generate code for if stmt condition" << std::endl;
        exit(1);
    }

    // Generate blocks for statements
    llvm::Function* func = llvmFunctions.back();
    llvm::BasicBlock* thenBlock = llvm::BasicBlock::Create(*Context, "if.then", func);
    llvm::BasicBlock* elseBlock = nullptr;
    if(this->else_stmt)
        elseBlock = llvm::BasicBlock::Create(*Context, "if.else");
    llvm::BasicBlock* mergeBlock = llvm::BasicBlock::Create(*Context, "if.end");

    // Set the branching condition
    if(this->else_stmt)
        Builder->CreateCondBr(cond, thenBlock, elseBlock);
    else
        Builder->CreateCondBr(cond, thenBlock, mergeBlock);

    // Generate statement for then block
    Builder->SetInsertPoint(thenBlock);
    this->stmt->codegen();
    if(!thenBlock->getTerminator()) {
        Builder->CreateBr(mergeBlock);
    }

    // Generate the else statement
    if(this->else_stmt) {
        func->getBasicBlockList().push_back(elseBlock);
        Builder->SetInsertPoint(elseBlock);
        this->else_stmt->codegen();
        if(!elseBlock->getTerminator()) {
            Builder->CreateBr(mergeBlock);
        }
    }

    // Emit merge block.
    func->getBasicBlockList().push_back(mergeBlock);
    Builder->SetInsertPoint(mergeBlock);
}

// Generate a call to printf
static void callPrint(std::string format, llvm::Value* exp = nullptr) {

    // Get the function or create it if it doesn't exist
    llvm::Function* func_printf = BaseNode::Module->getFunction("printf");

    if (!func_printf) {
        llvm::PointerType *Pty = 
            llvm::PointerType::get(llvm::IntegerType::get(*BaseNode::Context, 8), 0);
        llvm::FunctionType *FuncTy = 
            llvm::FunctionType::get(llvm::IntegerType::get(*BaseNode::Context, 32), true);

        func_printf = llvm::Function::Create(FuncTy, llvm::Function::ExternalLinkage, 
                                             "printf", BaseNode::Module.get());
        func_printf->setCallingConv(llvm::CallingConv::C);

        llvm::AttributeList func_printf_PAL;
        func_printf->setAttributes(func_printf_PAL);
    }

    // Get the parameters for printf
    llvm::Value* str = BaseNode::Builder->CreateGlobalStringPtr(format);
    std::vector<llvm::Value*> int32_call_params;
    if(exp)
        int32_call_params = {str, exp};
    else
        int32_call_params = {str};

    // Call printf
    BaseNode::Builder->CreateCall(func_printf, int32_call_params, "print");
}

llvm::Value* Print::codegen() {

    // Get the format for the string
    std::string format;
    switch(this->exp->type->kind) {
        case Type::t_void:
            std::cerr << "error: Cannot print expression of type void" << std::endl;
            exit(1);
        case Type::t_int:
        case Type::t_bool:
            format = "%d\n";
            break;
        case Type::t_float:
            format = "%f\n";
            break;
        default:
            std::cerr << "error: unknown type passed to print" << std::endl;
            exit(1);
        
    }

    // Get the expression to be printed
    llvm::Value* tmp = this->exp->codegen();

    // if(((llvm::ConstantInt*)tmp)->getBitWidth() == 8) {
    //     // This is a boolean
    //     if(((llvm::ConstantInt*)tmp)->isZero())
    //         return callPrint("false\n");
    //     else
    //         return callPrint("true\n");
    // }

    if(this->exp->type->kind == Type::t_float) {
        // Promote it to a double
        tmp = Builder->CreateFPExt(tmp, llvm::Type::getDoubleTy(*Context));
    }

    callPrint(format, tmp);

}

llvm::Value* PrintSlit::codegen() {
    callPrint(this->slit + "\n");
}

llvm::Value* Func::codegen() {
    // TODO: deal with ref types

    // Generate the argument types for the function
    std::vector<llvm::Type*> funcTypes;
    if(this->vdecls) {
        for(auto var: this->vdecls->vdecls) {
            funcTypes.push_back(getType(Builder.get(), var->type));
        }
    }

    // Create the type for the function
    llvm::FunctionType* ftype = llvm::FunctionType::get(getType(Builder.get(), this->type), funcTypes, false);

    // Create the function
    llvm::Function* function = llvm::Function::Create(ftype, llvm::Function::ExternalLinkage,
                                                      this->globid, Module.get());
    llvmFunctions.push_back(function);

    // Create block for the function
    llvm::BasicBlock* block = llvm::BasicBlock::Create(*Context, "entry", function);
    Builder->SetInsertPoint(block);

    // Add the names for the arguments and add them to the scope
    int i = 0;
    inScopeVars.emplace_back();         // Start a new scope
    for(auto &a: function->args()) {
        VDecl* var = this->vdecls->vdecls[i];

        // Allocate memory on the stack for this variable
        llvm::AllocaInst* alloca = static_cast<llvm::AllocaInst*>(var->codegen());
        
        // Store the initial value into it
        Builder->CreateStore(&a, alloca);

        // Set it's name
        a.setName(var->varid);

        // Add to scope
        inScopeVars.back().emplace(var->varid, alloca);

        i++;
    }


    // TODO: Implement type checking for return values
    fromFunction = true;
    this->blk->codegen();

    if(this->type->kind == Type::t_void) {
        Builder->CreateRetVoid();
    }
}

llvm::Value* Funcs::codegen() {
    for(auto f: this->funcs) {
        f->codegen();
    }
}

llvm::Value* Extern::codegen() {
    
    // Get the argument types for the extern func
    std::vector<llvm::Type*> externTypes;
    if(this->tdecls) {
        for(auto it = this->tdecls->types.begin(); it != this->tdecls->types.end(); it++) {
            externTypes.push_back(getType(Builder.get(), *it));
        }
    }

    // Create the type for the function
    llvm::FunctionType* ftype = llvm::FunctionType::get(getType(Builder.get(), this->type), 
                                                        externTypes, false);

    // Create the function
    llvm::Function* externFunc = llvm::Function::Create(ftype, llvm::Function::ExternalLinkage,
                                                        this->globid, Module.get());

    // Add function to global list of funcitons
    // llvmFunctions.push_back(externFunc);

    // Generate names for the args -- might need this later?

}

llvm::Value* Externs::codegen() {
    // Call code gen for each extern function
    for(auto ex: this->externs) {
        ex->codegen();
    }
}

extern std::string infile;      // Input file name

llvm::Value* Prog::codegen() {

    // Initialize context and builder
    Context = std::make_unique<llvm::LLVMContext>();
    Module = std::make_unique<llvm::Module>(infile, *Context);
    Builder = std::make_unique<llvm::IRBuilder<>>(*Context);

    // Generate extern code
    if(this->externs) {
        this->externs->codegen();
    }

    // Generate function code
    this->funcs->codegen();
}