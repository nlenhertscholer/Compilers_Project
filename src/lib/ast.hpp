/*
AST.HPP
by Nesta Lenhert-Scholer and Bowen Bao
for MPCS 51300 - Compilers

This file specifies the Abstract Syntax Tree to be used by the compiler. It is mainly derived
from the language specification and grammar. 
*/

#pragma once

#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <utility>
#include <memory>

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/MC/SubtargetFeature.h"

// Needed for forward declaration
class Type;
class Decls;

class BaseNode {
public:
    // Static variables
    static bool has_entry;
    static bool last_scope_function;
    static std::map<std::string, std::pair<Type*, Decls*>> functions;
    static std::vector<llvm::Function*> llvmFunctions;
    static std::vector<std::map<std::string, Type*>> scopes;
    static std::vector<std::map<std::string, llvm::AllocaInst*>> inScopeVars;
    static std::unique_ptr<llvm::LLVMContext> Context;
    static std::unique_ptr<llvm::Module> Module;
    static std::unique_ptr<llvm::IRBuilder<>> Builder;

    // Virtual functions
    virtual ~BaseNode() {}
    virtual void print_yaml(std::ostream& os, std::string prefix) = 0;
    virtual llvm::Value* codegen();     // Function to generate the IR using LLVM
};

class Type {
public:
    enum type_kind {
        t_int,
        t_float,
        t_bool,
        t_void
    } kind;

    bool ref;
    bool noalias;
    bool check_overflow;

    // Functions
    Type (type_kind k, bool r=false, bool na=false, bool c=false) : 
        kind(k), ref(r), noalias(na), check_overflow(c) {}
    
    std::string name() const{
        std::string n;
        if(ref) {
            if(noalias) {
                n += "noalias ";
            }
            n += "ref ";
        }

        switch (kind){
            case t_int:
                if(check_overflow) {
                    n += "cint";
                } else {
                    n += "int";
                }
                break;
            case t_float:
                n += "float";
                break;
            case t_bool:
                n += "bool";
                break;
            case t_void:
                n += "void";
                break;
        }
        return n;
    }
};

class VDecl : public BaseNode {
public:
    Type* type;
    std::string varid;

    // Functions
    VDecl(Type* t, std::string id, int lineno) : type(t), varid(id) {
        if(this->type->kind == Type::t_void) {
            std::cerr << "error: line " << lineno << " - Cannot declare " << this->varid
                      << " as type void" << std::endl;
            exit(1);
        }

        // Check to see if variable is already in this scope which is an error
        for(auto vdecl: scopes.back()) {
            if(vdecl.first == this->varid) {
                std::cerr << "error: line " << lineno << " - Redefinition of variable "
                          << this->varid << std::endl;
                exit(1);
            }
        }

        // Add the variable to the current scope
        scopes.back().emplace(id, t);
    }

    void print_yaml(std::ostream& os, std::string prefix) override {
        os << prefix << "node: vdecl\n";
        os << prefix << "type: " << type->name() << "\n";
        os << prefix << "var: " << varid << std::endl;
    }

    virtual llvm::Value* codegen() override;
};

class Decls : public BaseNode {};

class TDecls : public Decls {
public:
    std::vector<Type*> types;

    // Functions
    void print_yaml(std::ostream& os, std::string prefix) override{
        os << prefix << "name: tdecls\n";
        os << prefix << "types:\n";
        for (auto t : types)
            os << prefix << " - " << t->name() << std::endl;
    }
};

class VDecls : public Decls {
public:
    std::vector<VDecl*> vdecls;

    // Functions
    void print_yaml(std::ostream& os, std::string prefix) override {
        os << prefix << "name: vdecls\n";
        os << prefix << "vars:\n";
        for (auto v : vdecls) {
            os << prefix << "  -\n";
            v->print_yaml(os, prefix + "    ");
        }
    }
};

class Exp : public BaseNode {
public:
    Type* type;

    Exp() : type(nullptr) {};
};

class Exps : public BaseNode {
public:
    std::vector<Exp *> exps;

    // Functions
    void print_yaml(std::ostream& os, std::string prefix) override {
        os << prefix << "name: exps\n";
        os << prefix << "exps:\n";
        for (auto e : exps){
            os << prefix << "  -\n";
            e->print_yaml(os, prefix + "    ");
        }
    }

};

class BoolLit : public Exp {
public:
    bool value;

    // Functions
    BoolLit(bool val) : value(val) {
        type = new Type(Type::t_bool);
    }

    void print_yaml(std::ostream& os, std::string prefix) override {
        os << prefix << "name: boollit\n";
        os << std::boolalpha << prefix << "value: " << value << "\n";
        os << prefix << "type: bool" << std::endl;
    }

    virtual llvm::Value* codegen() override;
};

class FLit : public Exp {
public:
    float value;

    // Functions
    FLit(float val) : value(val) {
        type = new Type(Type::t_float);
    }

    void print_yaml(std::ostream& os, std::string prefix) override{
        os << prefix << "name: flit\n";
        os << prefix << "value: " << value << "\n";
        os << prefix << "type: float" << std::endl;
    }

    virtual llvm::Value* codegen() override;
};

class ILit : public Exp {
public:
    int value;

    // Functions
    ILit(int val) : value(val) {
        type = new Type(Type::t_int);
    }

    void print_yaml(std::ostream& os, std::string prefix) override{
        os << prefix << "name: ilit\n";
        os << prefix << "value: " << value << "\n";
        os << prefix << "type: int" << std::endl;
    }

    virtual llvm::Value* codegen() override;
};

class UOP : public Exp {
public:
    enum uop_type {
        uop_not,
        uop_minus
    } op;

    Exp* exp;

    // Functions
    UOP(uop_type o, Exp* e, int lineno) : op(o), exp(e) {
        
        // Ensure that operations have the correct type
        if(op == uop_not && exp->type->kind != Type::t_bool) {
            std::cerr << "error: line " << lineno << " - \"Unary not\" operator cannot be used on a"
                      << " numeric type" << std::endl;
            exit(1);
        } else if(op == uop_minus && exp->type->kind == Type::t_bool) {
            std::cerr << "error: line " << lineno << " - \"Unary minus\" operator cannot be used on"
                      << " a boolean type" << std::endl;
            exit(1);
        }
        if(exp->type->ref) {
            type = new Type(exp->type->kind);
            if(exp->type->check_overflow)
                 type->check_overflow = true;
        }
        else {
            type = exp->type;
        }
    }

    void print_yaml(std::ostream& os, std::string prefix) override {
        std::string type;
        switch(op) {
            case uop_not: type = "not"; break;
            case uop_minus: type = "minus"; break;
        }

        os << prefix << "name: uop\n";
        os << prefix << "op: " << type << "\n";
        os << prefix << "type: " << this->type->name() << "\n";
        os << prefix << "exp:\n";
        exp->print_yaml(os, prefix + "    ");
    }

    virtual llvm::Value* codegen() override;
};

class Assignment : public Exp {
public:
    std::string varid;
    Exp* exp;

    // Functions
    Assignment(std::string v, Exp* e, int lineno) : varid(v), exp(e) {
        
        // Check to make sure that the variable is being assigned the right type
        for(auto scope = scopes.rbegin(); scope != scopes.rend(); scope++) {
            auto it = scope->find(varid);
            if(it != scope->end()) {
                if(it->second->kind != exp->type->kind) {
                    std::cerr << "error: line " << lineno << " - Variable of type \"" 
                              << it->second->name() << "\" cannot be assigned an expression of type \""
                              << exp->type->name() << "\"" << std::endl;
                    exit(1);
                }

                this->type = it->second;
            } 
        }
    }

    void print_yaml(std::ostream& os, std::string prefix) override {
        os << prefix << "name: assignment\n";
        os << prefix << "var: " << varid << "\n";
        os << prefix << "type: " << type->name() << "\n";
        os << prefix << "exp:\n";
        exp->print_yaml(os, prefix + "    "); 
    }

    virtual llvm::Value* codegen() override;
};

class TypeCast : public Exp {
public:
    Exp* exp;

    // Functions
    TypeCast(Type* t, Exp* e, int lineno) : exp(e) {
        // Check that the castings are legal
        if(t->kind == Type::t_bool && exp->type->kind != Type::t_bool) {
            std::cerr << "error: line " << lineno << " - Cannot cast bool expression "
                      << "to a numeric type" << std::endl;
            exit(1);
        } else if(t->kind != Type::t_bool && exp->type->kind == Type::t_bool) {
            std::cerr << "error: line " << lineno << " - Cannot cast numeric expression "
                      << "to a boolean type" << std::endl;
            exit(1);
        } else if(t->ref) {
            std::cerr << "error: line " << lineno << " - Cannot cast expression to a ref type"
                      << std::endl;
            exit(1);
        }
            
        this->type = t;
    }

    void print_yaml(std::ostream& os, std::string prefix) override {
        os << prefix << "name: typecast\n";
        os << prefix << "type: " << type->name() << "\n";
        os << prefix << "exp:\n";
        exp->print_yaml(os, prefix + "    ");
    }

    virtual llvm::Value* codegen() override;
};

class Binop : public Exp {
public:
    enum binop_type {
        mult,
        div,
        plus,
        minus,
        eq,
        lt,
        gt,
        bi_and,
        bi_or
    } op;

    Exp* lhs, * rhs;

    // Functions
    Binop(binop_type sym, Exp* l, Exp* r, int lineno) : op(sym), lhs(l), rhs(r) {
        
        // Check that both sides of the expression are the same
        if(lhs->type->kind != rhs->type->kind) {
            std::cerr << "error: line " << lineno << " - Types in a binary expression must be"
                      << " the same. Current types are [" << lhs->type->name() << ", " 
                      << rhs->type->name() << "]" << std::endl;
            exit(1);
        }

        apply_type(lineno);
    }

    void print_yaml(std::ostream& os, std::string prefix) override {
        std::string type;
        switch(op) {
            case mult: type ="mult"; break;
            case div: type ="div"; break;
            case plus: type ="add"; break;
            case minus: type ="sub"; break;
            case eq: type ="eq"; break;
            case lt: type ="lt"; break;
            case gt: type ="gt"; break;
            case bi_and: type ="and"; break;
            case bi_or: type ="or"; break;
        }

        os << prefix << "name: binop\n";
        os << prefix << "op: " << type << "\n";
        os << prefix << "type: " << this->type->name() << "\n";
        os << prefix << "lhs:\n";
        lhs->print_yaml(os, prefix + "    ");
        os << prefix << "rhs:\n";
        rhs->print_yaml(os, prefix + "    ");
    }

    virtual llvm::Value* codegen() override;

private:
    void apply_type(int line) {
        // Apply the correct type based on expressions
        switch(op) {
            case eq:
                if(lhs->type->kind == Type::t_bool && !lhs->type->ref) {
                    type = lhs->type;
                } else {
                    type = new Type(Type::t_bool);
                }
                break;
            case lt:
            case gt:
                if(lhs->type->kind == Type::t_bool) {
                    std::cerr << "error: line " << line << " - Inequality operator cannot be used "
                              << "on a boolean type" << std::endl;
                    exit(1);
                }

                type = new Type(Type::t_bool);
                break;
            case bi_and:
            case bi_or:
                if(lhs->type->kind != Type::t_bool) {
                    std::cerr << "error: line " << line << " - Boolean operators cannot be used "
                              << "on numeric types" << std::endl;
                    exit(1);
                }
                if(lhs->type->ref)
                    type = new Type(lhs->type->kind);
                else
                    type = lhs->type;
                break;
            default:    // Arithmetic types
                if(lhs->type->ref) {
                    type = new Type(lhs->type->kind);
                    if(lhs->type->check_overflow)
                        type->check_overflow = true;
                }
                else
                    type = lhs->type;
        }
    }

};

class VarId : public Exp {
public:
    std::string varid;

    // Functions
    VarId(std::string id, int lineno) : varid(id) {

        // Check to see if variable is declared before they are used
        bool found = false;
        for(auto scope = scopes.rbegin(); scope != scopes.rend(); scope++) {
            auto it = scope->find(this->varid);
            if(it != scope->end()) {
                found = true;
                type = it->second;
                break;
            }
        }
        if(!found) {
            std::cerr << "error: line " << lineno << " - Variable " << varid << " not declared" 
                      << std::endl;
            exit(1);
        }

    }

    void print_yaml(std::ostream& os, std::string prefix) override {
        os << prefix << "name: varid\n";
        os << prefix << "type: " << type->name() << "\n";
        os << prefix << "var: " << varid << std::endl; 
    }

    virtual llvm::Value* codegen() override;
};

class FuncCall : public Exp {
public:
    std::string globid;
    Exps* exps;

    // Functions
    FuncCall(std::string id, int lineno, Exps* e = nullptr) : globid(id), exps(e) {
        
        auto existingFunc = functions.find(id);

        // See if the function has been declared/defined previously
        if(existingFunc == functions.end()) {
            std::cerr << "error: line " << lineno << " - Call to undeclared function: "
                      << id << std::endl;
            exit(1);
        }
        
        auto func = existingFunc->second;
        type = func.first;
        VDecls* vdecls = dynamic_cast<VDecls*>(func.second);
        TDecls* tdecls = dynamic_cast<TDecls*>(func.second);

        // The following checks the number of arguments as well as making sure they are correct 
        // for reference variables
        // TODO: Ensure that correct type is passed in
        if(vdecls) {
            if(e && e->exps.size() == vdecls->vdecls.size()) {
                for(int i = 0; i < e->exps.size(); ++i) {
                    if(vdecls->vdecls[i]->type->ref && !dynamic_cast<VarId*>(e->exps[i])) {
                            std::cerr << "error: line " << lineno << " - Argument passed to "
                                    << "reference parameter " << vdecls->vdecls[i]->varid
                                    << " must be a variable." << std::endl;
                            exit(1);
                    }
                }
            } else if(!e && vdecls->vdecls.size() > 0) {
                std::cerr << "error: line " << lineno << " - No arguments passed for"
                        << " function \"" << id << "\". Expected " << vdecls->vdecls.size()
                        << std::endl;
                exit(1);
            } else {
                std::cerr << "error: line " << lineno << " - Unequal number of arguments for"
                        << " function \"" << id << "\". Expected " << vdecls->vdecls.size()
                        << " but received " <<  e->exps.size() << std::endl;
                exit(1);
            }
        } else if(tdecls) {
            if(e && e->exps.size() == tdecls->types.size()) {
                for(int i = 0; i < e->exps.size(); ++i) {
                    if(tdecls->types[i]->ref && !dynamic_cast<VarId*>(e->exps[i])) {
                            std::cerr << "error: line " << lineno << " - Argument passed to "
                                    << "parameter " << i << " must be a variable." << std::endl;
                            exit(1);
                    }
                }
            } else if(!e && tdecls->types.size() > 0) {
                std::cerr << "error: line " << lineno << " - No arguments passed for"
                        << " function \"" << id << "\". Expected " << tdecls->types.size()
                        << std::endl;
                exit(1);
            } else {
                std::cerr << "error: line " << lineno << " - Unequal number of arguments for"
                        << " function \"" << id << "\". Expected " << tdecls->types.size()
                        << " but received " <<  e->exps.size() << std::endl;
                exit(1);
            }
        }

    }

    void print_yaml(std::ostream& os, std::string prefix) override {
        os << prefix << "name: funccall\n";
        os << prefix << "globid: " << globid << "\n";
        os << prefix << "type: " << type->name() << "\n";
        if (!exps) {
            return;
        }

        os << prefix << "params:\n";
        exps->print_yaml(os, prefix + "    ");
    }

    virtual llvm::Value* codegen() override;
};

class Stmt : public BaseNode {};

class Stmts :  public BaseNode {
public:
    std::vector<Stmt*> stmts;

    // Functions
    void print_yaml(std::ostream& os, std::string prefix) override {
        os << prefix << "name: stmts\n";
        os << prefix << "stmts:\n";
        for (auto s : stmts) {
            os << prefix << "  -\n";
            s->print_yaml(os, prefix + "    ");
        }  
    }

    virtual llvm::Value* codegen() override;
};

class Blk : public Stmt {
public:
    Stmts* stmts;

    // Functions
    Blk(Stmts* s = nullptr) : stmts(s) {}

    void print_yaml(std::ostream& os, std::string prefix) override {
        os << prefix << "name: blk\n";
        if (!stmts) {
            return;
        }
        os << prefix << "contents:\n";
        stmts->print_yaml(os, prefix + "    ");
    }

    virtual llvm::Value* codegen() override;
};

class Ret : public Stmt {
public:
    Exp* exp;

    // Functions
    Ret(int lineno, Exp* e = nullptr) : exp(e) {
        if(exp && exp->type->ref) {
            std::cerr << "error: line " << lineno << " - Function cannot return a ref type" 
                      << std::endl;
            exit(1);

        }
    }

    void print_yaml(std::ostream& os, std::string prefix) override {
        os << prefix << "name: ret\n";
        if (!exp){
            return;
        }
        os << prefix << "exp:\n";
        exp->print_yaml(os, prefix + "    ");
    }

    virtual llvm::Value* codegen() override;
};

class VDeclStmt : public Stmt {
public:
    VDecl* vdecl;
    Exp* exp;

    // Functions
    VDeclStmt(VDecl* v, Exp* e, int lineno) : vdecl(v), exp(e) {

        // Check for reference variable
        // If so, then make sure RHS is a variable
        if(v->type->ref) {
            if(!dynamic_cast<VarId*>(e)) {
                std::cerr << "error: line " << lineno << " - Initialization of a reference "
                          << "variable must be a variable." << std::endl;
                exit(1);
            }
        }

        // Check to see if the types match
        if(vdecl->type->kind != exp->type->kind) {
            std::cerr << "error: line " << lineno << " - Variable of type \"" << vdecl->type->name()
                      << "\" cannot be assigned an expression of type \"" << exp->type->name() 
                      << "\"" << std::endl;
            exit(1);

        }

    }

    void print_yaml(std::ostream& os, std::string prefix) override {
        os << prefix << "name: vdeclstmt\n";
        os << prefix << "vdecl:\n";
        vdecl->print_yaml(os, prefix + "    ");
        os << prefix << "exp:\n";
        exp->print_yaml(os, prefix + "    ");
    }

    virtual llvm::Value* codegen() override;
};

class ExpStmt : public Stmt {
public:
    Exp* exp;

    // Functions
    ExpStmt(Exp* e) : exp(e) {}

    void print_yaml(std::ostream& os, std::string prefix) override {
        os << prefix << "name: expstmt\n";
        os << prefix << "exp:\n";
        exp->print_yaml(os, prefix + "    ");
    }

    virtual llvm::Value* codegen() override;
};

class WhileStmt : public Stmt {
public:
    Exp* exp;
    Stmt* stmt;

    // Functions
    WhileStmt(Exp* e, Stmt* s, int lineno) : exp(e), stmt(s) {
        
        // Check to make sure exp is a boolean
        if(exp->type->kind != Type::t_bool) {
            std::cerr << "error: line " << lineno << " - Condition in while loop must be a boolean "
                      << "expression" << std::endl;
            exit(1);
        }
    }

    void print_yaml(std::ostream& os, std::string prefix) override {
        os << prefix << "name: while\n";
        os << prefix << "cond:\n";
        exp->print_yaml(os, prefix + "    ");
        os << prefix << "stmt:\n";
        stmt->print_yaml(os, prefix + "    ");
    }

    virtual llvm::Value* codegen() override;
};

class IfStmt : public Stmt {
public:
    Exp* exp;
    Stmt* stmt, * else_stmt;

    // Functions
    IfStmt(Exp* e, Stmt* s, int lineno, Stmt* es = nullptr) : exp(e), stmt(s), else_stmt(es) {
        
        // Check to make sure that exp is a boolean
        if(exp->type->kind != Type::t_bool) {
            std::cerr << "error: line " << lineno << " - Condition in an if statement must be a "
                      << "boolean expression" << std::endl;
            exit(1);
        }
    }

    void print_yaml(std::ostream& os, std::string prefix) override {
        os << prefix << "name: if\n";
        os << prefix << "cond:\n";
        exp->print_yaml(os, prefix + "    ");
        os << prefix << "stmt:\n";
        stmt->print_yaml(os, prefix + "    ");
        if (!else_stmt){
            return;
        }
        os << prefix << "else_stmt:\n";
        else_stmt->print_yaml(os, prefix + "    "); 
    }

    virtual llvm::Value* codegen() override;
};

class Print : public Stmt {
public:
    Exp* exp;

    // Functions
    Print(Exp* e) : exp(e) {}

    void print_yaml(std::ostream& os, std::string prefix) override {
        os << prefix << "name: print\n";
        os << prefix << "exp:\n";
        exp->print_yaml(os, prefix + "    ");
    }

    virtual llvm::Value* codegen() override;
};

class PrintSlit : public Stmt {
public:
    std::string slit;

    // Functions
    PrintSlit(std::string s) {
        this->slit = s.substr(1, s.size()-2);
    }

    void print_yaml(std::ostream& os, std::string prefix) override {
        os << prefix << "name: printslit\n";
        os << prefix << "string: " << slit << std::endl;
    }

    virtual llvm::Value* codegen() override;
};

class Func : public BaseNode {
public:
    Type* type;
    std::string globid;
    VDecls* vdecls;
    Blk* blk;

    // Functions
    Func(Type* t, std::string id, Blk* b, int lineno, VDecls* v = nullptr) 
        : type(t), globid(id), vdecls(v), blk(b) {
            
            // Check if it returns a reference which is an error
            if(this->type->ref) {
                std::cerr << "error: line " << lineno << " - Cannot return a ref type for "
                          << "function: " << id << std::endl;
                exit(1);
            }

            // Check if this is the run function
            if(id == "run") {

                has_entry = true;

                if(t->kind != Type::t_int) {
                    std::cerr << "error: line " << lineno << " - \"run\" function must return "
                              << "type int" << std::endl;
                    exit(1);
                }

                if(v) {
                    std::cerr << "error: line " << lineno << " - \"run\" function must not "
                              << "contain arguments." << std::endl;
                    exit(1);
                }
            }

        }

    void print_yaml(std::ostream& os, std::string prefix) override {
        os << prefix << "name: func\n";
        os << prefix << "type: " << type->name() << "\n";
        os << prefix << "globid: " << globid << "\n";
        if(vdecls) {
            os << prefix << "vdecls:\n";
            vdecls->print_yaml(os, prefix + "    ");
        }
        
        os << prefix << "blk:\n";
        blk->print_yaml(os, prefix + "    ");
    }

    virtual llvm::Value* codegen() override;
};

class Funcs : public BaseNode {
public:
    std::vector<Func*> funcs;

    // Functions
    void print_yaml(std::ostream& os, std::string prefix) override {
        os << prefix << "name: funcs\n";
        os << prefix << "funcs:\n";
        for (auto f: funcs){
            os << prefix << "  -\n";
            f->print_yaml(os, prefix + "    ");
        }
    }

    virtual llvm::Value* codegen() override;
};

class Extern : public BaseNode {
public:
    Type* type;
    std::string globid;
    TDecls* tdecls;

    // Functions
    Extern(Type* t, std::string id, int lineno, TDecls* td = nullptr) 
        : type(t), globid(id), tdecls(td) {

        // Check if it returns a reference which is an error
        if(this->type->ref) {
            std::cerr << "error: line " << lineno << " - Cannot return a ref type for "
                      << "extern function: " << id << std::endl;
            exit(1);
        }

        if(tdecls) {
            for(auto t: tdecls->types) {
                if(t->kind == Type::t_void) {
                    std::cerr << "error: line " << lineno << " - Extern function cannot have type void "
                            << "as a parameter" << std::endl;
                    exit(1);
                }
            }
        }
    }

    void print_yaml(std::ostream& os, std::string prefix) override {
        os << prefix << "name: extern\n";
        os << prefix << "type: " << type->name() << "\n";
        os << prefix << "globid: " << globid << "\n";
        if (!tdecls){
            return;
        }
        os << prefix << "tdecls:\n";
        tdecls->print_yaml(os, prefix + "    ");
    }

    virtual llvm::Value* codegen() override;
};

class Externs : public BaseNode {
public:
    std::vector<Extern*> externs;

    // Functions
    void print_yaml(std::ostream& os, std::string prefix) override {
        os << prefix << "name: externs\n";
        os << prefix << "externs:\n";
        for (auto e: externs){
            os << prefix << "  -\n";
            e->print_yaml(os, prefix + "    ");
        }
    }

    virtual llvm::Value* codegen() override;
};

class Prog : public BaseNode {
public:
    Externs* externs;
    Funcs* funcs;

    // Functions
    Prog(Funcs* f, Externs* e = nullptr) : externs(e), funcs(f) {}

    void print_yaml(std::ostream& os, std::string prefix) override {
        os << prefix << "name: prog\n";
        os << prefix << "funcs:\n";
        funcs->print_yaml(os, prefix + "    ");
        if (!externs){
            return; 
        }
        os << prefix << "externs:\n";
        externs->print_yaml(os, prefix + "    ");
    }

    virtual llvm::Value* codegen() override;
};
