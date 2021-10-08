#ifndef __RUNTIME_HPP__
#define __RUNTIME_HPP__

#include "llvm/IR/Type.h"
#include <llvm/IR/Value.h>
#include "llvm/IR/Instructions.h"

class LLVMListTypes {
public:
    LLVMListTypes() {}
    ~LLVMListTypes () {}
    llvm::Type* lookup(std::string hash) {
        if (list_map.find(hash) == list_map.end()) return nullptr;
        return list_map[hash];
    }

    void insert(std::string hash, llvm::Type* t){
        if (list_map.find(hash) != list_map.end()) {
            yyerror("List type: %s already defined", hash.c_str());
        }
        list_map[hash] = t;
    }
private:
    std::map<std::string, llvm::Type*> list_map;
};

struct ValueEntry {
    llvm::Type       * varType;
    llvm::Value      * varValue;
    llvm::AllocaInst * varAlloca;
    ValueEntry () {};
    ValueEntry(llvm::Type *t, llvm::Value *v, llvm::AllocaInst *a) : varType(t), varValue(v), varAlloca(a) {}; 
};

class RuntimeScope {
public:
    RuntimeScope() {}

    void addVariable(std::string name, llvm::Type *Ty, llvm::Value *Val, llvm::AllocaInst * Alloca){
        if (LocalVariables.find(name) != LocalVariables.end()) yyerror("Variable already declared");
        LocalVariables[name] = ValueEntry(Ty, Val, Alloca);
    }

    ValueEntry *lookupVar(std::string c) {
        if (LocalVariables.find(c) == LocalVariables.end()) {return nullptr;}
        return &LocalVariables[c];
    }

    void addFunc(std::string name, llvm::Function *fun){
        if (LocalFunctions.find(name) != LocalFunctions.end()) yyerror("Function %salready defined", name.c_str());
        LocalFunctions[name] = fun;
    }

    llvm::Function *lookupFunc(std::string c) {
        if (LocalFunctions.find(c) == LocalFunctions.end()) {return nullptr;}
        return LocalFunctions[c];
    }
private:
    std::map<std::string, ValueEntry> LocalVariables;
    std::map<std::string, llvm::Function *> LocalFunctions;
};

class RuntimeTable {
public:
    RuntimeTable() {}
    ~RuntimeTable () {}
    ValueEntry *lookup(std::string c) {
        for (auto i = scopes.rbegin(); i!= scopes.rend(); i++){
            ValueEntry *e = i->lookupVar(c);
            if (e!= nullptr) return e;
        }
        return nullptr;
    }

    void insertVar(std::string c, llvm::Type* t, llvm::Value *Val, llvm::AllocaInst * Alloca){
        scopes.back().addVariable(c, t, Val, Alloca);
    }

    llvm::Function *lookupFunc(std::string c) {
        for (auto i = scopes.rbegin(); i!= scopes.rend(); i++){
            llvm::Function *e = i->lookupFunc(c);
            if (e!= nullptr) return e;
        }
        return nullptr;
    }

    void insertFunc(std::string c, llvm::Function* fun){
        scopes.back().addFunc(c, fun);
    }

    void openScope(){
        scopes.push_back(RuntimeScope());
    }

    void closeScope(){
        scopes.pop_back();
    }

    void setCurrentFunction(llvm::Function *fun){
        function = fun;
    }

    llvm::Function *getCurrentFunction(){
        return function;
    }

private:
    llvm::Function *function;
    std::vector<RuntimeScope> scopes;
};

extern RuntimeTable rt;
extern LLVMListTypes llvm_list_types;

#endif