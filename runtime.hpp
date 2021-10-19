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


class RuntimeBlock {
public:
    RuntimeBlock() {
        fun = nullptr;
        currentBB = nullptr;
        hasRet = false;
    }
    ~RuntimeBlock(){}

    void setFun(llvm::Function *f1){
        fun = f1;
    }

    void setHasRet(){
        hasRet = true;
    }

    bool getHasRet(){
        return hasRet;
    }
    llvm::Function *getFun(){
        return fun;
    }
    
    void addArg(std::string name, llvm::Type *type, PassMode p){

        args.push_back(type);
        pass[name] = p;
        vars[name] = type;
    }

    bool containsVar(std::string key){
        if(vars.count(key) == 1) return true;
        return false;
    }

    void addVar(std::string name, llvm::Type *type, PassMode p){
        pass[name] = p;
        vars[name] = type;
    }

    llvm::Type *getVar(std::string name){
        if (vars.count(name) == 0) {return nullptr;}
        return vars[name];
    }
    
    void addAddr(std::string name, llvm::AllocaInst *alloca){
        addrs[name] = alloca;
    }

    llvm::AllocaInst *getAddr(std::string name){
        if (addrs.count(name) == 0) {return nullptr;}
        return addrs[name];
    }

    void addVal(std::string name, llvm::Value *v){
        values[name] = v;
    }

    llvm::Value *getVal(std::string name){
        if (values.count(name) == 0) {return nullptr;}
        return values[name];
    }


    std::vector<std::string> getAllKeys(){
        std::vector<std::string> keys; 
        for(auto it = vars.begin(); it!= vars.end(); ++it){
            keys.push_back(it->first);
        }
        return keys;
    }

    void setBlock(llvm::BasicBlock *BB){
        currentBB = BB;
        hasRet = false;
    }

    llvm::BasicBlock * getCurrentBasicBlock(){
        return currentBB;
    }

    std::vector<llvm::Type *> getArgs(){
        return args;
    }

    bool isRef(std::string name){
        return pass[name]==REF;
    }

    PassMode getPassMode(std::string name){
        return pass[name];
    }


private:
    llvm::Function *fun;
    std::vector<llvm::Type *> args;
    std::map<std::string, llvm::Type *> vars;
    std::map<std::string, llvm::Value *> values;
    std::map<std::string, llvm::AllocaInst *> addrs;
    std::map<std::string, PassMode> pass;
    llvm::BasicBlock *currentBB;
    bool hasRet;
};

class RuntimeScope {
public:
    RuntimeScope() {}
    ~RuntimeScope() {}

    void openRuntimeScope(){
        functions.push_back(std::map<std::string, llvm::Function *>());
    }

    void closeRuntimeScope(){
        if(functions.empty()){
            yyerror("Trying to close scope, none available!");
            return;
        }        
        functions.pop_back();
    }

    void insertFunc(std::string name, llvm::Function *Fun){
        functions.back()[name] = Fun;
    }

    llvm::Function* getFun(std::string name){
        for (auto i = functions.rbegin(); i!= functions.rend(); i++){
            if( (*i).find(name) != (*i).end() ){
                return (*i)[name];
            }
        }
        return nullptr;
    }

private:
    std::vector<std::map<std::string, llvm::Function *>> functions;
    
};

static std::pair<std::vector<std::string>, std::vector<llvm::Type*>> transferPrevBlockVariables(std::vector<RuntimeBlock *> blocks){
    std::vector<std::string> newKeys;
    std::vector<llvm::Type *> types;
    if(blocks.size() < 2) return std::make_pair(newKeys, types);
    
    // Get all keys from previous scope
    RuntimeBlock *secondLast =  blocks.end()[-2];
    RuntimeBlock *last = blocks.back();
    std::vector<std::string> keys = secondLast->getAllKeys();
    
    //Iterate and add only the new ones
    for(auto it = keys.begin(); it!= keys.end(); ++it){    
        if(last->containsVar(*it)) continue;
        newKeys.push_back(*it);
        types.push_back(secondLast->getVar(*it));
        //last->addVar(*it, getLLVMRefType1(secondLast->getVar(*it)), REF);
    }
    return std::make_pair(newKeys, types);
}
/* 

 static llvm::Type *lookupVar(std::vector<RuntimeBlock *> blocks, std::string name){
    llvm::Type *t = nullptr;
    for (auto i = blocks.rbegin(); i!= blocks.rend(); i++){
        t = (*i)->getVar(name);
        if (t!= nullptr) return t;
    }
    return nullptr;
}

static llvm::AllocaInst *lookupAddr(std::vector<RuntimeBlock *> blocks, std::string name){
    llvm::AllocaInst *t = nullptr;
    for (auto i = blocks.rbegin(); i!= blocks.rend(); i++){
        t = (*i)->getAddr(name);
        if (t!= nullptr) return t;
    }
    return nullptr;
} */

static llvm::Value *lookupVal(std::vector<RuntimeBlock *> blocks, std::string name){
    llvm::Value *t= nullptr;
    for (auto i = blocks.rbegin(); i!= blocks.rend(); i++){
        t = (*i)->getVal(name);
        if (t!= nullptr) return t;
    }
    return nullptr;
}


extern LLVMListTypes llvm_list_types;

#endif