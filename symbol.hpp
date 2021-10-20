#ifndef __SYMBOL_HPP___
#define __SYMBOL_HPP___
#pragma once

#include <vector>
#include <map>
#include <iostream>
#include "type.hpp"
#include <string>
#include "llvm/IR/Function.h"

void yyerror(const char *msg, ...);

enum TableType {T_FUNC, T_VAR, T_BOTH};

struct SymbolEntry {
    TonyType* type;
    SymbolEntry() {}
    SymbolEntry(TonyType* t) : type(t) {};
};

class Scope {
public:
    Scope(TonyType *t) : locals(), functions(), size(0), returnType(t), hasReturn(false) {}
    
    SymbolEntry *lookupVar(std::string c) {
        if (locals.find(c) == locals.end()) {return nullptr;}
        return &locals[c];
    }
    SymbolEntry *lookupFunc(std::string c) {
        if (functions.find(c) == functions.end()) {return nullptr;}
        return &functions[c];
    }

    SymbolEntry *lookupBoth(std::string c) {
        if (locals.find(c) != locals.end()) return &locals[c];

        if (functions.find(c) == functions.end()) {return nullptr;}
        return &functions[c];
    }
        
    void insertVar(std::string c, TonyType* t){
        if (locals.find(c) != locals.end()) yyerror("Variable already declared");
        locals[c] = SymbolEntry(t);
        ++size;
    }

    void insertFunc(std::string c, TonyType* t){
        if (functions.find(c) != functions.end()) yyerror("Function %s already declared", c.c_str());
        functions[c] = SymbolEntry(t);
        ++size;
    }
    int getSize() const { return size; }
    TonyType *getReturnType() { return returnType;}

    //For testing
    void printScope(int ident) { 
        
        for (auto i = locals.begin(); i != locals.end(); ++i){
            std::cout << std::string(ident*4, ' ') << i->first << " " << i->second.type << std::endl;
        } 
        for (auto i = functions.begin(); i != functions.end(); ++i){
            std::cout << std::string(ident*4, ' ') << i->first << " " << i->second.type << std::endl;
        }   
    }
    void setHasReturn(){
        hasReturn = true;
    }
    bool getHasReturn(){
        return hasReturn;
    }
    
    void setScopeFunction(TonyType *f){
        functionType = f;
    }
    
    TonyType * getScopeFunction(){
        return functionType;
    }
private:
    std::map<std::string, SymbolEntry> locals;
    std::map<std::string, SymbolEntry> functions;
    int size;
    TonyType *returnType;
    TonyType *functionType;
    bool hasReturn;
};

class SymbolTable{
public: 
    SymbolEntry *lookup(std::string c, TableType l) {
        for (auto i = scopes.rbegin(); i!= scopes.rend(); i++){
            SymbolEntry *e;
            if (l == T_VAR)         e = i->lookupVar(c);
            else if (l == T_FUNC)   e = i->lookupFunc(c);
            else                    e = i->lookupBoth(c);
            if (e!= nullptr) return e;
        }
        return nullptr;
    }

    SymbolEntry *lookupCurentScope(std::string c, TableType l) {
        SymbolEntry *e;
        if (l == T_VAR)         e = scopes.back().lookupVar(c);
        else if (l == T_FUNC)   e = scopes.back().lookupFunc(c);
        else                    e = scopes.back().lookupBoth(c);
        if (e!= nullptr) return e;
        return nullptr;
    }

    void insert(std::string c, TonyType* t, TableType l){
        if (l == T_VAR)       scopes.back().insertVar(c, t);
        else                  scopes.back().insertFunc(c, t);
    }

    bool hasParentScope(){
        return scopes.size() > 1;
    }
    void insertIntoParentScope(std::string c, TonyType* t, TableType l){
        if (l == T_VAR)       scopes.at(scopes.size() -2).insertVar(c, t);
        else                  scopes.at(scopes.size() -2).insertFunc(c, t);
    }

    SymbolEntry * lookupParentScope(std::string c, TableType l){
        if(hasParentScope()){
            SymbolEntry *e;
            if (l == T_VAR)         e = scopes.at(scopes.size() -2).lookupVar(c);
            else if (l == T_FUNC)   e = scopes.at(scopes.size() -2).lookupFunc(c);
            else                    e = scopes.at(scopes.size() -2).lookupBoth(c);
            if (e!= nullptr) return e;
            
        }
        return nullptr;
    }
    void openScope(TonyType *t){
        scopes.push_back(Scope(t));
    }
    void closeScope(){
        scopes.pop_back();
    }

    void setScopeFunction(TonyType *f){
        scopes.back().setScopeFunction(f);
    }
    TonyType *getScopeFunction(){
       return scopes.back().getScopeFunction();
    }

    int getSizeOfCurrentScope() const {
        return scopes.back().getSize();
    }

    TonyType *getCurrentScopeReturnType (){
        return scopes.back().getReturnType();
    }

    void setScopeHasReturn(){
        scopes.back().setHasReturn();
    }

    bool getScopeHasReturn(){
        return scopes.back().getHasReturn();
    }

    //For testing
    void printSymbolTable(){
        std::cout << "Printing symbol table" << std::endl;
        int scopecounter=0;
        for (auto i = scopes.begin(); i!= scopes.end(); i++){
            
            std::cout << std::string(scopecounter*4, ' ') << "<Scope " << scopecounter << ">" <<std::endl;
            //SymbolEntry *e = i->lookup(c);
            i->printScope(scopecounter);
            std::cout << std::string(scopecounter*4, ' ') << "</Scope " << scopecounter << ">" <<std::endl;
            scopecounter++;
        }
    }
private:
    std::vector<Scope> scopes;
};

extern std::map<std::string, llvm::Function*> GLOBAL_FUNCTIONS;

extern SymbolTable st;

#endif