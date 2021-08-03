#ifndef __SYMBOL_HPP___
#define __SYMBOL_HPP___
#pragma once

#include <vector>
#include <map>
#include <iostream>
#include "type.hpp"

void yyerror(const char *msg);

struct SymbolEntry {
    Type* type;
    SymbolEntry() {}
    SymbolEntry(Type* t) : type(t) {};
};

class Scope {
public:
    Scope() : locals(), size(0) {}
    SymbolEntry *lookup(std::string c) {
        if (locals.find(c) == locals.end()) return nullptr;
        return &locals[c];
        
    }
    void insert(std::string c, Type* t){
        if (locals.find(c) != locals.end()) ;//yyerror("Variable already declared");
        locals[c] = SymbolEntry(t);
        ++size;
    }
    int getSize() const { return size; }

    //For testing
    void printScope() { 
        for (auto i = locals.begin(); i != locals.end(); ++i){
            std::cout << i->first << " " << i->second.type << std::endl;
        }  
    }
private:
    std::map<std::string, SymbolEntry> locals;
    int size;
};

class SymbolTable{
public: 
    SymbolEntry *lookup(std::string c) {
        for (auto i = scopes.rbegin(); i!= scopes.rend(); i++){
            SymbolEntry *e = i->lookup(c);
            if (e!= nullptr) return e;
        }
        //yyerror("Variable not found");
        return nullptr;
    }
    void insert(std::string c, Type* t){
        scopes.back().insert(c, t);
    }
    void openScope(){
        scopes.push_back(Scope());
    }
    void closeScope(){
        scopes.pop_back();
    }

    int getSizeOfCurrentScope() const {
        return scopes.back().getSize();
    }

    //For testing
    void printSymbolTable(){
        std::cout << "Printing symbol table" << std::endl;
        int scopecounter=0;
        for (auto i = scopes.rbegin(); i!= scopes.rend(); i++){
            
            std::cout << "<Scope " << scopecounter << ">" <<std::endl;
            //SymbolEntry *e = i->lookup(c);
            i->printScope();
            std::cout << "</Scope " << scopecounter << ">" <<std::endl;
            scopecounter++;
        }

    }
private:
    std::vector<Scope> scopes;
};

extern SymbolTable st;

#endif