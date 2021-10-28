.PHONY: clean distclean default

LLVMCONFIG=llvm-config-10

CXX=clang++-10
CXXFLAGS=-Wall -Wno-unused-function -Wno-unneeded-internal-declaration -std=c++11 -g `$(LLVMCONFIG) --cxxflags` 
LDFLAGS=`$(LLVMCONFIG) --ldflags --system-libs --libs all`

default: tonyc

lexer/lexer.cpp: lexer/lexer.l
	flex -s -o lexer/lexer.cpp lexer/lexer.l

lexer/lexer.o: lexer/lexer.cpp lexer/lexer.hpp parser/parser.hpp error/error.hpp

parser/parser.hpp parser/parser.cpp: parser/parser.y
	bison -dv -o parser/parser.cpp parser/parser.y

parser/parser.o: parser/parser.cpp lexer/lexer.hpp error/error.hpp

tonyc: lexer/lexer.o parser/parser.o llvm/ast.o symbol/type.o semantic/semantic.o error/error.o parser/parsing.o
	$(CXX) $(CXXFLAGS) -o tonyc $^ $(LDFLAGS)

clean:
	$(RM) lexer/lexer.cpp lexer/lexer.o 
	$(RM) parser/parser.cpp parser/parser.hpp parser/parser.output parser.o  
	$(RM) *.o *.hpp.gch type/type.o semantic/semantic.o parser/parsing.o error/error.o llvm/ast.o

distclean: clean
	$(RM) tonyc

# This, additionally, cleans the ".asm", ".imm", ".ll", ".o" that are produced
# when running a test program inside the Tony-Compiler directory.
cleanall: distclean
	find . -regex '.*\.\(asm\|imm\|ll\|o\|out\)' -type f -delete
