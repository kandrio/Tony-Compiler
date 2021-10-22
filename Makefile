.PHONY: clean distclean default

LLVMCONFIG=llvm-config-10

CXX=clang++-10
CXXFLAGS=-Wall -std=c++11 -g `$(LLVMCONFIG) --cxxflags`
LDFLAGS=`$(LLVMCONFIG) --ldflags --system-libs --libs all`

default: tony

lexer.cpp: lexer.l
	flex -s -o lexer.cpp lexer.l

lexer.o: lexer.cpp lexer.hpp parser.hpp error.hpp

parser.hpp parser.cpp: parser.y
	bison -dv -o parser.cpp parser.y

parser.o: parser.cpp lexer.hpp error.hpp

tony: lexer.o parser.o ast.o semantic.o error.o
	$(CXX) $(CXXFLAGS) -o tony type.cpp $^ $(LDFLAGS)

clean:
	$(RM) lexer.cpp parser.cpp parser.hpp parser.output *.o *.ll tony lextest *.hpp.gch

distclean: clean
	$(RM) tony lextest
