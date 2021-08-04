.PHONY: clean distclean default

CXX=c++
CXXFLAGS=-Wall -std=c++11

default: tony

lexer.cpp: lexer.l
	flex -s -o lexer.cpp lexer.l

lexer.o: lexer.cpp lexer.hpp parser.hpp

lexer_pure.cpp: lexer_pure.l
	flex -s -o lexer_pure.cpp lexer_pure.l

parser.hpp parser.cpp: parser.y
	bison -dv -o parser.cpp parser.y

parser.o: parser.cpp lexer.hpp

tony: lexer.o parser.o
	$(CXX) $(CXXFLAGS) -o tony lexer.o parser.o type.cpp

lextest: lexer_pure.cpp
	$(CXX) $(CXXFLAGS) -o lextest lexer_pure.cpp

clean:
	$(RM) lexer.cpp lexer_pure.cpp parser.cpp parser.hpp parser.output *.o

distclean: clean
	$(RM) tony lextest
