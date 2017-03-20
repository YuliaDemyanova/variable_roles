CXX := clang++
LLVMCOMPONENTS := cppbackend
#RTTIFLAG := -fno-rtti
RTTIFLAG :=


CXXFLAGS := -I$(LLVM_PATH)/llvm/include -I$(LLVM_PATH)/build/include -I$(LLVM_PATH)/build/tools/clang/include -I$(LLVM_PATH)/llvm/tools/clang/include -Werror -g -D_DEBUG -D__STDC_CONSTANT_MACROS -D__STDC_LIMIT_MACROS -std=c++11 -fno-rtti
#-stdlib=libc++ 

LLVMLDFLAGS := $(shell $(LLVM_PATH)/build/bin/llvm-config --ldflags --libs $(LLVMCOMPONENTS))

export TEST_DIR = $(VARROLES_DIR)"/test"

DDD := $(shell echo $(LLVMLDFLAGS))
SOURCES = $(wildcard *.cpp)

OBJECTS = $(SOURCES:.cpp=.o)
EXES = translator
#rewriter_decl rewriter_stmt 

CLANGLIBS = \
    -lclangFrontend \
    -lclangRewriteFrontend \
    -lclangParse \
    -lclangSema \
    -lclangEdit \
    -lclangAnalysis \
    -lclangAST \
    -lclangLex \
    -lclangBasic \
    -lclangDriver \
    -lclangSerialization \
	-lLLVMMCParser \
	-lLLVMSupport \
	-lLLVMMC \
	-lclangTooling \
	-lLLVMOption \
	-lLLVMBitReader \
	-lLLVMTransformUtils \
	-lclangRewriteCore \
	-lclangARCMigrate


all: $(EXES) 


#	$(CXX) -g -o $@ $^ $(CLANGLIBS) $(LLVMLDFLAGS)

translator: translator.o rewriter_stmt.o IdenticalExprChecker.o
	$(CXX) -g -o $@ $^ $(CLANGLIBS) $(LLVMLDFLAGS)

#$(CLANGLIBS) $(LLVMLDFLAGS)

clean:
	-rm -rf $(EXES) $(OBJECTS)

TEST=test

run: $(EXES)
	./rewriter_decl ${TEST}.c -- > ${TEST}_mod.c
	./rewriter_stmt ${TEST}_mod.c -- > ${TEST}_mod2.c
	./translator ${TEST}_mod2.c -- > ${TEST}_datalog.txt
	./datalog_to_graph.sh ${TEST}_datalog.txt ${TEST}_datalog.png
