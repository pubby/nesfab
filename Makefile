.PHONY: all tests deps cleandeps clean run
all: compiler tests
run: compiler
	./compiler
test: tests
	./tests

define compile
@echo -e '\033[32mCXX $@\033[0m'
$(CXX) $(CXXFLAGS) -c -o $@ $<
endef

define deps
@echo -e '\033[32mDEPS $@\033[0m'
$(CXX) $(CXXFLAGS) -MM -MP -MT '\
$(patsubst $(SRCDIR)/%,$(OBJDIR)/%,$(<:.cpp=.o)) \
$(patsubst $(SRCDIR)/%,$(OBJDIR)/%,$(<:.cpp=.d))\
' -o $@ $<
endef

##########################################################################

SRCDIR:=src
OBJDIR:=obj
INCS:=-I$(SRCDIR)

VERSION := "0.1"
GIT_COMMIT := "$(shell git describe --abbrev=8 --dirty --always --tags)"

override CXXFLAGS+= \
  -std=c++2a \
  -O0 \
  -pthread \
  -g \
  -export-dynamic \
  -Wall \
  -Wextra \
  -Wno-unused-parameter \
  -Wno-narrowing \
  -Wno-missing-field-initializers \
  -fmax-errors=3 \
  $(INCS) \
  -DVERSION=\"$(VERSION)\" \
  -DGIT_COMMIT=\"$(GIT_COMMIT)\" 
  #-DNDEBUG

VPATH=$(SRCDIR)

LDLIBS:= -lboost_program_options

SRCS:= \
main.cpp \
parser.cpp \
parser_types.cpp \
symbol_table.cpp \
ir.cpp \
ir_util.cpp \
ir_builder.cpp \
types.cpp \
compiler_error.cpp \
file.cpp \
globals.cpp \
pass1.cpp \
fixed.cpp \
constraints.cpp \
ssa_op.cpp \
lex_tables.cpp \
add_constraints_table.cpp \
graphviz.cpp \
carry.cpp \
o.cpp \
o_phi.cpp \
o_ai.cpp \
o_unused.cpp \
asm.cpp \
locator.cpp \
options.cpp \
stmt.cpp \
cg.cpp \
cg_liveness.cpp \
cg_schedule.cpp \
cg_isel.cpp \
cg_order.cpp \
byteify.cpp

OBJS := $(foreach o,$(SRCS),$(OBJDIR)/$(o:.cpp=.o))
DEPS := $(foreach o,$(SRCS),$(OBJDIR)/$(o:.cpp=.d))

TESTS_SRCS:= \
tests.cpp \
robin_tests.cpp \
fixed.cpp \
fixed_tests.cpp \
constraints.cpp \
constraints_tests.cpp \
carry.cpp \
ssa_op.cpp \
add_constraints_table.cpp

TESTS_OBJS := $(foreach o,$(TESTS_SRCS),$(OBJDIR)/$(o:.cpp=.o))
TESTS_DEPS := $(foreach o,$(TESTS_SRCS),$(OBJDIR)/$(o:.cpp=.d))

compiler: $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LDLIBS) 
	echo 'LINK'
tests: $(TESTS_OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LDLIBS) 
	echo 'LINK'
$(OBJDIR)/%.o: $(SRCDIR)/%.cpp
	$(compile)
$(OBJDIR)/%.d: $(SRCDIR)/%.cpp
	$(deps)

# Lexer

$(SRCDIR)/lex_tables.hpp: lexer_gen
	./lexer_gen
	mv lex_tables.hpp $(SRCDIR)/

$(SRCDIR)/lex_tables.cpp: $(SRCDIR)/lex_tables.hpp
	mv lex_tables.cpp $(SRCDIR)/

lexer_gen: $(SRCDIR)/lexer_gen.cpp
	$(CXX) -std=c++17 -O2 -o $@ $^

# Other Tables

$(SRCDIR)/add_constraints_table.cpp: add_constraints_table_gen
	./add_constraints_table_gen > $@

add_constraints_table_gen: $(SRCDIR)/add_constraints_table_gen.cpp
	$(CXX) $(CXXFLAGS) -o $@ $^

# Instructions

# TODO

#$(SRCDIR)/asm_tables.hpp: asm_gen $(SRCDIR)/asm.txt
	#cat $(SRCDIR)/asm.txt | ./asm_gen > $@

#asm_gen: $(SRCDIR)/asm_gen.cpp $(SRCDIR)/addr_mode.inc $(SRCDIR)/asm_decl.hpp
	#$(CXX) $(CXXFLAGS) -o $@ $<


-include $(DEPS)
-include $(TESTS_DEPS)

##########################################################################	

deps: $(DEPS)
	@echo 'dependencies created'

cleandeps:
	rm -f $(wildcard $(OBJDIR)/*.d)

clean: cleandeps
	rm -f $(wildcard $(OBJDIR)/*.o)
	rm -f compiler

