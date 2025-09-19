.PHONY: all debug debugstatic release static profile docs tests deps cleandeps clean run tables
debug: nesfab
debugstatic: nesfab
release: nesfab
static: nesfab
profile: nesfab
all: nesfab tests
run: nesfab
	./nesfab
test: tests
	./tests

define compile
@printf '\033[32mCXX $@\033[m\n'
$(CXX) $(CXXFLAGS) -c -o $@ $<
endef

define deps
@printf '\033[32mDEPS $@\033[m\n'
$(CXX) $(CXXFLAGS) -MM -MP -MT '\
$(patsubst $(SRCDIR)/%,$(OBJDIR)/%,$(<:.cpp=.o)) \
$(patsubst $(SRCDIR)/%,$(OBJDIR)/%,$(<:.cpp=.d))\
' -o $@ $<
endef

##########################################################################

SRCDIR:=src
OBJDIR:=obj
INCS:=-I$(SRCDIR)

VERSION := "1.8"
GIT_COMMIT := "$(shell git describe --all --abbrev=8 --dirty --always)"

override CXXFLAGS+= \
  -std=c++20 \
  -pthread \
  -Wall \
  -Wextra \
  -Wno-unused-parameter \
  -Wno-unknown-warning-option \
  -Wno-narrowing \
  -Wno-missing-field-initializers \
  -Wno-missing-braces \
  -Wno-unused-command-line-argument \
  -fmax-errors=3 \
  -ftemplate-depth=100 \
  -pipe \
  $(INCS) \
  -DVERSION=\"$(VERSION)\" \
  -DGIT_COMMIT=\"$(GIT_COMMIT)\"

ifndef ARCH
	ARCH=AMD64
endif

ifeq ($(ARCH),AMD64)
override CXXFLAGS+= \
  -mpopcnt \
  -msse4 \
  -mcx16 \
  -mmovbe
endif

ifeq ($(ARCH),AMD64_OLD)
override CXXFLAGS+= \
  -mpopcnt \
  -msse4
endif

ifeq ($(ARCH),MINGW_CROSS)
override CXX:=x86_64-w64-mingw32-g++
override CXXFLAGS+= \
  -mpopcnt \
  -msse4 \
  -Wa,-mbig-obj
endif

ifeq ($(ISA),LEGAL)
override CXXFLAGS+= -DLEGAL
endif

debug: CXXFLAGS += -O0 -g
debugstatic: CXXFLAGS += -static -O0 -g
release: CXXFLAGS += -O3 -DNDEBUG -Wno-unused-variable
static: CXXFLAGS += -static -O3 -DNDEBUG
profile: CXXFLAGS += -O3 -DNDEBUG -g

ifeq ($(MAKECMDGOALS), all)
CXXFLAGS += -g
endif

VPATH=$(SRCDIR)

LDLIBS:= -lboost_program_options

SRCS:= \
text.cpp \
main.cpp \
parser.cpp \
token.cpp \
symbol_table.cpp \
ir.cpp \
ir_edge.cpp \
ir_util.cpp \
ir_algo.cpp \
type.cpp \
compiler_error.cpp \
file.cpp \
globals.cpp \
pass1.cpp \
constraints.cpp \
ssa_op.cpp \
lex_tables.cpp \
asm_lex_tables.cpp \
ext_lex_tables.cpp \
macro_lex_tables.cpp \
add_constraints_table.cpp \
graphviz.cpp \
carry.cpp \
o_phi.cpp \
o_ai.cpp \
o_unused.cpp \
o_merge_bb.cpp \
asm.cpp \
locator.cpp \
options.cpp \
stmt.cpp \
cg.cpp \
cg_liveness.cpp \
cg_schedule.cpp \
cg_isel.cpp \
byteify.cpp \
asm_proc.cpp \
gmanager.cpp \
span.cpp \
span_allocator.cpp \
group.cpp \
ram_alloc.cpp \
lvar.cpp \
o_motion.cpp \
eval.cpp \
rval.cpp \
type_name.cpp \
cg_isel_cpu.cpp \
convert.cpp \
convert_compress.cpp \
lt.cpp \
o_arg.cpp \
o_id.cpp \
cg_cset.cpp \
rom_alloc.cpp \
mapper.cpp \
rom.cpp \
runtime.cpp \
rom_link.cpp \
ram_init.cpp \
mods.cpp \
rom_prune.cpp \
cg_ptr.cpp \
pbqp.cpp \
asm_graph.cpp \
fn_def.cpp \
ast.cpp \
multi.cpp \
lodepng/lodepng.cpp \
convert_png.cpp \
switch.cpp \
o_loop.cpp \
o_defork.cpp \
unroll_divisor.cpp \
puf.cpp \
worklist.cpp \
mlb.cpp \
macro.cpp \
o_shift.cpp \
mapfab.cpp \
xfab.cpp \
define.cpp \
o_locator.cpp \
ctags.cpp \
donut.cpp \
convert_map.cpp \
rom_dummy.cpp \
o_type.cpp

OBJS := $(foreach o,$(SRCS),$(OBJDIR)/$(o:.cpp=.o))
DEPS := $(foreach o,$(SRCS),$(OBJDIR)/$(o:.cpp=.d))

TESTS_SRCS:= \
tests.cpp \
robin_tests.cpp \
fixed_tests.cpp \
constraints.cpp \
constraints_tests.cpp \
bitset_tests.cpp \
carry.cpp \
ssa_op.cpp \
type_name.cpp \
add_constraints_table.cpp \
catch/catch.cpp

TESTS_OBJS := $(foreach o,$(TESTS_SRCS),$(OBJDIR)/$(o:.cpp=.o))
TESTS_DEPS := $(foreach o,$(TESTS_SRCS),$(OBJDIR)/$(o:.cpp=.d))

nesfab: $(OBJS)
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

LEX_TABLES:= \
$(SRCDIR)/lex_tables.hpp \
$(SRCDIR)/lex_tables.cpp \
$(SRCDIR)/asm_lex_tables.hpp \
$(SRCDIR)/asm_lex_tables.cpp \
$(SRCDIR)/ext_lex_tables.hpp \
$(SRCDIR)/ext_lex_tables.cpp \
$(SRCDIR)/macro_lex_tables.hpp \
$(SRCDIR)/macro_lex_tables.cpp

tables: $(LEX_TABLES)

ifeq ($(MAKECMDGOALS), tables)
lexer_gen: $(SRCDIR)/lexer_gen.cpp $(SRCDIR)/lex_op_name.inc
	$(CXX) -std=c++17 -O1 -o lexer_gen $<

$(LEX_TABLES): lexer_gen $(SRCDIR)/lexer_gen.cpp $(SRCDIR)/lex_op_name.inc
	./lexer_gen 
	mv lex_tables.hpp $(SRCDIR)/ 
	mv lex_tables.cpp $(SRCDIR)/ 
	mv asm_lex_tables.hpp $(SRCDIR)/ 
	mv asm_lex_tables.cpp $(SRCDIR)/ 
	mv ext_lex_tables.hpp $(SRCDIR)/
	mv ext_lex_tables.cpp $(SRCDIR)/
	mv macro_lex_tables.hpp $(SRCDIR)/
	mv macro_lex_tables.cpp $(SRCDIR)/

# Other Tables

$(SRCDIR)/add_constraints_table.cpp: $(SRCDIR)/add_constraints_table_gen.cpp
	$(CXX) -std=c++17 -O1 -o add_constraints_table_gen $<
	./add_constraints_table_gen > $@
endif

##########################################################################	

deps: $(DEPS)
	@echo 'dependencies created'

cleandeps:
	rm -f $(wildcard $(OBJDIR)/*.d)

clean: cleandeps
	rm -f $(wildcard $(OBJDIR)/*.o)
	rm -f $(wildcard $(OBJDIR)/lodepng/*.o)
	rm -f $(wildcard $(OBJDIR)/catch/*.o)
	rm -f nesfab

docs:
	asciidoctor doc/doc.adoc -o doc/doc.html

# Create directories:

$(info $(shell mkdir -p $(OBJDIR)))
$(info $(shell mkdir -p $(OBJDIR)/catch))
$(info $(shell mkdir -p $(OBJDIR)/lodepng))

ifneq ($(MAKECMDGOALS), clean)
ifneq ($(MAKECMDGOALS), tables)
-include $(DEPS)
-include $(TESTS_DEPS)
endif
endif
