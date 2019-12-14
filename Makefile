OBJS=src/scanner.hi src/scanner.o src/scanner.info \
     src/parser.hi src/parser.o src/parser.info \
     src/intermediate_rep.hi src/intermediate_rep.o \
     src/compiler.hi src/compiler.o \
     src/mips.hi src/mips.o \
     src/main.hi src/main.o

AUTO_INTERM=src/scanner.hs src/parser.hs
INTERM=$(AUTO_INTERM) src/intermediate_rep.hs src/compiler.hs src/mips.hs src/main.hs

PROGRAM=compiler

all: $(PROGRAM)

src/scanner.hs: src/scanner.x
	alex --info -o src/scanner.hs src/scanner.x

src/parser.hs : src/parser.y
	happy --info -o src/parser.hs src/parser.y


$(PROGRAM): $(INTERM)
	ghc -dynamic -o $(PROGRAM) $(INTERM)
clean:
	rm -f $(PROGRAM) $(AUTO_INTERM) $(OBJS)
