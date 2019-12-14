OBJS=scanner.hi scanner.o scanner.info parser.hi parser.o parser.info intermediate_rep.hi \
intermediate_rep.o compiler.hi compiler.o out.asm mips.hi mips.o main.hi main.o

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
