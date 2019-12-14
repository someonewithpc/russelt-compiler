OBJS=scanner.hi scanner.o scanner.info parser.hi parser.o parser.info intermediate_rep.hi intermediate_rep.o compiler.hi compiler.o out.asm

AUTO_INTERM=scanner.hs parser.hs
INTERM=$(AUTO_INTERM) intermediate_rep.hs compiler.hs

PROGRAM=compiler

all: $(PROGRAM)

scanner.hs: scanner.x
	alex --info -o scanner.hs scanner.x

parser.hs : parser.y
	happy --info -o parser.hs parser.y


$(PROGRAM): $(INTERM)
	ghc -dynamic -o $(PROGRAM) $(INTERM)
clean:
	rm -f $(PROGRAM) $(AUTO_INTERM) $(OBJS)
