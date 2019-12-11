OBJS=lexer.hi lexer.o parser.hi parser.o parser.info lexer.info compiler.o

INTERM=lexer.hs parser.hs compiler.hs

PROGRAM=compiler

all: $(PROGRAM)

lexer.hs: lexer.x
	alex --info -o lexer.hs lexer.x

parser.hs : parser.y
	happy --info -o parser.hs parser.y


$(PROGRAM): $(INTERM)
	ghc -o $(PROGRAM) $(INTERM)
clean:
	rm -f $(PROGRAM) $(INTERM) $(OBJS)
