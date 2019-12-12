OBJS=scanner.hi scanner.o scanner.info parser.hi parser.o parser.info compiler.o

INTERM=scanner.hs parser.hs

PROGRAM=compiler

all: $(PROGRAM)

scanner.hs: scanner.x
	alex --info -o scanner.hs scanner.x

parser.hs : parser.y
	happy --info -o parser.hs parser.y


$(PROGRAM): $(INTERM)
	ghc -dynamic -o $(PROGRAM) $(INTERM) compiler.hs
clean:
	rm -f $(PROGRAM) $(INTERM) $(OBJS)
