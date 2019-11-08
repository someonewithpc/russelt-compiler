OBJS=scanner.hi scanner.o parser.hi parser.o

INTERM=scanner.hs parser.hs

PROGRAM=parser

all: $(PROGRAM)

scanner.hs: scanner.x
	alex -o scanner.hs scanner.x

parser.hs : parser.y
	happy --info -o parser.hs parser.y

$(PROGRAM): $(INTERM)
	ghc -o $(PROGRAM) $(INTERM)
clean:
	rm -f $(PROGRAM) $(INTERM) $(OBJS)
