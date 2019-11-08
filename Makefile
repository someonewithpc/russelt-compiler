OBJS=scanner.hi scanner.o parser.hi parser.o parser.info scanner.info

INTERM=scanner.hs parser.hs

PROGRAM=parser

all: $(PROGRAM)

scanner.hs: scanner.x
	alex --info -o scanner.hs scanner.x

parser.hs : parser.y
	happy --info -o parser.hs parser.y

$(PROGRAM): $(INTERM)
	ghc -o $(PROGRAM) $(INTERM)
clean:
	rm -f $(PROGRAM) $(INTERM) $(OBJS)
