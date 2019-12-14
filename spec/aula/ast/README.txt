============================================================
Abstract syntax trees (ASTs)
============================================================

Summary: we will work on building ASTs for a language of 
arithmetic expressions involving integers.

We will start with "programs" defined by a single expression
made of numbers and sum (+) operations.  

After parsing, the AST for a program is visited/interpreted 
to print values for expressions.

============================================================
1. Get started: 

a) Check the available files:
   - Makefile : makefile to build the interpreter.
   - scanner.flex : the lexical analyser (scanner) in flex
   - parser.bison: the parser in bison
   - ast.h, ast.c: AST declarations & constructor implementations
   - interpreter.c: the interpreter routines including main
   - example[1234].txt : example files

b) Compile the interpreter by typing "make" in the command line.
   The generated executable file is called "interpreter".

c)  The interpreter recognises a single expression composed 
of single integers or expressions making use
of the '+' operator.  

Execute it for example1.txt and example2.txt, for instance 
type "./interpreter example1.txt". 

============================================================
2. Handle binary operators '-', '*', '/', '%'.
You'll need to:

a) Modify the parser / scanner by defining / recognising 
a new tokens for each operator, and also operator 
associativity and precedence.

b) Add new rules to the grammar for 'expr'.

c) Modify the 'eval' function in 'interpreter.c'.

c) Test with examples that make use of the new
operators, for instance the available 'example3.txt'.

============================================================

3. Modify the language such that the interpreter accepts a 
list of expressions, rather than a single expression.
The following steps are suggested:

a) In ast.h: define the 'ExprList' datatype 
as a single-linked list of expressions and
declare a constructor function for expression lists:

ExprList* ast_exprlist(Expr* expr, ExprList* next);

b) In ast.c: implement the new constructor function.

c) In parser.bison change the grammar by:
  -- modifying the rule for 'program' to
     program: expr_list 
  and define the rules for exprList
  -- associating the 'ExprList' type to 'expr_list'
  -- changing the type of the 'root' variable to 'ExprList'

d) In interpreter.c: modify the main function such that it 
prints the evaluation of each expression in the 'root' list.

You may test your code with "example4.txt".

============================================================
4. Relational operators (to work outside class) 

Handle expressions of the type 'bexpr ? e1 : e2'
where 'bexpr' is a boolean expression, and 'e1' and 'e2' are
arithmetic expressions.

Boolean expressions can either be 'true', 'false', 
or 'expr <relop> expr' where <relop> is one of the 
relational operators '==', '!=', '<', '>', <=' and '>='.

