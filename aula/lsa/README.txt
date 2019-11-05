============================================================
Introduction to Bison.
============================================================
1. Get started!

a) Check the available files:
   - Makefile : makefile to build the parser.
   - scanner.flex : the lexical analyser (scanner) in flex
   - parser.bison: the parser in bison
       Note: the given scanner & grammar only deal 
             with numbers and the "+" operator.
   - common.c, common.h : common definitions / routines
   - ex1.txt ex2.txt ex3.txt : samples 

b) Compile the parser by typing "make" in the command line.
   The generated executable file is called "parser".

c)  Execute the parser for exemplo1.txt exemplo2.txt exemplo3.txt
    e.g. "./parser exemplo1.txt". 
    You should get errors for all of them.
    Modify these files until the parser reports no errors. 

============================================================
2. You may have noticed during compilation (step 1b) 
that bison reports "1 shift/reduce conflict"
(if you missed it, recompile the parser using "make clean all"). 
The conflict comes from the ambiguity of "expr ADD_TOKEN expr"
(why is it ambiguous?).

To overcome this conflict add 
  %left ADD_TOKEN 
to the parser specification (on top) and re-compile.

This change specifies left associativity for sum expressions
and gets rid of the ambiguity.

============================================================

3. Complete the grammar to also handle the binary operators 
 -, *, and /.

Use %left as in step 2 to ensure there are no grammar conflicts.


============================================================

4. Now we will use semantic actions to associate a "double" 
value to each expression, and print these values.

Proceed in the following steps:

a) In parser.bison add at the top

   %define api.value.type { double } 

  This will set "double" as the type of expressions.

b) Modify scanner.flex to  assign the value of a number to 
the special variable 'yylval' as part of the actions in the 
NUMBER_TOKEN rule. You may use the C library function 'atof' 
to convert the 'yytext' string to a number.

c) In parser.bison encode semantic actions as follows:

   NUMBER_TOKEN { $$ = $1; }
   expr SUM_TOKEN expr { $$ = $1 + $3;}
   ... etc ...

d) Use a semantic action for 'prog' to print the value of each 
expression read from the input
  
e) If the values printed are wrong look at the order of the 
   "%left" directives for the operators. 
   This order dictates the precedence (lowest to highest) for
   the operators.

5. Extensions (outside class):

a) Allow single-line comments begginning with "#".

b) Handle expressions between "(" and ")".

c) Allow variables identified by "a" to "z" in expressions, and allow 
   attributions of the form "var = expr".
   You may assume all variables are initially 0. 
   Suggestion: use an array to hold variable values.

============================================================

