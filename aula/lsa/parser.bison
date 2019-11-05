%define api.value.type { double } 
%start prog;

%token NUMBER_TOKEN  // Numbers 

%token MULT_TOKEN // *
%token DIV_TOKEN // /
%token SUB_TOKEN // -
%token ADD_TOKEN // +
%token RESULT_TOKEN // ;

%left RESULT_TOKEN
%left ADD_TOKEN SUB_TOKEN
%left MULT_TOKEN DIV_TOKEN

%{
#include <stdio.h>
#include "common.h"
%}

%%
prog: /* empty */
    | prog result { printf("Result: %f\n", $2); }
    ;

expr: NUMBER_TOKEN { $$ = $1; }
   | expr ADD_TOKEN expr { $$ = $1 + $3; }
   | expr SUB_TOKEN expr { $$ = $1 - $3; }
   | expr MULT_TOKEN expr { $$ = $1 * $3; }
   | expr DIV_TOKEN expr { $$ = $1 / $3; }
   ;

result: expr RESULT_TOKEN { $$ = $1; }
      ;

%%
#include <stdio.h>

int main(int argc, char **argv)
{
  if (argc > 0) yyin = fopen(argv[1], "r");
  yyparse();
  return 0;
}

