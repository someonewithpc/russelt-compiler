%{
// HEADERS
#include <stdlib.h>
#include "common.h"
#include "parser.h"

// variables maintained by the lexical analyser
int yyline = 1;
%}

%option noyywrap

%%
[ \t] {  }
\n { yyline++; }
"+" { return ADD_TOKEN; }
"-" { return SUB_TOKEN; }
"*" { return MULT_TOKEN; }
"/" { return DIV_TOKEN; }
";" { return RESULT_TOKEN; }
\-?[0-9][0-9]*(\.[0-9]*)? {
   yylval = atof(yytext);
   return NUMBER_TOKEN;
}
.  { yyerror("unexpected character"); }
%%

