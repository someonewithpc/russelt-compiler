#ifndef __common_h__
#define __common_h__

// Scanner declarations
extern int yylex();
extern int yyline;
extern char* yytext;
extern FILE* yyin;

// Common routines
extern void yyerror(const char* err);

#endif
