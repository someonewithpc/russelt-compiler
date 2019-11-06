{
module Main where
import Lexer
import Text.Printf
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  int                                   { TokenInt _ $$ }
  bool                                  { TokenBool _ $$ }
  var                                   { TokenVar _ $$ }
    -- Arithmetic Expessions
  '+'                                   { TokenOp _ "+" }
  '-'                                   { TokenOp _ "-" }
  '*'                                   { TokenOp _ "*" }
  '/'                                   { TokenOp _ "/" }
  '%'                                   { TokenOp _ "%" }
  '('                                   { TokenLB _ }
  ')'                                   { TokenRB _ }
  '{'                                   { TokenLC _ }
  '}'                                   { TokenRC _ }
    -- Boolean Expessions
  '!'                                   { TokenOp _ "!" }
  '||'                                  { TokenOp _ "||" }
  '&&'                                  { TokenOp _ "&&" }
    -- Boolean Comparisons
  '=='                                  { TokenOp _ "==" }
  '!='                                  { TokenOp _ "!=" }
  '<'                                   { TokenOp _ "<" }
  '>'                                   { TokenOp _ ">" }
  '<='                                  { TokenOp _ "<=" }
  '>='                                  { TokenOp _ ">=" }
    -- Functions
  main                                  { TokenMain _ }
  fn                                    { TokenFn _ }
    -- Attributions
  let                                   { TokenLet _ }
  '='                                   { TokenAtr _ }
    -- Ifs
--  if                                    { TokenIf _ }
--  else                                  { TokenElse _ }
    -- While
--  while                                 { TokenWhile _ }
    -- Miscelaneous
  ';'                                   { TokenSep _ }

%nonassoc '>' '<' '<=' '>=' '==' '!=' '!'
%left '||'
%left '&&'
%left '+' '-'
%left '*' '/' '%'
--%left sign
%left lc ';' --','
%left '='

%%
Main : fn main '(' ')' '{'
            Statements
       '}' { $6 }

Statements : Statement Statements       { $1 : $2 }
           | {- empty -}                { [] }

Statement : Exp ';'                     { $1 }

Exp
    : int                               { LitExp (VTInt $1) }
    | bool                              { LitExp (VTBool $1) }
    | var                               { LitExp (VTAuto $1) }
    | Exp '+' Exp                       { BiOperation $1 "+" $3 }
    | Exp '-' Exp                       { BiOperation $1 "-" $3 }
    | Exp '*' Exp                       { BiOperation $1 "*" $3 }
    | Exp '/' Exp                       { BiOperation $1 "/" $3 }
    | Exp '%' Exp                       { BiOperation $1 "%" $3 }
    | Exp '||' Exp                      { BiOperation $1 "||" $3 }
    | Exp '&&' Exp                      { BiOperation $1 "&&" $3 }
    | Exp '==' Exp                      { BiOperation $1 "==" $3 }
    | Exp '!=' Exp                      { BiOperation $1 "!=" $3 }
    | Exp '<' Exp                       { BiOperation $1 "<" $3 }
    | Exp '>' Exp                       { BiOperation $1 ">" $3 }
    | Exp '<=' Exp                      { BiOperation $1 "<=" $3 }
    | Exp '>=' Exp                      { BiOperation $1 ">=" $3 }
    | '-' Exp                           { UnOperation "-" $2 }

{
data Exp = LitExp ValueType
          | BiOperation Exp String Exp
          | UnOperation String Exp
          deriving Show

parseError :: [Token] -> a
parseError (token:tokenList) = let pos = token_pos(token) in
                       error ("parse error at line " ++ show(getLineNum(pos)) ++ " and column " ++ show(getColumnNum(pos)))
parseError _ = error "parse error"

main :: IO()
main = do
        s <- getContents
        print (scan_tokens s)
}
