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
--  float                                 { TokenFloat _ $$ }
--  bool                                  { TokenBool _ $$ }
--  var                                   { TokenVar _ $$ }
    -- Arithmetic Expressions
  '+'                                   { TokenOp _ "+" }
  '-'                                   { TokenOp _ "-" }
  '*'                                   { TokenOp _ "*" }
  '/'                                   { TokenOp _ "/" }
  '%'                                   { TokenOp _ "%" }
  '('                                   { TokenLB _ }
  ')'                                   { TokenRB _ }
  '{'                                   { TokenLC _ }
  '}'                                   { TokenRC _ }
--  '['                                   { TokenLP _ }
--  ']'                                   { TokenRP _ }
    -- Boolean Expressions
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
  return                                { TokenReturn _ }
    -- Types
--  tint                                  { TokenTInt _ }
--  tfloat                                { TokenTFloat _ }
--  tbool                                 { TokenTBool _ }

    -- Attributions
--  '='                                   { TokenAtr _ }
    -- Ifs
--  if                                    { TokenIf _ }
--  elseif                                { TokenElseIf _ }
--  else                                  { TokenElse _ }
--    -- While
--  while                                 { TokenWhile _ }
--    -- Miscelaneous
--  ','                                   { TokenComma _ }
--  new                                   { TokenNew _ }
--  end                                   { TokenEnd _ }
  ';'                                   { TokenSep _ }
  nl                                    { TokenNL _ }

%nonassoc '>' '<' '<=' '>=' '==' '!=' '!'
%left '||'
%left '&&'
%left '+' '-'
%left '*' '/' '%'
--%left sign
%left lc ';' --','

%%
Main : fn main '(' ')' '{'
            Statements
       '}' { $6 }

Statements : Statement nl Statements       { $1 : $3 }
           | {- empty -}                      { [] }

Statement : Expr ';'                          { $1 }

--Type    : tint                                     { TInt }
--        | tfloat                                   { TFloat }
--        | tbool                                    { TBool }

--Const   : int                                      { EConst (VTInt $1) }
--        | float                                    { EConst (VTFloat $1) }
--        | bool                                     { EConst (VTBool $1) }

Expr
    : int                                  { EConst (VTInt $1) }
--    | float                                { $1 }
    | Expr '+' Expr                        { BiOperation $1 "+" $3 }
    | Expr '-' Expr                        { BiOperation $1 "-" $3 }
    | Expr '*' Expr                        { BiOperation $1 "*" $3 }
    | Expr '/' Expr                        { BiOperation $1 "/" $3 }
    | Expr '%' Expr                        { BiOperation $1 "%" $3 }
    | Expr '||' Expr                       { BiOperation $1 "||" $3 }
    | Expr '&&' Expr                       { BiOperation $1 "&&" $3 }
    | Expr '==' Expr                       { BiOperation $1 "==" $3 }
    | Expr '!=' Expr                       { BiOperation $1 "!=" $3 }
    | Expr '<' Expr                        { BiOperation $1 "<" $3 }
    | Expr '>' Expr                        { BiOperation $1 ">" $3 }
    | Expr '<=' Expr                       { BiOperation $1 "<=" $3 }
    | Expr '>=' Expr                       { BiOperation $1 ">=" $3 }
    | '-' Expr                             { UnOperation "-" $2 }
--    | Const                                { $1 }

{

--data Type = TInt | TFloat | TBool | TPointer Type  deriving (Show, Eq)
data ValueType = VTInt Int
               | VTFloat Float -- | VTBool Bool | VTPointer Int
                 deriving (Show)

data Expr = EConst ValueType
          | BiOperation Expr String Expr
          | UnOperation String Expr
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
