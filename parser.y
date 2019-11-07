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
  -- Symbols
  ';'                                   { TokenSemi _ }
  ':'                                   { TokenColon _ }
  ','                                   { TokenComma _ }
  '('                                   { TokenLB _ }
  ')'                                   { TokenRB _ }
  '{'                                   { TokenLC _ }
  '}'                                   { TokenRC _ }
    -- Arithmetic Expessions
  '+'                                   { TokenOp _ "+" }
  '-'                                   { TokenOp _ "-" }
  '*'                                   { TokenOp _ "*" }
  '/'                                   { TokenOp _ "/" }
  '%'                                   { TokenOp _ "%" }
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
  fn                                    { TokenFn _ }
    -- Attributions
  let                                   { TokenLet _ }
  '='                                   { TokenAtr _ }
    -- Ifs
  if                                    { TokenIf _ }
  else                                  { TokenElse _ }
    -- While
  while                                 { TokenWhile _ }
    -- Miscelaneous
  id                                    { TokenIdentifier _ $$ }
  builtin                               { TokenBuiltin _ $$ }

%nonassoc '>' '<' '<=' '>=' '==' '!=' '!'
%left '||'
%left '&&'
%left '+' '-'
%left '*' '/' '%'
%left ';' ','
%left '='

%%
Rust : FuncDecl Rust                               { $1 : $2 }
     | {- empty -}                                 { [] }

Statement : Exp ';'                                { Expression $1 }
          | let id '=' Exp ';'                     { Attr $2 $4 }
          | If                                     { $1 }
          | While                                  { $1 }

Statements : Statement Statements                  { $1 : $2 }
           | {- empty -}                           { [] }

Block : '{' Statements '}'                         { $2 }

-- Type : int                                         { VTInt }
--      | bool                                        { VTBool }

-- VarDecl : id ':' Type                              { FOOO }
--         | id                                       { FOO }

-- FuncDeclArgs :  ','

FuncDecl : fn id '(' ')' Block                     { FuncDecl $2 $5 }

FuncArgs1Plus : Exp ',' FuncArgs1Plus              { $1 : $3 }
              | Exp                                { [$1] }
FuncArgs : {- empty -}                             { [] }
         | FuncArgs1Plus                           { $1 }

FuncCall : builtin '(' FuncArgs ')' ';'            { FuncCall $1 $3 }

If : if Exp Block                                  { IfStmt $2 $3 [] }
   | if Exp Block else Block                       { IfStmt $2 $3 $5 }
   | if Exp Block else If                          { IfStmt $2 $3 [$5] }

While : while Exp Block                            { WhileStmt $2 $3 }

Exp
    -- Value Expressions
    : int                                          { LitExp (VTInt $1) }
    | bool                                         { LitExp (VTBool $1) }
    | id                                           { Var $1 }
    -- Arithmetic Operators
    | Exp '+' Exp                                  { BinaryOp $1 "+" $3 }
    | Exp '-' Exp                                  { BinaryOp $1 "-" $3 }
    | Exp '*' Exp                                  { BinaryOp $1 "*" $3 }
    | Exp '/' Exp                                  { BinaryOp $1 "/" $3 }
    | Exp '%' Exp                                  { BinaryOp $1 "%" $3 }
    | '-' Exp                                      { UnaryOp "-" $2 }
    -- Boolean Operators
    | Exp '||' Exp                                 { BinaryOp $1 "||" $3 }
    | Exp '&&' Exp                                 { BinaryOp $1 "&&" $3 }
    | Exp '==' Exp                                 { BinaryOp $1 "==" $3 }
    | Exp '!=' Exp                                 { BinaryOp $1 "!=" $3 }
    | Exp '<' Exp                                  { BinaryOp $1 "<" $3 }
    | Exp '>' Exp                                  { BinaryOp $1 ">" $3 }
    | Exp '<=' Exp                                 { BinaryOp $1 "<=" $3 }
    | Exp '>=' Exp                                 { BinaryOp $1 ">=" $3 }
    | '!' Exp                                      { UnaryOp "!" $2 }
    -- Misc
    | '(' Exp ')'                                  { $2 }
{

data Exp = LitExp ValueType
         | Var String
         | BinaryOp Exp String Exp
         | UnaryOp String Exp
         deriving Show

data Statement = Expression Exp
               | Attr String Exp
               | IfStmt Exp [Statement] [Statement]
               | WhileStmt Exp [Statement]
               | FuncCall String [Exp]
               deriving Show

data Tree = FuncDecl String [Statement]
          | Statements [Statement]
          deriving Show

parseError :: [Token] -> a
parseError (token:tokenList) = let pos = token_pos(token) in
                       error ("Parse error at " ++ show(getLineNum(pos)) ++ ":" ++ show(getColumnNum(pos)) ++ " - " ++ show(token))
parseError _ = error "Parse error"


main :: IO ()
main = do
        raw_input <- getContents
        let token_list = scan_tokens raw_input
        let parse_tree = parse token_list

        putStrLn ("Token List:\n" ++ (show token_list) ++ "\n")
        putStrLn ("Parse Tree:\n" ++ (show parse_tree) ++ "\n")
}
