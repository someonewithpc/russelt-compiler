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
  var                                   { TokenVar _ $$ }
  '='                                   { TokenAtr _ }
    -- Ifs
  if                                    { TokenIf _ }
  else                                  { TokenElse _ }
    -- While
  while                                 { TokenWhile _ }
    -- Miscelaneous
  ';'                                   { TokenSep _ }

%nonassoc '>' '<' '<=' '>=' '==' '!=' '!'
%left '||'
%left '&&'
%left '+' '-'
%left '*' '/' '%'
%left ';' ','
%left '='

%%
Main : fn main '(' ')' Block { Func "main" $5 }

Statement : Exp ';'                     { Expression $1 }
          | let var '=' Exp ';'         { Attr $2 $4 }
          | If                          { $1 }

Statements : Statement Statements       { $1 : $2 }
           | {- empty -}                { [] }

Block : '{' Statements '}'              { $2 }
      | Statement                       { [$1] }

If : if Exp Block                       { IfStmt $2 $3 [] }
   | if Exp Block else Block            { IfStmt $2 $3 $5 }

While : while Exp Block                 { WhileStmt $2 $3 }

Exp
    : int                               { LitExp (VTInt $1) }
    | bool                              { LitExp (VTBool $1) }
    | var                               { Var $1 }
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
    | '(' Exp ')'                       { $2 }
{

data Exp = LitExp ValueType
         | Var String
         | BiOperation Exp String Exp
         | UnOperation String Exp
         deriving Show

data Statement = Expression Exp
               | Attr String Exp
               | IfStmt Exp [Statement] [Statement]
               | WhileStmt Exp [Statement]
               deriving Show

data Tree = Func String [Statement]
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
