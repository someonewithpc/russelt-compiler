{
module Main where
import Lexer
import Text.Printf
import Data.List
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
    -- Languages constructors
  println                               { TokenPrintln _ $$ }
  read_line                             { TokenReadline _ }

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

FuncDecl : fn id '(' ')' Block                     { FuncDecl $2 $5 }

FuncArgs1 : {- empty -}                            { [] }
          | Exp                                    { [$1] }

FuncArgs : {- empty -}                             { [] }
         | FuncArgs1 FuncArgs                      { $1 : $2 }

BuiltinCall : println '(' FuncArgs ')' ';'         { BuiltinCall "println" $3 }
            | read_line '(' ')' ';'                { BuiltinCall "read_line" [] }

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
               | BuiltinCall String [Exp]

instance Show Statement where
  show (Expression exp) = show exp ++ "\n"
  show (Attr string exp) = "Atributtion to " ++ string ++ " of " ++ show exp ++ "\n"
  show (IfStmt exp stmt1 stmt2) = "If statement with condition: " ++ show exp ++
                                  " and the following statements: \n" ++
                                  (intercalate "\n" (map show stmt1)) ++ "\n" ++
                                  "else contents: \n" ++
                                  (intercalate "\n" (map show stmt2)) ++ "\n"
  show (WhileStmt exp stmt) = "While statement with condition:" ++ show exp ++
                              " and the following statements: \n" ++
                              (intercalate "\n" (map show stmt)) ++ "\n"
  show (BuiltinCall func_name exp) = "Invoked the function: " ++ func_name ++ "\n" ++
                                     " with the following arguments: " ++
                                     (intercalate "\n" (map show exp)) ++ "\n"

data Tree = FuncDecl String [Statement]
          | Statements [Statement]

instance Show Tree where
  show (FuncDecl name statements) = "Function " ++ name ++ ":\n" ++ (intercalate "\n" (map show statements))
  show (Statements statements) = intercalate "\n" (map show statements)

parseError :: [Token] -> a
parseError (token:tokenList) = let pos = token_pos(token) in
                       error ("Parse error at " ++ show(getLineNum(pos)) ++ ":" ++ show(getColumnNum(pos)) ++ " - " ++ show(token))
parseError _ = error "Parse error"


main :: IO ()
main = do
        raw_input <- getContents
        let token_list = scan_tokens raw_input
        let parse_tree = parse token_list

--        putStrLn ("Token List:\n" ++ (show token_list) ++ "\n")
        putStrLn ("Parse Tree:\n" ++ (show parse_tree) ++ "\n")
}
