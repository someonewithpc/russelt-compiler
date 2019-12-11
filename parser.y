{
module Parser where
import Lexer
import Text.Printf
import Data.List
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  -- Literals and types
  int                                   { TokenInt _ $$ }
  bool                                  { TokenBool _ $$ }
  tint                                  { TokenTInt _ $$ }
  tbool                                 { TokenTBool _ $$ }
  -- Symbols
  ';'                                   { TokenSemi _ }
  ':'                                   { TokenColon _ }
--  ','                                   { TokenComma _ }
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
    -- Languages constructs
  println                               { TokenPrintln _ }
  read_line                             { TokenReadline _ }

%nonassoc '>' '<' '<=' '>=' '==' '!=' '!'
%left '||'
%left '&&'
%left '+' '-'
%left '*' '/' '%'
%left ';' ','
%left '='

%%
-- Rust : FuncDecl Rust                               { $1 : $2 }
--      | {- empty -}                                 { [] }

Rust : Exp                                         { [Expression $1] }

Statement : ExpSemi                                { Expression $1 }
          | let id '=' ExpSemi                     { VarDecl $2 $4 Tauto }
          | let id ':' Type '=' ExpSemi            { VarDecl $2 $6 $4 }
          | While                                  { $1 }
          | Println ';'                            { $1 }


Statements : Statement Statements                  { $1 : $2 }
           | {- empty -}                           { [] }
           | ';' Statements                        { $2 }

Block : '{' Statements '}'                         { $2 }

Type : tint                                        { $1 }
     | tbool                                       { $1 }

FuncDecl : fn id '(' ')' Block                     { FuncDecl $2 $5 }

Println : println '(' Exp ')'                      { Println $3 }
Readline : read_line '(' ')'                       { Readline }

If : if Exp IfBlock                                { IfStmt $2 $3 [] }
   | if Exp IfBlock else IfBlock                   { IfStmt $2 $3 $5 }
   | if Exp IfBlock else If                        { IfStmt $2 $3 [Expression $5] }

IfBlock : '{' Statements '}'                       { $2 }
        | '{' Exp '}'                              { [Expression $2] }

While : while Exp Block                            { WhileStmt $2 $3 }

ExpSemi : Exp ';'                                  { $1 }
        | If                                       { $1 }

Exp
    -- Value Expressions
    : int                                          { LitExp (VTInt $1 Ti32) }
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
    | Readline                                     { $1 }
{

-- data FuncCall = Println Exp
--               | Readline
--               | Function String [Exp]

data Exp = LitExp ValueType
         | Var String
         | BinaryOp Exp String Exp
         | UnaryOp String Exp
         | IfStmt Exp [Statement] [Statement]
         | Readline

data Statement = Expression Exp
               | VarDecl String Exp Type
               | WhileStmt Exp [Statement]
               | Println Exp

data Tree = FuncDecl String [Statement]
          | Statements [Statement]

-- Util
wrapped s = "[\n" ++ s ++ "]"
wrapped' s = "[" ++ s ++ "]"
padded s = unlines $ map ((++) $ replicate 2 ' ') (lines s)
padded_print l = wrapped $ intercalate "\n" $ map (padded . print_tree) l

-- Parse Tree Printing
class Print a where print_tree :: a -> [Char]

instance Print Tree where
  print_tree (FuncDecl name statements) = "define function " ++ name ++ ": " ++ (padded_print statements)
  print_tree (Statements statements)    = padded_print statements

instance Print Statement where
  print_tree (Expression exp)           = print_tree exp
  print_tree (VarDecl string exp typ)   = "set " ++ wrapped' (show string ++ ": " ++ (show typ)) ++ " to "
                                              ++ print_tree exp
  print_tree (WhileStmt exp stmt)       = "while cond:" ++ print_tree exp ++
                                              "\n" ++ (padded_print stmt)
  print_tree (Println exp)              = "call println " ++ (print_tree exp)

instance Print Exp where
  print_tree (LitExp vt)                = print_tree vt
  print_tree (Var str)                  = "variable " ++ (show str)
  print_tree (BinaryOp exp1 str exp2)   = (print_tree exp1) ++ " " ++ str ++ " " ++ (print_tree exp2)
  print_tree (UnaryOp str exp)          = str ++ (print_tree exp)
  print_tree (IfStmt exp stmt [])       = "if cond: [" ++ print_tree exp ++ "] " ++ (padded_print stmt)
  print_tree (IfStmt exp stmt1 stmt2)   = "if cond: [" ++ print_tree exp ++ "] " ++ (padded_print stmt1) ++
                                              " else: " ++ (padded_print stmt2)
  print_tree (Readline)                 = "call read_line"

instance Print ValueType where
  print_tree (VTInt i t)                = wrapped' $ show i ++ ": " ++ (tail (show t))
  print_tree (VTBool b)                 = wrapped' $ show b ++ ": bool"
  print_tree _                          = undefined -- "UNDEFINED"

printTree :: [Statement] -> String
printTree = padded_print

parseError :: [Token] -> a
parseError (token:tokenList) = let pos = token_pos(token) in
                       error ("Parse error at " ++ show(getLineNum(pos)) ++ ":"
                              ++ show(getColumnNum(pos)) ++ " - " ++ show(token))
parseError _ = error "Parse error"

}
