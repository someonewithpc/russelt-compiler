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
Rust : FuncDecl Rust                               { $1 : $2 }
     | {- empty -}                                 { [] }

Statement : ExpSemi                                { Expression $1 }
          | let id '=' ExpSemi                     { VarDecl $2 $4 "auto" }
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

If : if Exp Block                                  { IfStmt $2 $3 [] }
   | if Exp Block else Block                       { IfStmt $2 $3 $5 }
   | if Exp Block else If                          { IfStmt $2 $3 [Expression $5] }

While : while Exp Block                            { WhileStmt $2 $3 }

ExpSemi : Exp ';'                                  { $1 }
        | If                                       { $1 }

Exp
    -- Value Expressions
    : int                                          { LitExp (TInt $1) }
    | bool                                         { LitExp (TBool $1) }
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

data Exp = LitExp Type
         | Var String
         | BinaryOp Exp String Exp
         | UnaryOp String Exp
         | IfStmt Exp [Statement] [Statement]
         | Readline

data Statement = Expression Exp
               | VarDecl String Exp String
               | WhileStmt Exp [Statement]
               | Println Exp

data Tree = FuncDecl String [Statement]
          | Statements [Statement]

-- Util
wrapped s = "[\n" ++ s ++ "]"
wrapped' s = "[" ++ s ++ "]"
padded lvl s = unlines $ map ((++) $ replicate (lvl*2) ' ') (lines s)
padded_print lvl l = wrapped $ intercalate "\n" $ map (padded lvl . print_tree lvl) l

-- Parse Tree Printing
class Print a where print_tree :: Int -> a -> [Char]

instance Print Tree where
  print_tree lvl (FuncDecl name statements) = "define function " ++ name ++ ": " ++ (padded_print lvl statements)
  print_tree lvl (Statements statements)    = padded_print lvl statements

instance Print Statement where
  print_tree lvl (Expression exp)           = print_tree lvl exp
  print_tree lvl (VarDecl string exp typ)   = "set " ++ wrapped' (show string ++ ": " ++ typ) ++ " to "
                                              ++ print_tree lvl exp
  print_tree lvl (WhileStmt exp stmt)       = "while cond:" ++ print_tree lvl exp ++
                                              "\n" ++ (padded_print lvl stmt)
  print_tree lvl (Println exp)              = "call println " ++ (print_tree lvl exp)

instance Print Exp where
  print_tree lvl (LitExp vt)                = print_tree lvl vt
  print_tree lvl (Var str)                  = str
  print_tree lvl (BinaryOp exp1 str exp2)   = (print_tree lvl exp1) ++ " " ++ str ++ " " ++ (print_tree lvl exp2)
  print_tree lvl (UnaryOp str exp)          = str ++ (print_tree lvl exp)
  print_tree lvl (IfStmt exp stmt [])       = "if cond: [" ++ print_tree lvl exp ++ "] " ++ (padded_print lvl stmt)
  print_tree lvl (IfStmt exp stmt1 stmt2)   = "if cond: [" ++ print_tree lvl exp ++ "] " ++ (padded_print lvl stmt1) ++
                                              " else: " ++ (padded_print lvl stmt2)
  print_tree lvl (Readline)                 = "call read_line"

instance Print Type where
  print_tree lvl (TInt i)                  = wrapped' $ show i ++ ": i32"
  print_tree lvl (TBool b)                 = wrapped' $ show b ++ ": bool"
  print_tree _ _                            = "UNDEFINED"

printTree = padded_print 1

parseError :: [Token] -> a
parseError (token:tokenList) = let pos = token_pos(token) in
                       error ("Parse error at " ++ show(getLineNum(pos)) ++ ":"
                              ++ show(getColumnNum(pos)) ++ " - " ++ show(token))
parseError _ = error "Parse error"


main :: IO ()
main = do
        raw_input <- getContents
        let token_list = scan_tokens raw_input
        let parse_tree = parse token_list

        putStrLn ("Token List:\n" ++ (show token_list) ++ "\n")
        putStrLn ("Parse Tree:\n" ++ (printTree parse_tree))
}
