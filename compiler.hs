{-# LANGUAGE ViewPatterns #-}

module Main where
import Data.List
import Scanner
import Parser
import Options.Applicative
import Data.Monoid ((<>))
import Data.Maybe
import Control.Monad

data Flag = Flag
  {
    input_file :: String,
    output_file :: Maybe String,
    print_token_list :: Bool,
    print_parse_tree :: Bool,
    print_instruction_list :: Bool,
    print_mips :: Bool
  }
  deriving Show

argument_parser :: Parser Flag
argument_parser = Flag
  <$>  argument str
  -- https://hackage.haskell.org/package/optparse-applicative-0.15.1.0/docs/Options-Applicative.html#v:str
  ( metavar "<file>"
    <> help "Input file" )
  <*> (optional $ strOption
  ( metavar "<file>"
    <> long "output-file"
    <> short 'o'
    <> help "Output file" ))
  <*> switch
  ( long "print-token-list"
    <> short 't'
    <> help "Print the scanner's token list" )
  <*> switch
  ( long "print-parse-tree"
    <> short 'p'
    <> help "Print the parser's tree" )
  <*> switch
  ( long "print-instruction-list"
    <> short 'i'
    <> help "Print the compiler's intermediate code" )
  <*> switch
  ( long "print-mips"
    <> short 'm'
    <> help "Print the compiler's MIPS code" )

splash_screen :: String
splash_screen = "Russel! - A compiler for a subset of Rust to MIPS written in Haskell\n \
                \ |- Version 0.1.0\n \
                \ |- By Diogo Cordeiro and Hugo Sales (DCC-FCUP)\n\n"

-- Registers

newtype Reg = Reg Integer

instance Show Reg where
  show (Reg i) = "t" ++ (show i)

instance Num Reg where
  (Reg l) + (Reg r)             = Reg $ l + r
  (Reg l) - (Reg r) | l >= r    = Reg $ l - r
                    | otherwise = undefined
  fromInteger i                 = (Reg i)
  (*)                           = undefined
  abs                           = undefined
  signum                        = undefined

reg0 = Reg 0
reg1 = Reg 1

nextR :: Reg -> Reg
nextR r = r + 1

-- Atoms

data Atom = AVar String
          | AReg Reg
          | ANumber Int

instance Show Atom where
  show (AVar s) = s
  show (AReg r) = show r
  show (ANumber i) = show i

relocate_atom :: Reg -> Atom -> Atom
relocate_atom l (AReg r) = AReg $ l + r
relocate_atom _ a = a

-- Operators

data NumOp = Plus | Minus | Div | Mult | Rem

instance Show NumOp where
  show Plus  = "+"
  show Minus = "-"
  show Div   = "/"
  show Mult  = "*"
  show Rem   = "%"

instance Read NumOp where
  readsPrec _ ('+':rest) = [(Plus, rest)]
  readsPrec _ ('-':rest) = [(Minus, rest)]
  readsPrec _ ('*':rest) = [(Mult, rest)]
  readsPrec _ ('/':rest) = [(Div, rest)]
  readsPrec _ ('%':rest) = [(Rem, rest)]
  readsPrec _ _          = error "Invalid character to make an `NumOp`"

data RelOp = Equal | Diff | Lt | Gt | Le | Ge

instance Show RelOp where
  show Equal  = "=="
  show Diff   = "!="
  show Le     = "<="
  show Ge     = ">="
  show Lt     = "<"
  show Gt     = ">"

instance Read RelOp where
  readsPrec _ (c:'=':rest) | c == '<' = [(Le, rest)]
                           | c == '>' = [(Ge, rest)]
                           | c == '=' = [(Equal, rest)]
                           | c == '!' = [(Diff, rest)]
  readsPrec _ ('<':rest)              = [(Lt, rest)]
  readsPrec _ ('>':rest)              = [(Gt, rest)]
  readsPrec _ _                       = error "Invalid character to make an `RelOp`"

-- Label

data Label = Label Integer

label0 = Label 0
label1 = Label 1

instance Show Label where
  show (Label l) = "label" ++ (show l)

instance Num Label where
  (Label l) + (Label r) = Label $ l + r
  (Label l) - (Label r)
    | l >= r            = Label $ l - r
    | otherwise         = undefined
  fromInteger i         = (Label i)
  (*)                   = undefined
  abs                   = undefined
  signum                = undefined

nextL :: Label -> Label
nextL l = l + 1

-- Instructions

data Instruction = Unary Reg Atom
                 | Binary Reg Atom NumOp Atom
                 | Goto Label
                 | MkLabel Label
                 | If Atom RelOp Atom Label (Maybe Label)

instance Show Instruction where
  show (Unary r a)          = "  " ++ (show r) ++ ":= " ++ (show a) ++ ";"
  show (Binary r al op ar)  = "  " ++ (show r) ++ ":= " ++ (show al) ++ " " ++ (show op) ++ " " ++ (show ar) ++ ";"
  show (Goto l)             = "  goto " ++ show l
  show (MkLabel l)          = show l ++ ":"
  show (If al rel ar lt lf) = "  if " ++ (show al) ++ " " ++ (show rel) ++ " " ++ (show ar) ++ " then\n  " ++ (show (Goto lt)) ++ (maybe "" (\_ -> "\n  else\n  " ++ (show (Goto (fromJust lf)))) lf)

relocate_instruction :: Reg -> Label -> Instruction -> Instruction
relocate_instruction rb _ (Unary r a)             = Unary (rb + r) (relocate_atom rb a)
relocate_instruction rb _ (Binary r al numop ar)  = Binary (rb + r) (relocate_atom rb al) numop (relocate_atom rb ar)
relocate_instruction rb lb (If al relop ar lt lf) = If (relocate_atom rb al) relop (relocate_atom rb ar) (lb + lt) (liftM (+ lb) lf)
relocate_instruction _ _ a                        = a

-- Blocks

type Block = ([Instruction], Reg, Label)

relocate_block :: Reg -> Label -> Block -> Block
relocate_block rb lb (ins, rr, lr) = (map (relocate_instruction rb lb) ins, (rb + rr), (lb + lr))

merge_block :: Block -> Block -> Block
merge_block (insl, rl, ll) blkr = (insl ++ insr, rr, lr) where (insr, rr, lr) = relocate_block (nextR rl) ll blkr

inst_block :: Instruction -> Block
inst_block (Unary reg a)          = ([Unary reg a], reg, label0)
inst_block (Binary reg al op ar)  = ([Binary reg al op ar], reg, label0)
inst_block (Goto l)               = ([Goto l], reg0, label0)
inst_block (MkLabel l)            = ([MkLabel l], reg0, l)
inst_block (If al relop ar lt lf) = ([If al relop ar lt lf], reg0, (fromMaybe lt lf)) -- Use lf if not Nothing, otherwise use lt

resequence :: [Block] -> Block
resequence (blk : blks) = foldl merge_block blk blks

-- Compiling

compile :: [Tree] -> Block
compile t = resequence $ map comp_tree t

comp_tree :: Tree -> Block
comp_tree (FuncDecl name st) = undefined
comp_tree (Statements sts)   = resequence $ map comp_stmt sts

comp_stmt :: Statement -> Block
comp_stmt (Expression exp) = comp_exp exp

comp_exp :: Exp -> Block
comp_exp (LitExp (VTInt i _))  = inst_block $ Unary reg0 (ANumber i)
comp_exp (Var s)               = inst_block $ Unary reg0 (AVar s)
comp_exp (BinaryOp el so er)   = let blkl@(insl, rl, _) = comp_exp el
                                     (insr, rr, lr) = merge_block blkl (comp_exp er) in
                                   (insr ++ [Binary (nextR rr) (AReg rl) (read so :: NumOp) (AReg rr)], (nextR rr), (nextL lr))



main :: IO ()
main = do
  -- Input --
  args <- execParser (info (helper <*> argument_parser) (fullDesc <> header splash_screen))
  raw_input <- readFile $ input_file args

  -- Process --
  let token_list = scan_tokens raw_input
  let parse_tree = parse token_list
  let (compile_result, _, _) = compile parse_tree

  -- Output --
  if (print_token_list args) then
    putStrLn ("Token List:\n" ++ (show token_list) ++ "\n")
  else return ()

  if (print_parse_tree args) then
    putStrLn ("Parse Tree:\n" ++ (printTree parse_tree) ++ "\n")
  else return ()

  if (print_instruction_list args) then
    putStrLn ("Instruction List:\n")
  else return ()

  let compile_output = (unlines $ map show compile_result)
  if (print_mips args) then
    putStrLn ("Compile Result:\n" ++ compile_output)
  else return ()

  let out_file = fromMaybe "out.asm" (output_file args)

  writeFile out_file compile_output

