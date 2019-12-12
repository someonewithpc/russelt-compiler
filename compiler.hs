{-# LANGUAGE ViewPatterns #-}

module Main where
import Data.List
import Scanner
import Parser
import Options.Applicative
import Data.Monoid ((<>))

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

from_just :: String -> Maybe b -> b
from_just msg Nothing = error ("ERROR: " ++ msg)
from_just _ (Just x) = x

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

next :: Reg -> Reg
next r = r + 1

-- Atoms

data Atom = AVar String
          | AReg Reg
          | ANumber Int
  deriving Show

relocate_atom :: Reg -> Atom -> Atom
relocate_atom l (AReg r) = AReg $ l + r
relocate_atom _ a = a

-- Operators

data Op = Plus | Minus | Div | Mult | Rem

instance Show Op where
  show Plus  = "+"
  show Minus = "-"
  show Div   = "/"
  show Mult  = "*"
  show Rem   = "%"

instance Read Op where
  readsPrec _ ('+':rest) = [(Plus, rest)]
  readsPrec _ ('-':rest) = [(Minus, rest)]
  readsPrec _ ('*':rest) = [(Mult, rest)]
  readsPrec _ ('/':rest) = [(Div, rest)]
  readsPrec _ ('%':rest) = [(Rem, rest)]
  readsPrec _ _          = error "Invalid characther to make an `Op`"

-- Instructions

data Instruction = Unary Reg Atom
                 | Binary Reg Atom Op Atom

instance Show Instruction where
  show (Unary r a)         = (show r) ++ ":= " ++ (show a)
  show (Binary r al op ar) = (show r) ++ ":= " ++ (show al) ++ " " ++ (show op) ++ " " ++ (show ar)


relocate_instruction :: Reg -> Instruction -> Instruction
relocate_instruction rb (Unary r a)          = Unary (rb + r) (relocate_atom rb a)
relocate_instruction rb (Binary r al op ar)  = Binary (rb + r) (relocate_atom rb al) op (relocate_atom rb ar)

-- Blocks

type Block = ([Instruction], Reg)

relocate_block :: Reg -> Block -> Block
relocate_block rb (ins, rr) = (map (relocate_instruction rb) ins, (rb + rr))

merge_block :: Block -> Block -> Block
merge_block (insl, rl) blkr = (insl ++ insr, rr) where (insr, rr) = relocate_block (next rl) blkr

inst_block :: Instruction -> Block
inst_block (Unary reg a)         = ([Unary reg a], reg)
inst_block (Binary reg al op ar) = ([Binary reg al op ar], reg)

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
comp_exp (BinaryOp el so er)   = let blkl@(insl, rl) = comp_exp el
                                     (insr, rr) = merge_block blkl (comp_exp er) in
                                   (insr ++ [Binary (next rr) (AReg rl) (read so :: Op) (AReg rr)], (next rr))



main :: IO ()
main = do
  args <- execParser (info (helper <*> argument_parser) (fullDesc <> header splash_screen))
  raw_input <- readFile $ input_file args

  let token_list = scan_tokens raw_input
  let parse_tree = parse token_list
  let (compile_result, reg) = compile parse_tree
  putStrLn ("Compile Result:\n" ++ (unlines $ map show compile_result))

  -- putStrLn (show args)
  -- let flag_list = sort (argument_handler(fmap id arg_list))

  -- let quiet_compile = elem Quiet flag_list
  -- if (not quiet_compile) then
  --   putStr splash_screen
  -- else return ()

  -- let print_help_screen = elem Help flag_list
  -- if (print_help_screen) then
  --   putStr help_screen
  -- else return ()

  -- let output_file = get_output_file flag_list
  -- raw_input <- readFile $ from_just "no input files\nTry the '-h' option for basic \
  --                                  \information" $ get_input_file flag_list
  -- let print_token_list = elem PrintTokenList flag_list
  -- if print_token_list then
  --   putStrLn ("Token List:\n" ++ (show token_list) ++ "\n")
  -- else return ()

  -- let print_parse_tree = elem PrintParseTree flag_list
  -- if print_parse_tree then
  --   putStrLn ("Parse Tree:\n" ++ (printTree parse_tree) ++ "\n")
  -- else return ()

  -- if (not quiet_compile) then
  -- else
  --   writeFile output_file (show compile_result)
