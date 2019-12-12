{-# LANGUAGE ViewPatterns #-}

module Main where
import Data.List
import Scanner
import Parser
import Options.Applicative
import Data.Monoid ((<>))

data Flag = Flag
  {
    display_help :: Bool,
    input_file :: String,
    output_file :: Maybe String,
    print_token_list :: Bool,
    print_parse_tree :: Bool,
    print_instruction_list :: Bool,
    print_mips :: Bool,
    no_splash :: Bool
  }
  deriving Show

argument_parser :: Parser Flag
argument_parser = Flag
  <$> switch
  ( long "help"
    <> short 'h'
    <> help "Display this help" )
  <*> argument str
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
  <*> switch
  ( long "no-splash"
    <> short 's'
    <> help "Silence the splash screen" )

splash_screen :: String
splash_screen = "Russel! - A compiler for a subset of Rust to MIPS written in Haskell\n \
                \ |- Version 0.1.0\n \
                \ |- By Diogo Cordeiro and Hugo Sales (DCC-FCUP)\n\n"

from_just :: String -> Maybe b -> b
from_just msg Nothing = error ("ERROR: " ++ msg)
from_just _ (Just x) = x


-- get_output_file :: [Flag] -> String
-- get_output_file [] = "out.asm"
-- get_output_file (OutputFile s:xs) = s ++ ".asm"
-- get_output_file (_:xs) = get_output_file xs

-- get_input_file :: [Flag] -> Maybe String
-- get_input_file [] = Nothing
-- get_input_file (InputFile s:xs) = Just s
-- get_input_file (_:xs) = get_input_file xs

newtype Reg = Reg Int

instance Show Reg where
  show (Reg i) = "t" ++ (show i)

data Atom = AVar String
          | AReg Reg
          | ANumber Int
  deriving Show

data Op = Plus | Minus | Div | Mult | Rem

instance Show Op where
  show Plus  = "+"
  show Minus = "-"
  show Div   = "/"
  show Mult  = "*"
  show Rem   = "%"

instance Read Op where
  readsPrec _ ('+':rest) = [(Plus, rest)]
  -- read "-" = Minus
  -- read "*" = Mult
  -- read "/" = Div
  -- read "%" = Rem
  -- read _   = undefined

data Instruction = Unary Reg Atom
                 | Binary Reg Atom Op Atom

instance Show Instruction where
  show (Unary r a)         = (show r) ++ ":= " ++ (show a)
  show (Binary r al op ar) = (show r) ++ ":= " ++ (show al) ++ " " ++ (show op) ++ " " ++ (show ar)

type Block = ([Instruction], Reg)

next :: Reg -> Reg
next (Reg r) = (Reg (r + 1))

relocate_reg (Reg b) (Reg r) = (Reg (b + r))

relocate_atom (Reg b) (AReg (Reg r)) = (AReg (Reg (b + r)))
relocate_atom _ a = a

relocate :: Reg -> Instruction -> Instruction
relocate rb (Unary r a)          = Unary (relocate_reg rb r) (relocate_atom rb a)
relocate rb (Binary r al op ar)  = Binary (relocate_reg rb r) (relocate_atom rb al) op (relocate_atom rb ar)

rebase :: Reg -> Block -> Block
rebase rb (ins, rr) = (map (relocate rb) ins, (relocate_reg rb rr))

merge_block :: Block -> Block -> Block
merge_block (insl, rl) blkr = let (insr, rr) = rebase (next rl) blkr in
                                (insl ++ insr, rr)

resequence :: [Block] -> Block
resequence (blk : blks) = foldl merge_block blk blks

compile :: [Tree] -> Block
compile t = resequence $ map comp_tree t

comp_tree :: Tree -> Block
comp_tree (FuncDecl name st) = undefined
comp_tree (Statements sts)   = resequence $ map comp_stmt sts

comp_stmt :: Statement -> Block
comp_stmt (Expression exp) = comp_exp exp

comp_exp :: Exp -> Block
comp_exp (LitExp (VTInt i _))  = ([Unary (Reg 0) (ANumber i)], (Reg 0))
comp_exp (Var s)               = ([Unary (Reg 0) (AVar s)], (Reg 0))
comp_exp (BinaryOp el so er)   = let (insl, rl) = comp_exp el
                                     (insr, rr) = rebase rl (comp_exp er)  in
                                   (insl ++ insr ++ [Binary (next rr) (AReg rl) (read so :: Op) (AReg rr)], (next rr))

main :: IO ()
main = do
  args <- execParser (info (helper <*> argument_parser) (fullDesc <> header splash_screen))
  putStrLn (show args)
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

  -- let token_list = scan_tokens raw_input
  -- let parse_tree = parse token_list
  -- let (compile_result, reg) = compile parse_tree

  -- let print_token_list = elem PrintTokenList flag_list
  -- if print_token_list then
  --   putStrLn ("Token List:\n" ++ (show token_list) ++ "\n")
  -- else return ()

  -- let print_parse_tree = elem PrintParseTree flag_list
  -- if print_parse_tree then
  --   putStrLn ("Parse Tree:\n" ++ (printTree parse_tree) ++ "\n")
  -- else return ()

  -- if (not quiet_compile) then
  --   putStrLn ("Compile Result:\n" ++ (unlines $ map show compile_result))
  -- else
  --   writeFile output_file (show compile_result)
