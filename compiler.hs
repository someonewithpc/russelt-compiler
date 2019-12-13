{-# LANGUAGE ViewPatterns #-}

module Main where
import Data.List
import Scanner
import Parser
import Options.Applicative
import Data.Monoid ((<>))
import Data.Maybe
import Data.Map.Strict as Map (singleton, updateLookupWithKey, (!), Map)
import Control.Monad
import Data.Bifunctor

-- Argument parsing

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

instance Enum Reg where
  toEnum i = Reg (toInteger i)
  fromEnum (Reg i) = fromIntegral i

reg0 = Reg 0
reg1 = Reg 1

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

data Label = LabelI Integer | LabelS String

label0 = LabelI 0
label1 = LabelI 1

instance Show Label where
  show (LabelI l) = "label" ++ (show l)
  show (LabelS s) = s

instance Num Label where
  (LabelI l) + (LabelI r) = LabelI $ l + r
  _ + _                   = error "Cannot add string labels"
  (LabelI l) - (LabelI r)
    | l >= r              = LabelI $ l - r
    | otherwise           = undefined
  _ - _                   = error "Cannot substract string labels"
  fromInteger i           = (LabelI i)
  (*)                     = undefined
  abs                     = undefined
  signum                  = undefined

instance Enum Label where
  toEnum i = LabelI (toInteger i)
  fromEnum (LabelI i) = fromIntegral i
  fromEnum _ = undefined

-- Instructions

type Addr = Int

data Instruction = Unary Reg Atom
                 | Binary Reg Atom NumOp Atom
                 | Goto Label
                 | MkLabel Label
                 | Load Reg Addr
                 | Store Reg Addr
                 | If Atom RelOp Atom Label (Maybe Label)

instance Show Instruction where
  show (Unary r a)          = "  " ++ (show r) ++ ":= " ++ (show a) ++ ";"
  show (Binary r al op ar)  = "  " ++ (show r) ++ ":= " ++ (show al) ++ " " ++ (show op) ++ " " ++ (show ar) ++ ";"
  show (Goto l)             = "  goto " ++ show l
  show (MkLabel l)          = show l ++ ":"
  show (Load r addr)        = "  load " ++ (show r) ++ " (" ++ (show addr) ++ ")"
  show (Store r addr)       = "  store " ++ (show r) ++ " (" ++ (show addr) ++ ")"
  show (If al rel ar lt lf) = "  if " ++ (show al) ++ " " ++ (show rel) ++ " " ++ (show ar)
                              ++ " then\n  " ++ (show (Goto lt)) ++
                              (maybe "" (\jlf -> "\n  else\n  " ++ (show (Goto jlf))) lf)

relocate_instruction :: Reg -> Label -> Instruction -> Instruction
relocate_instruction rb _ (Unary r a)             = Unary (rb + r) (relocate_atom rb a)
relocate_instruction rb _ (Binary r al numop ar)  = Binary (rb + r) (relocate_atom rb al) numop (relocate_atom rb ar)
relocate_instruction rb lb (If al relop ar lt lf) = If (relocate_atom rb al) relop (relocate_atom rb ar)
                                                    (lb + lt) ((+ lb) <$> lf)
relocate_instruction _ _ a                        = a

-- Blocks

type Block = ([Instruction], Reg, Label)

relocate_block :: Reg -> Label -> Block -> Block
relocate_block rb lb (ins, rr, lr) = (map (relocate_instruction rb lb) ins, (rb + rr), (lb + lr))

merge_block :: Block -> Block -> Block
merge_block (insl, rl, ll) blkr = (insl ++ insr, rr, lr) where (insr, rr, lr) = relocate_block (succ rl) ll blkr

inst_block :: Instruction -> Block
inst_block (Unary reg a)          = ([Unary reg a], reg, label0)
inst_block (Binary reg al op ar)  = ([Binary reg al op ar], reg, label0)
inst_block (Goto l)               = ([Goto l], reg0, label0)
inst_block (MkLabel l)            = ([MkLabel l], reg0, l)
inst_block (If al relop ar lt lf) = ([If al relop ar lt lf],
                                     reg0,
                                     (fromMaybe lt lf)) -- Use lf if not Nothing, otherwise use lt

resequence :: [Block] -> Block
resequence (blk : blks) = foldl merge_block blk blks

-- Map

type Vars = Map String Addr
variable_address_index = "__variable_address_index"
variables = singleton variable_address_index 0 :: Vars

-- Compiling

compile :: Vars -> [Tree] -> (Vars, Block)
compile vars t = second resequence $ foldlWithMap vars comp_tree t
-- resequence $ map (comp_tree vars) t

comp_tree :: Vars -> Tree -> (Vars, Block)
comp_tree vars (FuncDecl name sts) = let blk = inst_block (MkLabel (LabelS name)) in
                                       second (resequence . (blk :)) $ foldlWithMap vars comp_stmt sts
comp_tree vars (Statements sts)    = second resequence $ foldlWithMap vars comp_stmt sts

foldlWithMap :: Vars -> (Vars -> a -> (Vars, b)) -> [a] -> (Vars, [b])
foldlWithMap vars f (x:xs) = let (vars', xres) = f vars x in
                               second ((:) xres) (foldlWithMap vars' f xs)
foldlWithMap vars _ []     = (vars, [])

comp_stmt :: Vars -> Statement -> (Vars, Block)
comp_stmt vars (Expression exp)    = comp_exp vars exp
comp_stmt vars (VarDecl s exp _)   = let ((Just addr), vars') =
                                           updateLookupWithKey (\_ -> Just . succ) variable_address_index vars
                                         (vars'', blk@(_, rr, _)) = comp_exp vars' exp in
                                       (vars'', merge_block blk $ inst_block $ Store rr addr)
comp_stmt vars (WhileStmt exp sts) = let (vars', start_blk@(_, exp_res, _)) = second (merge_block (inst_block $ MkLabel label0)) $ comp_exp vars exp
                                         (vars'', body_blk)                 = comp_tree vars' (Statements sts)
                                         end_blk@(_, _, end_label)          = merge_block body_blk (inst_block $ MkLabel label1)
                                         if_blk                             = inst_block (If (AReg exp_res) Equal (ANumber 0) end_label Nothing) in
                                       (,) vars'' $ resequence [start_blk, if_blk, body_blk, end_blk]

comp_exp :: Vars -> Exp -> (Vars, Block)
comp_exp vars (LitExp (VTInt i _)) = (,) vars $ inst_block $ Unary reg0 (ANumber i)
comp_exp vars (Var s)              = (,) vars $ inst_block $ Load reg0 (vars ! s)
comp_exp vars (BinaryOp el so er)  = let (vars', blkl@(insl, rl, _)) = comp_exp vars el
                                         (vars'', blkr)              = comp_exp vars' er
                                         (insr, rr, lr)              = merge_block blkl blkr in
                                       (,) vars'' (insr ++ [Binary (succ rr) (AReg rl) (read so :: NumOp) (AReg rr)],
                                                     (succ rr),
                                                     (succ lr))



main :: IO ()
main = do
  -- Input --
  args <- execParser (info (helper <*> argument_parser) (fullDesc <> header splash_screen))
  raw_input <- readFile $ input_file args

  -- Process --
  let token_list = scan_tokens raw_input
  let parse_tree = parse token_list
  let (_, (compile_result, _, _)) = compile variables parse_tree

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

