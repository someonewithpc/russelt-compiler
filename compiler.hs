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

-- Utils

foldl_with_map :: Vars -> (Vars -> a -> (Vars, b)) -> [a] -> (Vars, [b])
foldl_with_map vars f (x:xs) = let (vars', xres) = f vars x in
                               second ((:) xres) (foldl_with_map vars' f xs)
foldl_with_map vars _ []     = (vars, [])

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

-- Regs and labels

-- Nothing if both nothing, otherwise convert nothing to zero and sum
maybe_offset :: (Num a) => Maybe a -> Maybe a -> Maybe a
maybe_offset Nothing Nothing = Nothing
maybe_offset b r             = Just $ (fromMaybe 0 b) + (fromMaybe 0 r)

-- Atoms

data Atom = AVar String
          | AReg Reg
          | ANumber Int
          | ABool Bool
          | AString String

instance Show Atom where
  show (AVar s) = s
  show (AReg r) = show r
  show (ANumber i) = show i
  show (AString s) = s

relocate_atom :: Maybe Reg -> Atom -> Atom
relocate_atom (Just l) (AReg r) = AReg $ l + r
relocate_atom _ a = a

-- Operators

data Op = Plus | Minus | Div | Mult | Rem
        | Equal | Diff | Lt | Gt | Le | Ge

instance Show Op where
  show Plus  = "+"
  show Minus = "-"
  show Div   = "/"
  show Mult  = "*"
  show Rem   = "%"
  show Equal  = "=="
  show Diff   = "!="
  show Le     = "<="
  show Ge     = ">="
  show Lt     = "<"
  show Gt     = ">"

instance Read Op where
  readsPrec _ ('+':rest)      = [(Plus, rest)]
  readsPrec _ ('-':rest)      = [(Minus, rest)]
  readsPrec _ ('*':rest)      = [(Mult, rest)]
  readsPrec _ ('/':rest)      = [(Div, rest)]
  readsPrec _ ('%':rest)      = [(Rem, rest)]
  readsPrec _ ('<':'=':rest)  = [(Le, rest)]
  readsPrec _ ('>':'=':rest)  = [(Ge, rest)]
  readsPrec _ ('=':'=':rest)  = [(Equal, rest)]
  readsPrec _ ('!':'=':rest)  = [(Diff, rest)]
  readsPrec _ ('<':rest)      = [(Lt, rest)]
  readsPrec _ ('>':rest)      = [(Gt, rest)]
  readsPrec _ _               = error "Invalid character to make an `Op`"

-- Instructions

type Addr = Int

data Instruction = Unary Reg Atom
                 | Binary Reg Atom Op Atom
                 | Load Reg Addr
                 | Store Reg Addr
                 | Goto Label
                 | MkLabel Label
                 | If Atom Op Atom Label (Maybe Label)
                 | PrintLn Exp

instance Show Instruction where
  show (Unary r a)          = "  " ++ (show r) ++ ":= " ++ (show a) ++ ";"
  show (Binary r al op ar)  = "  " ++ (show r) ++ ":= " ++ (show al) ++ " " ++ (show op) ++ " " ++ (show ar) ++ ";"
  show (Load r addr)        = "  load " ++ (show r) ++ " (" ++ (show addr) ++ ")"
  show (Store r addr)       = "  store " ++ (show r) ++ " (" ++ (show addr) ++ ")"
  show (PrintLn e)          = "  println " ++ ":= " ++ (print_tree e) ++ ";"
  show (Goto l)             = "  goto " ++ show l
  show (MkLabel l)          = show l ++ ":"
  show (If al rel ar lt lf) = "  if " ++ (show al) ++ " " ++ (show rel) ++ " " ++ (show ar)
                              ++ " then\n  " ++ (show (Goto lt)) ++
                              (maybe "" (\jlf -> "\n  else\n  " ++ (show (Goto jlf))) lf)

relocate_instruction :: Maybe Reg -> Maybe Label -> Instruction -> Instruction
relocate_instruction rb _  (Unary r a)            = Unary   (fromJust $ maybe_offset rb $ Just r) (relocate_atom rb a)
relocate_instruction _ _   (PrintLn e)            = PrintLn e
relocate_instruction rb _  (Binary r al numop ar) = Binary  (fromJust $ maybe_offset rb $ Just r) (relocate_atom rb al) numop (relocate_atom rb ar)
relocate_instruction rb _  (Load r a)             = Load    (fromJust $ maybe_offset rb $ Just r) a
relocate_instruction rb _  (Store r a)            = Store   (fromJust $ maybe_offset rb $ Just r) a
relocate_instruction _ lb  (Goto l)               = Goto    (fromJust $ maybe_offset lb $ Just l)
relocate_instruction _ lb  (MkLabel l)            = MkLabel (fromJust $ maybe_offset lb $ Just l)
relocate_instruction rb lb (If al relop ar lt lf) = If      (relocate_atom rb al) relop (relocate_atom rb ar)
                                                            (fromJust $ maybe_offset lb (Just lt)) (maybe_offset lb lf)

-- Blocks

type Blk = ([Instruction], (Maybe Reg), (Maybe Label))

relocate_block :: Maybe Reg -> Maybe Label -> Blk -> Blk
relocate_block rb lb (ins, rr, lr) = (map (relocate_instruction rb lb) ins, (maybe_offset rb rr), (maybe_offset lb lr))

merge_blk :: Blk -> Blk -> Blk
merge_blk (insl, rl, ll) blkr = (insl ++ insr, rr, lr) where (insr, rr, lr) = relocate_block (succ <$> rl) ll blkr

inst_blk :: Instruction -> Blk
inst_blk (Unary reg a)          = ([Unary reg a],         (Just reg), Nothing)
inst_blk (Binary reg al op ar)  = ([Binary reg al op ar], (Just reg), Nothing)
inst_blk (Load reg addr)        = ([Load reg addr],       (Just reg), Nothing)
inst_blk (Store reg addr)       = ([Store reg addr],      (Just reg), Nothing)
inst_blk (Goto l)               = ([Goto l],               Nothing,   Nothing)
inst_blk (MkLabel l)            = ([MkLabel l],            Nothing,   Just l)
inst_blk (PrintLn a)            = ([PrintLn a],       (Nothing), Nothing)
inst_blk (If al relop ar lt lf) = ([If al relop ar lt lf], Nothing,   Just $ (fromMaybe lt lf))
                                  -- Use lf if not Nothing, otherwise use lt

resequence :: [Blk] -> Blk
resequence (blk : blks) = foldl merge_blk blk blks

-- Map

type Vars = Map String Addr
variable_address_index = "__variable_address_index"
variables = singleton variable_address_index 0 :: Vars

-- Compiling

compile :: Vars -> [Tree] -> (Vars, Blk)
compile vars t = second resequence $ foldl_with_map vars comp_tree t

comp_tree :: Vars -> Tree -> (Vars, Blk)
comp_tree vars (FuncDecl name sts) = let blk = inst_blk (MkLabel (LabelS name)) in
                                       second (resequence . (blk :)) $ foldl_with_map vars comp_stmt sts
comp_tree vars (Statements sts)    = second resequence $ foldl_with_map vars comp_stmt sts

comp_stmt :: Vars -> Statement -> (Vars, Blk)
comp_stmt vars (Expression exp)    = comp_exp vars exp
comp_stmt vars (VarDecl s exp _)   = let ((Just addr), vars') =
                                           updateLookupWithKey (\_ -> Just . succ) variable_address_index vars
                                         (vars'', blk@(_, rr, _)) = comp_exp vars' exp in
                                       (vars'', merge_blk blk $ inst_blk $ Store (fromMaybe 0 rr) addr)
comp_stmt vars (Println exp) = (,) vars $ inst_blk $ PrintLn exp
comp_stmt vars (WhileStmt exp sts) = let (vars', start_blk@(_, exp_res, _)) = second (merge_blk (inst_blk $ MkLabel label0)) $ comp_exp vars exp
                                         (_, body_blk)                      = comp_tree vars' (Statements sts)
                                         end_blk@(_, _, end_label)          = merge_blk body_blk (inst_blk $ MkLabel label1)
                                         if_blk                             = inst_blk (If (AReg $ fromMaybe 0 exp_res) Equal (ANumber 0) (fromJust end_label) Nothing) in
                                       -- Ignore the vars from body_blk as the variables alocated inside should not be propagated outward
                                       (,) vars $ resequence [start_blk, if_blk, body_blk, end_blk]



comp_exp :: Vars -> Exp -> (Vars, Blk)
comp_exp vars (LitExp (VTInt i _)) = (,) vars $ inst_blk $ Unary reg0 (ANumber i)
comp_exp vars (LitExp (VTBool b)) = (,) vars $ inst_blk $ Unary reg0 (ABool b)
comp_exp vars (LitExp (VTString s)) = (,) vars $ inst_blk $ Unary reg0 (AString s)
comp_exp vars (Var s)              = (,) vars $ inst_blk $ Load reg0 (vars ! s)
comp_exp vars (BinaryOp el so er)  = let (vars', blkl@(insl, rl, _)) = comp_exp vars el
                                         (vars'', blkr)              = comp_exp vars' er
                                         (insr, rr, lr)              = merge_blk blkl blkr in
                                       (,) vars'' (insr ++ [Binary (succ $ fromJust rr) (AReg $ fromJust rl) (read so :: Op) (AReg $ fromJust rr)],
                                                   (succ <$> rr),
                                                   (succ <$> lr))
comp_exp vars (IfExp exp true false) = let (vars', eval_blk@(_, er, _))     = comp_exp vars exp
                                           (_,  true_blk@(ins_tr, rt, llt)) = second (merge_blk $ inst_blk $ MkLabel label0)               $ comp_tree vars' (Statements true)
                                           (_, false_blk@(ins_fa, rf, _))   = second (merge_blk $ inst_blk $ MkLabel $ succ $ fromJust llt) $ comp_tree vars' (Statements false)
                                           true_blk'                        = maybe true_blk  (\trt -> (ins_tr ++ [Unary reg0 (AReg trt)], Just reg0, Nothing)) rt
                                           false_blk'                       = maybe false_blk (\trf -> (ins_fa ++ [Unary reg0 (AReg trf)], Just reg0, Nothing)) rf
                                           if_blk                           = inst_blk $ If (AReg $ fromJust er) Diff (ANumber 0) label0 (succ <$> llt) in
                                         (,) vars $ resequence [eval_blk, if_blk, true_blk', false_blk']



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

