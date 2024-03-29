{-# LANGUAGE ViewPatterns #-}

module IR where

import Scanner
import Parser
import Options.Applicative
import Data.Monoid ((<>))
import Data.Maybe
import Data.Map.Strict as Map (fromList, insert, insertLookupWithKey, updateLookupWithKey, (!), Map)
import Control.Monad
import Data.Bifunctor

-- Utils

foldl_with_map :: Vars -> (Vars -> a -> (Vars, b)) -> [a] -> (Vars, [b])
foldl_with_map vars f (x:xs) = let (vars', xres) = f vars x in
                               second ((:) xres) (foldl_with_map vars' f xs)
foldl_with_map vars _ []     = (vars, [])

-- Registers

newtype Reg = Reg Integer

instance Show Reg where
  show (Reg i) = "t" ++ (show i)

instance Num Reg where
  (Reg l) + (Reg r) = Reg $ l + r
  (Reg l) - (Reg r) = Reg $ l - r
  fromInteger i     = (Reg i)
  (*)               = undefined
  abs               = undefined
  signum            = undefined

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
  (LabelS _) + (LabelI r) = LabelI r
  (LabelI l) + (LabelS _) = LabelI l
  _ + _                   = error "Cannot add string labels"
  (LabelI l) - (LabelI r) = LabelI $ l - r
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

data Atom = AReg Reg
          | ANumber Int
          | AAddr Addr
          | ALabel Int

instance Show Atom where
  show (AReg r) = show r
  show (ANumber i) = show i
  show (AAddr a) = "#" ++ show a
  show (ALabel s) = "lbl-" ++ show s

relocate_atom :: Maybe Reg -> Atom -> Atom
relocate_atom (Just l) (AReg r) = AReg $ l + r
relocate_atom _ a = a

-- Operators

data Op = Plus | Minus | Div | Mult | Rem   -- Arithmetic
        | And | Or                          -- Logic
        | Equal | Diff | Lt | Gt | Le | Ge  -- Comparison

instance Show Op where
  show Plus   = "+"
  show Minus  = "-"
  show Div    = "/"
  show Mult   = "*"
  show Rem    = "%"
  show And    = "&&"
  show Or     = "||"
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
  readsPrec _ ('&':'&':rest)  = [(And, rest)]
  readsPrec _ ('|':'|':rest)  = [(Or, rest)]
  readsPrec _ ('<':'=':rest)  = [(Le, rest)]
  readsPrec _ ('>':'=':rest)  = [(Ge, rest)]
  readsPrec _ ('=':'=':rest)  = [(Equal, rest)]
  readsPrec _ ('!':'=':rest)  = [(Diff, rest)]
  readsPrec _ ('<':rest)      = [(Lt, rest)]
  readsPrec _ ('>':rest)      = [(Gt, rest)]
  readsPrec _ _               = error "Invalid character to make an `Op`"

-- Map

type Vars = Map String Addr
variable_address_index = "__variable_address_index"
statics_label_counter  = "__statics_label_counter"
variables = fromList [(variable_address_index, 0), (statics_label_counter, 0)] :: Vars

-- Instructions

type Addr = Int

data Instruction = Unary Reg Atom
                 | Binary Reg Reg Op Atom
                 | Load Reg Atom
                 | Store Reg Atom
                 | Goto Label
                 | MkLabel Label
                 | If Atom Op Atom Label (Maybe Label)
                 | PrintLn Reg
                 | Halt

instance Show Instruction where
  show (Unary r a)          = "  " ++ (show r) ++ ":= " ++ (show a) ++ ";"
  show (Binary r rl op ar)  = "  " ++ (show r) ++ ":= " ++ (show rl) ++ " " ++ (show op) ++ " " ++ (show ar) ++ ";"
  show (Load r a)           = "  load " ++ (show r) ++ " (" ++ (show a) ++ ")"
  show (Store r a)          = "  store " ++ (show r) ++ " (" ++ (show a) ++ ")"
  show (Goto l)             = "  goto " ++ show l
  show (MkLabel l)          = show l ++ ":"
  show (If al rel ar lt lf) = "  if " ++ (show al) ++ " " ++ (show rel) ++ " " ++ (show ar)
                              ++ " then\n  " ++ (show (Goto lt)) ++
                              (maybe "" (\jlf -> "\n  else\n  " ++ (show (Goto jlf))) lf)
  show (PrintLn r)          = "  call println " ++ (show r) ++ ";"
  show (Halt)               = "  halt;"

relocate_instruction :: Maybe Reg -> Maybe Label -> Instruction -> Instruction
relocate_instruction rb _  (Unary r a)            = Unary   (fromJust $ maybe_offset rb $ Just r) (relocate_atom rb a)
relocate_instruction rb _  (Binary r rl op ar)    = Binary  (fromJust $ maybe_offset rb $ Just r)
                                                    (fromJust $ maybe_offset rb $ Just rl) op (relocate_atom rb ar)
relocate_instruction rb _  (Load r a)             = Load    (fromJust $ maybe_offset rb $ Just r) (relocate_atom rb a)
relocate_instruction rb _  (Store r a)            = Store   (fromJust $ maybe_offset rb $ Just r) (relocate_atom rb a)
relocate_instruction _ lb  (Goto l)               = Goto    (fromJust $ maybe_offset lb $ Just l)
relocate_instruction _ lb  (MkLabel l@(LabelI _)) = MkLabel (fromJust $ maybe_offset lb $ Just l)
relocate_instruction _ lb  (MkLabel l@(LabelS _)) = MkLabel l
relocate_instruction rb lb (If al relop ar lt lf) = If      (relocate_atom rb al) relop (relocate_atom rb ar)
                                                            (fromJust $ maybe_offset lb (Just lt)) (maybe_offset lb lf)
relocate_instruction rb _  (PrintLn r)            = PrintLn (fromJust $ maybe_offset rb $ Just r)
relocate_instruction _ _   (Halt)                 = Halt

-- Blocks

type Blk = ([Instruction], (Maybe Reg), (Maybe Label))

relocate_block :: Maybe Reg -> Maybe Label -> Blk -> Blk
relocate_block rb lb (ins, rr, lr) = (map (relocate_instruction rb lb) ins, (maybe_offset rb rr), (maybe_offset lb lr))

merge_blk :: Blk -> Blk -> Blk
merge_blk (insl, rl, ll) blkr = (insl ++ insr, rr, lr) where (insr, rr, lr) = relocate_block (succ <$> rl) ll blkr

inst_blk :: Instruction -> Blk
inst_blk (Unary reg a)          = ([Unary reg a],         (Just reg), Nothing)
inst_blk (Binary reg rl op ar)  = ([Binary reg rl op ar], (Just reg), Nothing)
inst_blk (Load reg a)           = ([Load reg a],          (Just reg), Nothing)
inst_blk (Store reg a)          = ([Store reg a],         (Just reg), Nothing)
inst_blk (Goto l)               = ([Goto l],               Nothing,   Nothing)
inst_blk (MkLabel l)            = ([MkLabel l],            Nothing,   Just l)
inst_blk (If al relop ar lt lf) = ([If al relop ar lt lf], Nothing,   Just $ (fromMaybe lt lf))
                                  -- Use lf if not Nothing, otherwise use lt
inst_blk (PrintLn r)            = ([PrintLn r],            Nothing,   Nothing)
inst_blk (Halt)                 = ([Halt],                 Nothing,   Nothing)

resequence :: [Blk] -> Blk
resequence (blk : blks) = foldl merge_blk blk blks
resequence [] = ([], Nothing, Nothing)

norelocate_concat :: [Blk] -> Blk
norelocate_concat [] = error "Cannot concat empty Blk list"
norelocate_concat [b@(_, _, _)] = b
norelocate_concat ((ins, _, _) : blks) = (ins ++ insr, rr, lr) where (insr, rr, lr) = norelocate_concat blks

norelocate_append :: Blk -> [Blk] -> Blk
norelocate_append blkl blksr = norelocate_concat (blkl : blksr)

-- Transforming to Intermediate Representation

ir :: Vars -> [Tree] -> (Vars, Blk)
ir vars t = second (resequence . (\blk -> blk ++ [inst_blk Halt])) $ foldl_with_map vars ir_tree t

ir_tree :: Vars -> Tree -> (Vars, Blk)
ir_tree vars (FuncDecl name sts) = let blk = inst_blk (MkLabel (LabelS name)) in
                                      second (resequence . (blk :)) $ foldl_with_map vars ir_stmt sts
ir_tree vars (Statements sts)    = second resequence $ foldl_with_map vars ir_stmt sts

ir_stmt :: Vars -> Statement -> (Vars, Blk)
ir_stmt vars (Expression exp)    = ir_exp vars exp
ir_stmt vars (VarDecl s exp _)   = let (Just addr, vars')       = updateLookupWithKey (\_ -> Just . succ) variable_address_index vars
                                       (vars'', blk@(_, rr, _)) = first (insert s addr) $ ir_exp vars' exp in
                                     (vars'', norelocate_append blk [(inst_blk $ Store (fromMaybe 0 rr) $ AAddr addr)])
ir_stmt vars (WhileStmt exp sts) = let (vars', start_blk@(_, exp_res, le)) = second (merge_blk (inst_blk $ MkLabel label0)) $ ir_exp vars exp
                                       (_, body_blk@(_, _, end_label))     = second (relocate_block (succ <$> exp_res) le) $ ir_tree vars' (Statements sts)
                                       end_label'                          = (succ $ fromJust $ end_label <|> le)
                                       end_blk                             = inst_blk $ MkLabel $ end_label'
                                       if_blk                              = inst_blk (If (AReg $ fromMaybe 0 exp_res) Equal (ANumber 0) end_label' Nothing)
                                       if_blk'                             = norelocate_concat [start_blk, if_blk, body_blk] in
                                     (,) vars $ norelocate_concat [if_blk', end_blk]
                                     -- Ignore the vars from body_blk as the variables alocated inside should not be propagated outward
ir_stmt vars (Println exp)       = second (\blk@(_, re, _) -> norelocate_concat [blk, inst_blk $ PrintLn $ fromJust re]) $ ir_exp vars exp
ir_stmt vars (Attr s exp)        = let (vars', blk@(_, rr, _)) = ir_exp vars exp in
                                     (,) vars' $ norelocate_concat $ [blk, inst_blk $ Store (fromJust rr) $ AAddr $ vars' ! s]



ir_exp :: Vars -> Exp -> (Vars, Blk)
ir_exp vars (LitExp (VTInt i _))  = (,) vars $ inst_blk $ Unary reg0 (ANumber i)
ir_exp vars (LitExp (VTBool b))   = (,) vars $ inst_blk $ Unary reg0 (ANumber $ fromEnum b)
ir_exp vars (LitExp (VTString s)) = let (Just lbl, vars') = updateLookupWithKey (\_ -> Just . succ) statics_label_counter vars in
                                      (,) vars' $ inst_blk $ Unary reg0 (ALabel lbl)
ir_exp vars (Var s)               = (,) vars $ inst_blk $ Load reg0 (AAddr $ vars ! s)
ir_exp vars (UnaryOp so (Var s))  = (,) vars $ norelocate_concat [snd $ ir_exp vars (Var s), inst_blk (Binary reg0 reg0 (read (so) :: Op) (ANumber 1))]
ir_exp vars (BinaryOp (Var s) so er) = let (vars', blke@(_, re, _)) = ir_exp vars er
                                           (_,     blkv@(_, rv, _)) = ir_exp vars (Var s) in
                                         (,) vars' $ norelocate_concat [resequence [blke, blkv], inst_blk $ Binary (succ $ fromJust rv) (succ $ fromJust rv) (read (so) :: Op) (AReg $ fromJust re)]
                                         -- (,) vars' (insv ++ inse ++ [Binary (fromJust rv) (fromJust rv) (read (take 1 so) :: Op) (AReg $ fromJust re)], rv, (succ <$> lr))
ir_exp vars (BinaryOp el so er) = let (vars', blkl@(insl, rl, _)) = ir_exp vars el
                                      (vars'', blkr)              = ir_exp vars' er
                                      (insr, rr, lr)              = merge_blk blkl blkr in
                                   (,) vars'' (insr ++ [Binary (succ $ fromJust rr) (fromJust rl) (read so :: Op) (AReg $ fromJust rr)], (succ <$> rr), (succ <$> lr))
ir_exp vars (IfExp exp true false) = let (vars', eval_blk@(_, er, _))     = ir_exp vars exp
                                         (_,  true_blk@(ins_tr, rt, llt)) = second (merge_blk $ inst_blk $ MkLabel label0)                $ ir_tree vars' (Statements true)
                                         (_, false_blk@(ins_fa, rf, llf)) = second (merge_blk $ inst_blk $ MkLabel $ succ $ fromJust llt) $ ir_tree vars' (Statements false)
                                         end_label                        = succ $ fromJust llf
                                         end_blk                          = inst_blk $ MkLabel $ end_label
                                         true_blk'                        = maybe true_blk  (\trt -> (ins_tr ++ [Unary reg0 (AReg trt), Goto end_label], Just reg0, Nothing)) rt
                                         false_blk'                       = maybe false_blk (\trf -> (ins_fa ++ [Unary reg0 (AReg trf), Goto end_label], Just reg0, Nothing)) rf
                                         if_blk                           = inst_blk $ If (AReg $ fromJust er) Diff (ANumber 0) label0 (succ <$> llt) in
                                       (,) vars $ norelocate_concat [eval_blk, if_blk, true_blk', false_blk', end_blk]
