{-# LANGUAGE ViewPatterns #-}

module InterRep where

import Scanner
import Parser
import Options.Applicative
import Data.Monoid ((<>))
import Data.Maybe
import Data.Map.Strict as Map (singleton, insert, insertLookupWithKey, updateLookupWithKey, (!), Map)
import Control.Monad
import Data.Bifunctor

-- Utils

foldl_with_map :: State -> (State -> a -> (State, b)) -> [a] -> (State, [b])
foldl_with_map state f (x:xs) = let (state', xres) = f state x in
                               second ((:) xres) (foldl_with_map state' f xs)
foldl_with_map state _ []     = (state, [])

-- Registers

newtype IRReg = IRReg Integer

instance Show IRReg where
  show (IRReg i) = "t" ++ (show i)

instance Num IRReg where
  (IRReg l) + (IRReg r) = IRReg $ l + r
  (IRReg l) - (IRReg r) = IRReg $ l - r
  fromInteger i     = (IRReg i)
  (*)               = undefined
  abs               = undefined
  signum            = undefined

instance Enum IRReg where
  toEnum i = IRReg (toInteger i)
  fromEnum (IRReg i) = fromIntegral i

reg0 = IRReg 0
reg1 = IRReg 1

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

data Atom = AVar String
          | AReg IRReg
          | ANumber Int
          | AAddr Addr
          | AStatic Int

instance Show Atom where
  show (AVar s) = s
  show (AReg r) = show r
  show (ANumber i) = show i
  show (AAddr a) = "$" ++ show a
  show (AStatic s) = "#" ++ show s

relocate_atom :: Maybe IRReg -> Atom -> Atom
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
variables = singleton variable_address_index 0 :: Vars
type Statics = Map String Int
statics_address_index = "__statics_address_index"
statics = singleton statics_address_index 0 :: Statics
state = (variables, statics)
type State = (Vars, Statics)

-- IRInstructions

type Addr = Int

data IRInstruction = Unary IRReg Atom
                   | Binary IRReg Atom Op Atom
                   | Load IRReg Atom
                   | Store IRReg Atom
                   | Goto Label
                   | MkLabel Label
                   | If Atom Op Atom Label (Maybe Label)
                   | PrintLn IRReg
                   | Halt

instance Show IRInstruction where
  show (Unary r a)          = "  " ++ (show r) ++ ":= " ++ (show a) ++ ";"
  show (Binary r al op ar)  = "  " ++ (show r) ++ ":= " ++ (show al) ++ " " ++ (show op) ++ " " ++ (show ar) ++ ";"
  show (Load r a)           = "  load " ++ (show r) ++ " (" ++ (show a) ++ ")"
  show (Store r a)          = "  store " ++ (show r) ++ " (" ++ (show a) ++ ")"
  show (Goto l)             = "  goto " ++ show l
  show (MkLabel l)          = show l ++ ":"
  show (If al rel ar lt lf) = "  if " ++ (show al) ++ " " ++ (show rel) ++ " " ++ (show ar)
                              ++ " then\n  " ++ (show (Goto lt)) ++
                              (maybe "" (\jlf -> "\n  else\n  " ++ (show (Goto jlf))) lf)
  show (PrintLn r)          = "  call println " ++ (show r) ++ ";"
  show (Halt)               = "  halt;"

relocate_instruction :: Maybe IRReg -> Maybe Label -> IRInstruction -> IRInstruction
relocate_instruction rb _  (Unary r a)            = Unary   (fromJust $ maybe_offset rb $ Just r) (relocate_atom rb a)
relocate_instruction rb _  (Binary r al numop ar) = Binary  (fromJust $ maybe_offset rb $ Just r) (relocate_atom rb al) numop (relocate_atom rb ar)
relocate_instruction rb _  (Load r a)             = Load    (fromJust $ maybe_offset rb $ Just r) (relocate_atom rb a)
relocate_instruction rb _  (Store r a)            = Store   (fromJust $ maybe_offset rb $ Just r) (relocate_atom rb a)
relocate_instruction _ lb  (Goto l)               = Goto    (fromJust $ maybe_offset lb $ Just l)
relocate_instruction _ lb  (MkLabel l)            = MkLabel (fromJust $ maybe_offset lb $ Just l)
relocate_instruction rb lb (If al relop ar lt lf) = If      (relocate_atom rb al) relop (relocate_atom rb ar)
                                                            (fromJust $ maybe_offset lb (Just lt)) (maybe_offset lb lf)
relocate_instruction rb _  (PrintLn r)            = PrintLn (fromJust $ maybe_offset rb $ Just r)
relocate_instruction _ _   (Halt)                 = Halt

-- Blocks

type IRBlk = ([IRInstruction], (Maybe IRReg), (Maybe Label))

relocate_block :: Maybe IRReg -> Maybe Label -> IRBlk -> IRBlk
relocate_block rb lb (ins, rr, lr) = (map (relocate_instruction rb lb) ins, (maybe_offset rb rr), (maybe_offset lb lr))

merge_blk :: IRBlk -> IRBlk -> IRBlk
merge_blk (insl, rl, ll) blkr = (insl ++ insr, rr, lr) where (insr, rr, lr) = relocate_block (succ <$> rl) ll blkr

inst_blk :: IRInstruction -> IRBlk
inst_blk (Unary reg a)          = ([Unary reg a],         (Just reg), Nothing)
inst_blk (Binary reg al op ar)  = ([Binary reg al op ar], (Just reg), Nothing)
inst_blk (Load reg a)           = ([Load reg a],          (Just reg), Nothing)
inst_blk (Store reg a)          = ([Store reg a],         (Just reg), Nothing)
inst_blk (Goto l)               = ([Goto l],               Nothing,   Nothing)
inst_blk (MkLabel l)            = ([MkLabel l],            Nothing,   Just l)
inst_blk (If al relop ar lt lf) = ([If al relop ar lt lf], Nothing,   Just $ (fromMaybe lt lf))
                                  -- Use lf if not Nothing, otherwise use lt
inst_blk (PrintLn r)            = ([PrintLn r],            Nothing,   Nothing)
inst_blk (Halt)                 = ([Halt],                 Nothing,   Nothing)

resequence :: [IRBlk] -> IRBlk
resequence (blk : blks) = foldl merge_blk blk blks

norelocate_concat :: [IRBlk] -> IRBlk
norelocate_concat [] = error "Cannot concat empty IRBlk list"
norelocate_concat [b@(_, _, _)] = b
norelocate_concat ((ins, _, _) : blks) = (ins ++ insr, rr, lr) where (insr, rr, lr) = norelocate_concat blks

norelocate_append :: IRBlk -> [IRBlk] -> IRBlk
norelocate_append blkl blksr = norelocate_concat (blkl : blksr)

-- Transforming to Intermediate Representation

ir :: State -> [Tree] -> (State, IRBlk)
ir state t = second (resequence . (\blk -> blk ++ [inst_blk Halt])) $ foldl_with_map state ir_tree t

ir_tree :: State -> Tree -> (State, IRBlk)
ir_tree state (FuncDecl name sts) = let blk = inst_blk (MkLabel (LabelS name)) in
                                      second (resequence . (blk :)) $ foldl_with_map state ir_stmt sts
ir_tree state (Statements sts)    = second resequence $ foldl_with_map state ir_stmt sts

ir_stmt :: State -> Statement -> (State, IRBlk)
ir_stmt state (Expression exp)    = ir_exp state exp
ir_stmt state (VarDecl s exp _)   = let ((Just addr, vars), statics) = first (updateLookupWithKey (\_ -> Just . succ) variable_address_index) state
                                        (state', blk@(_, rr, _))     = first (first $ insert s addr) $ ir_exp (vars, statics) exp in
                                      (state', norelocate_append blk [(inst_blk $ Store (fromMaybe 0 rr) $ AAddr addr)])
ir_stmt state (WhileStmt exp sts) = let (state', start_blk@(_, exp_res, le)) = second (merge_blk (inst_blk $ MkLabel label0)) $ ir_exp state exp
                                        (_, body_blk@(_, _, end_label))      = second (relocate_block (succ <$> exp_res) le) $ ir_tree state' (Statements sts)
                                        end_label'                           = (succ $ fromJust $ end_label <|> le)
                                        end_blk                              = inst_blk $ MkLabel $ end_label'
                                        if_blk                               = inst_blk (If (AReg $ fromMaybe 0 exp_res) Equal (ANumber 0) end_label' Nothing)
                                        if_blk'                              = norelocate_concat [start_blk, if_blk, body_blk] in
                                      (,) state $ norelocate_concat [if_blk', end_blk]
                                       -- Ignore the state from body_blk as the variables alocated inside should not be propagated outward
ir_stmt state (Println exp)       = second (\blk@(_, re, _) -> norelocate_concat [blk, inst_blk $ PrintLn $ fromJust re]) $ ir_exp state exp
ir_stmt state (Attr s exp)        = let (state', blk@(_, rr, _)) = ir_exp state exp in
                                      (,) state' $ norelocate_concat $ [blk, inst_blk $ Store (fromJust rr) $ AAddr $ (fst state') ! s]



ir_exp :: State -> Exp -> (State, IRBlk)
ir_exp state (LitExp (VTInt i _))   = (,) state $ inst_blk $ Unary reg0 (ANumber i)
ir_exp state (LitExp (VTBool b))    = (,) state $ inst_blk $ Unary reg0 (ANumber $ fromEnum b)
ir_exp state (LitExp (VTString s))  = let (vars, (Just st, statics)) = second (updateLookupWithKey (\_ -> Just . succ) statics_address_index) state
                                          (old, statics')              = insertLookupWithKey (\_ _ new -> new) s st statics in
                                          -- If old is Just, the string was already accounted for, so keep the old state
                                        (maybe (vars, statics') (\_ -> state) old, inst_blk $ Load reg0 $ AStatic st)
ir_exp state (Var s)                = (,) state $ inst_blk $ Load reg0 (AAddr $ (fst state) ! s)
ir_exp state (BinaryOp el so er)    = let (state', blkl@(insl, rl, _)) = ir_exp state el
                                          (state'', blkr)              = ir_exp state' er
                                          (insr, rr, lr)              = merge_blk blkl blkr in
                                        (,) state'' (insr ++ [Binary (succ $ fromJust rr) (AReg $ fromJust rl) (read so :: Op) (AReg $ fromJust rr)],
                                                      (succ <$> rr),
                                                      (succ <$> lr))
ir_exp state (IfExp exp true false) = let (state', eval_blk@(_, er, _))     = ir_exp state exp
                                          (_,  true_blk@(ins_tr, rt, llt)) = second (merge_blk $ inst_blk $ MkLabel label0)                $ ir_tree state' (Statements true)
                                          (_, false_blk@(ins_fa, rf, llf)) = second (merge_blk $ inst_blk $ MkLabel $ succ $ fromJust llt) $ ir_tree state' (Statements false)
                                          end_label                        = succ $ fromJust llf
                                          end_blk                          = inst_blk $ MkLabel $ end_label
                                          true_blk'                        = maybe true_blk  (\trt -> (ins_tr ++ [Unary reg0 (AReg trt), Goto end_label], Just reg0, Nothing)) rt
                                          false_blk'                       = maybe false_blk (\trf -> (ins_fa ++ [Unary reg0 (AReg trf), Goto end_label], Just reg0, Nothing)) rf
                                          if_blk                           = inst_blk $ If (AReg $ fromJust er) Diff (ANumber 0) label0 (succ <$> llt) in
                                        (,) state $ norelocate_concat [eval_blk, if_blk, true_blk', false_blk', end_blk]
