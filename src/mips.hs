{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ExistentialQuantification #-}

module MIPS where

import Parser
import InterRep
import Compiler

import Data.Maybe
import Data.Map.Strict as Map ()
import Data.Bifunctor
import Data.List

data Reg = Zero | VReg Int | AReg Int | TReg Int | GP | SP | FP | RA

instance ASM MIPS where
  compile state ir@(inst, reg, label) = undefined

data AnyShow = forall s. Show s => AS s
showIt (AS s) = show s

commas :: [AS] -> String
commas = intercalate ", " . showIt

-- MIPS --
to_mips :: Instruction -> String
-- to_mips [] = "\n"

-- > Arithmetic/Logic
to_mips Binary result (ANumber x) Plus  (ANumber y) = "add " $ commas [result, x, y]
to_mips Binary result (ANumber x) Minus (ANumber y) = "sub " $ commas [result, x, y]
to_mips Binary result (ANumber x) Div   (ANumber y) = "div " $ commas [result, x, y]
to_mips Binary result (ANumber x) Mult  (ANumber y) = "mul " $ commas [result, x, y]
to_mips Binary result (ANumber x) Rem  (ANumber y)  = "rem " $ commas [result, x, y]

to_mips Binary result (ANumber x) And (ANumber y)   = "and " $ commas [result, x, y]
to_mips Binary result (ANumber x) Or (ANumber y)    = "or " $ commas [result, x, y]

-- > Comparison
to_mips Binary result (ANumber x) Equal (ANumber y) = "seq " $ commas [result, x, y]
to_mips Binary result (ANumber x) Diff (ANumber y)  = "sne " $ commas [result, x, y]
to_mips Binary result (ANumber x) Lt (ANumber y)    = "slt " $ commas [result, x, y]
to_mips Binary result (ANumber x) Gt (ANumber y)    = "sgt " $ commas [result, x, y]
to_mips Binary result (ANumber x) Le (ANumber y)    = "sle " $ commas [result, x, y]
to_mips Binary result (ANumber x) Ge (ANumber y)    = "sge " $ commas [result, x, y]

-- > Branch and Jump Instructions
to_mips MkLabel label          = label ++ ":\n"
to_mips If x Equal y l Nothing = "beq " $ commas [x, y, l]
to_mips If x Diff y l Nothing  = "bne " $ commas [x, y, l]
to_mips Goto label             = "jal " $ commas [label]
-- to_mips If x Lt y l Nothing = "slt $t1, " $ commas [y]
--                                      "beq $t1, $zero, " $ commas [how l)   ++ (to_mips xs)]
-- to_mips ((If x Gt y l Nothing:xs) = "slt $t1, " $ commas [y]
--                                      "bnez $t1, " $ commas [how l)   ++ (to_mips xs)]
to_mips If x Lt y l Nothing   = "blt " $ commas [x, y, l]
to_mips If x Gt y l Nothing   = "bgt " $ commas [x, y, l]
to_mips If x Le y l Nothing   = "ble " $ commas [x, y, l]
to_mips If x Ge y l Nothing   = "bge " $ commas [x, y, l]
to_mips If x Lt y l (Just l2) = "blt " $ commas [x, y, l] ++ "\njal " ++ commas [l2]
to_mips If x Gt y l (Just l2) = "bgt " $ commas [x, y, l] ++ "\njal " ++ commas [l2]
to_mips If x Le y l (Just l2) = "ble " $ commas [x, y, l] ++ "\njal " ++ commas [l2]
to_mips If x Ge y l (Just l2) = "bge " $ commas [x, y, l] ++ "\njal " ++ commas [l2]

-- > Load/Store/Move Instructions
to_mips Unary reg (ANumber num) = "li " $ commas [reg, num]
to_mips Store reg (ANumber num) = "li " $ commas [reg, num]
