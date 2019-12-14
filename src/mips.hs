module MIPS where

import Parser
import InterRep
import Compiler
import Data.Maybe
import Data.Map.Strict as Map ()
import Data.Bifunctor

data MIPS = Nop

instance ASM MIPS where
  compile state ir@(inst, reg, label) = undefined

-- -- MIPS --
-- to_mips :: [Instruction] -> String
-- to_mips [] = "\n"

-- -- > Arithmetic/Logic
-- to_mips ((Binary result (ANumber x) Plus (ANumber y)):xs) = "add " ++ (show result) ++ ", " ++ (show x) ++ ", " ++ (show y) ++ "\n" ++ (to_mips xs)
-- to_mips ((Binary result (ANumber x) Minus (ANumber y)):xs) = "sub " ++ (show result) ++ ", " ++ (show x) ++ ", " ++ (show y) ++ "\n" ++ (to_mips xs)
-- to_mips ((Binary result (ANumber x) Div (ANumber y)):xs) = "div " ++ (show result) ++ ", " ++ (show x) ++ ", " ++ (show y) ++ "\n" ++ (to_mips xs)
-- to_mips ((Binary result (ANumber x) Mult (ANumber y)):xs) = "mul " ++ (show result) ++ ", " ++ (show x) ++ ", " ++ (show y) ++ "\n" ++ (to_mips xs)
-- to_mips ((Binary result (ANumber x) Rem (ANumber y)):xs) = "rem " ++ (show result) ++ ", " ++ (show x) ++ ", " ++ (show y) ++ "\n" ++ (to_mips xs)

-- to_mips ((Binary result (ANumber x) And (ANumber y)):xs) = "and " ++ (show result) ++ ", " ++ (show x) ++ ", " ++ (show y) ++ "\n" ++ (to_mips xs)
-- to_mips ((Binary result (ANumber x) Or (ANumber y)):xs) = "or " ++ (show result) ++ ", " ++ (show x) ++ ", " ++ (show y) ++ "\n" ++ (to_mips xs)

-- -- > Comparison
-- to_mips ((Binary result (ANumber x) Equal (ANumber y)):xs) = "seq " ++ (show result) ++ ", " ++ (show x) ++ ", " ++ (show y) ++ "\n" ++ (to_mips xs)
-- to_mips ((Binary result (ANumber x) Diff (ANumber y)):xs) = "sne " ++ (show result) ++ ", " ++ (show x) ++ ", " ++ (show y) ++ "\n" ++ (to_mips xs)
-- to_mips ((Binary result (ANumber x) Lt (ANumber y)):xs) = "slt " ++ (show result) ++ ", " ++ (show x) ++ ", " ++ (show y) ++ "\n" ++ (to_mips xs)
-- to_mips ((Binary result (ANumber x) Gt (ANumber y)):xs) = "sgt " ++ (show result) ++ ", " ++ (show x) ++ ", " ++ (show y) ++ "\n" ++ (to_mips xs)
-- to_mips ((Binary result (ANumber x) Le (ANumber y)):xs) = "sle " ++ (show result) ++ ", " ++ (show x) ++ ", " ++ (show y) ++ "\n" ++ (to_mips xs)
-- to_mips ((Binary result (ANumber x) Ge (ANumber y)):xs) = "sge " ++ (show result) ++ ", " ++ (show x) ++ ", " ++ (show y) ++ "\n" ++ (to_mips xs)

-- -- > Branch and Jump Instructions
-- to_mips ((MkLabel label):xs) = (show label) ++ ":\n" ++ (to_mips xs)
-- to_mips ((If x Equal y l Nothing):xs) = "beq " ++ (show x) ++ ", " ++ (show y) ++ ", " ++ (show l)  ++ "\n" ++ (to_mips xs)
-- to_mips ((If x Diff y l Nothing):xs) = "bne " ++ (show x) ++ ", " ++ (show y) ++ ", " ++ (show l)  ++ "\n" ++ (to_mips xs)
-- to_mips ((Goto label):xs) = "jal " ++ (show label) ++ "\n" ++ (to_mips xs)
-- -- to_mips ((If x Lt y l Nothing):xs) = "slt $t1, " ++ (show y) ++ ", " ++ (show x) ++ "\n" ++
-- --                                      "beq $t1, $zero, " ++ (show l)  ++ "\n" ++ (to_mips xs)
-- -- to_mips ((If x Gt y l Nothing):xs) = "slt $t1, " ++ (show y) ++ ", " ++ (show x) ++ "\n" ++
-- --                                      "bnez $t1, " ++ (show l)  ++ "\n" ++ (to_mips xs)
-- to_mips ((If x Lt y l Nothing):xs) = "blt " ++ (show x) ++ ", " ++ (show y) ++ ", " ++ (show l) ++ "\n" ++ (to_mips xs)
-- to_mips ((If x Gt y l Nothing):xs) = "bgt " ++ (show x) ++ ", " ++ (show y) ++ ", " ++ (show l) ++ "\n" ++ (to_mips xs)
-- to_mips ((If x Le y l Nothing):xs) = "ble " ++ (show x) ++ ", " ++ (show y) ++ ", " ++ (show l) ++ "\n" ++ (to_mips xs)
-- to_mips ((If x Ge y l Nothing):xs) = "bge " ++ (show x) ++ ", " ++ (show y) ++ ", " ++ (show l) ++ "\n" ++ (to_mips xs)
-- to_mips ((If x Lt y l (Just l2)):xs) = "blt " ++ (show x) ++ ", " ++ (show y) ++ ", " ++ (show l) ++ "\njal" ++ (show l2) ++ "\n" ++ (to_mips xs)
-- to_mips ((If x Gt y l (Just l2)):xs) = "bgt " ++ (show x) ++ ", " ++ (show y) ++ ", " ++ (show l) ++ "\n" ++ "\njal" ++ (show l2) ++ (to_mips xs)
-- to_mips ((If x Le y l (Just l2)):xs) = "ble " ++ (show x) ++ ", " ++ (show y) ++ ", " ++ (show l) ++ "\n" ++ "\njal" ++ (show l2) ++ (to_mips xs)
-- to_mips ((If x Ge y l (Just l2)):xs) = "bge " ++ (show x) ++ ", " ++ (show y) ++ ", " ++ (show l) ++ "\n" ++ "\njal" ++ (show l2) ++ (to_mips xs)

-- -- > Load/Store/Move Instructions
-- to_mips ((Unary reg (ANumber num)):xs) = "li " ++ (show reg) ++ " " ++ (show num) ++ "\n" ++ (to_mips xs)
-- to_mips ((Store reg (ANumber num)):xs) = "li " ++ (show reg) ++ " " ++ (show num) ++ "\n" ++ (to_mips xs)
