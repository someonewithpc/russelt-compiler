{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ExistentialQuantification #-}

module MIPS where

import Parser
import qualified IR
import Compiler

import Data.Maybe
import Data.Map.Strict as Map ()
import Data.Bifunctor
import Data.List

-- Registers

data Reg = Zero | VReg Integer | AReg Integer | TReg Integer | SReg Integer | GP | SP | FP | RA

instance Num Reg where
  fromInteger i | i == 0             = Zero
                | i == 1             = undefined -- Reserved for assembler, at register
                | i >= 2 && i <= 3   = VReg (i - 2)
                | i >= 4 && i <= 7   = AReg (i - 4)
                | i >= 8 && i <= 15  = TReg (i - 8)
                | i >= 16 && i <= 23 = SReg (i - 16)
                | i >= 24 && i <= 25 = TReg (i - 16)
                | i >= 26 && i <= 27 = undefined -- Reserved for OS, k registersXS
                | i == 28            = GP
                | i == 29            = SP
                | i == 30            = FP
                | i == 31            = RA
  _ + _  = undefined
  _ * _  = undefined
  _ - _  = undefined
  abs    = undefined
  signum = undefined

instance Show Reg where
  show Zero     = "$zero"
  show (VReg i) = "$" ++ (show i)
  show (AReg i) = "$" ++ (show i)
  show (TReg i) = "$" ++ (show i)
  show GP       = "$gp"
  show SP       = "$sp"
  show FP       = "$fp"
  show RA       = "$ra"

to_reg :: IR.Reg -> Reg
to_reg (IR.Reg i) = fromInteger i :: Reg

-- Instructions

data MIPS = NOOP | ADD Reg Reg Reg
          | ADDI Reg Reg Int | MULT Reg Reg | DIV Reg Reg | SUB Reg Reg Reg
          | AND Reg Reg Reg | ANDI Reg Reg Int | OR Reg Reg Reg | ORI Reg Reg Int
          | BEQ Reg Reg Int | BGEZ Reg Int | BGEZAL Reg Int | BGTZ Reg Int
          | BLTZ Reg Int | BLTZAL Reg Int | BNE Reg Reg Int
          | J Int | JAL Int | JR Reg
          | LB Reg Reg Int | LW Reg Reg Int | LUI Reg Int
          | MOVE Reg Reg | LI Reg Int | LA Reg String
          | SB Reg Reg Int | SW Reg Reg Int
          | SLT Reg Reg Reg | SLTI Reg Reg Int
          | MFHI Reg | MFLO Reg
          | LABEL String
          | SYSCALL

data Operands = OpReg Reg | OpInt Int | OpStr String

instance Show Operands where
  show (OpReg r) = show r
  show (OpInt i) = show i
  show (OpStr s) = s

commas :: [Operands] -> String
commas = intercalate ", " . map show

instance Show MIPS where
  show (NOOP)         = "noop"
  show (ADD r0 r1 r2) = "add "    ++ (commas [OpReg r0, OpReg r1, OpReg r2])
  show (ADDI r0 r1 i) = "addi "   ++ (commas [OpReg r0, OpReg r1, OpInt i])
  show (MULT r0 r1)   = "mult "   ++ (commas [OpReg r0, OpReg r1])
  show (DIV r0 r1)    = "div "    ++ (commas [OpReg r0, OpReg r1])
  show (SUB r0 r1 r2) = "sub "    ++ (commas [OpReg r0, OpReg r1, OpReg r2])
  show (AND r0 r1 r2) = "and "    ++ (commas [OpReg r0, OpReg r1, OpReg r2])
  show (ANDI r0 r1 i) = "andi "   ++ (commas [OpReg r0, OpReg r1, OpInt i])
  show (OR r0 r1 r2)  = "or "     ++ (commas [OpReg r0, OpReg r1, OpReg r2])
  show (ORI r0 r1 i)  = "ori "    ++ (commas [OpReg r0, OpReg r1, OpInt i])
  show (BEQ r0 r1 i)  = "beq "    ++ (commas [OpReg r0, OpReg r1, OpInt i])
  show (BGEZ r0 i)    = "bgez "   ++ (commas [OpReg r0, OpInt i])
  show (BGEZAL r0 i)  = "bgezal " ++ (commas [OpReg r0, OpInt i])
  show (BGTZ r0 i)    = "bgtz "   ++ (commas [OpReg r0, OpInt i])
  show (BLTZ r0 i)    = "bltz "   ++ (commas [OpReg r0, OpInt i])
  show (BLTZAL r0 i)  = "bltzal " ++ (commas [OpReg r0, OpInt i])
  show (BNE r0 r1 i)  = "bne "    ++ (commas [OpReg r0, OpReg r1, OpInt i])
  show (LW r0 r1 i)   = "lw "     ++ (commas [OpReg r0, OpReg r1, OpInt i])
  show (LUI r0 i)     = "lui "    ++ (commas [OpReg r0, OpInt i])
  show (MOVE r0 r1)   = "move "   ++ (commas [OpReg r0, OpReg r1])
  show (LI r0 i)      = "li "     ++ (commas [OpReg r0, OpInt i])
  show (LA r0 s)      = "la "     ++ (commas [OpReg r0, OpStr s])
  show (SB r0 r1 i)   = "sb "     ++ (commas [OpReg r0, OpReg r1, OpInt i])
  show (SW r0 r1 i)   = "sw "     ++ (commas [OpReg r0, OpReg r1, OpInt i])
  show (SLT r0 r1 r2) = "slt "    ++ (commas [OpReg r0, OpReg r1, OpReg r2])
  show (SLTI r0 r1 i) = "slti "   ++ (commas [OpReg r0, OpReg r1, OpInt i])
  show (J i)          = "j "      ++ (show i)
  show (JAL i)        = "jal "    ++ (show i)
  show (JR r0)        = "jr "     ++ (show r0)
  show (LB r0 r1 i)   = "lb "     ++ (show r0)
  show (MFHI r0)      = "mfhi "   ++ (show r0)
  show (MFLO r0)      = "mflo "   ++ (show r0)
  show (LABEL s)      = s ++ ":"
  show (SYSCALL)      = "syscall"


instance ASM MIPS where
  compile vars ir@(inst, _, _) = map (comp_inst vars) inst

comp_inst :: IR.Vars -> IR.Instruction -> MIPS
comp_inst vars (IR.Unary reg (IR.AReg areg))  = MOVE (to_reg reg) (to_reg areg)
comp_inst vars (IR.Unary reg (IR.ANumber an)) = LI (to_reg reg) an
comp_inst vars (IR.Unary reg (IR.ALabel lbl)) = LA (to_reg reg) $ "__st" ++ (show lbl)
-- comp_inst vars (IR.Binary reg (IR.ANumber an)) = LI (to_reg reg) an
