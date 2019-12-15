
module MIPS where

import Parser
import qualified IR
import Compiler

import Data.Maybe
import Data.Map.Strict as Map ()
import Data.Bifunctor
import Data.List
import Control.Exception (try, evaluate)

base_address = 0

-- Registers

register_count = 32
data Reg = Zero | VReg Integer | AReg Integer | TReg Integer | SReg Integer | GP | SP | FP | RA

instance Num Reg where
  fromInteger i | i == 0             = Zero
                | i == 1             = error "Register r1 is reserved for assembler, $at register"
                | i >= 2 && i <= 3   = VReg (i - 2)
                | i >= 4 && i <= 7   = AReg (i - 4)
                | i >= 8 && i <= 15  = TReg (i - 8)
                | i >= 16 && i <= 23 = SReg (i - 16)
                | i >= 24 && i <= 25 = TReg (i - 16)
                | i >= 26 && i <= 27 = error "Register is reserved for OS, $k0/$k1 registersXS"
                | i == 28            = GP
                | i == 29            = SP
                | i == 30            = FP
                | i == 31            = RA
                | otherwise          = error "Ran out of registers"

  _ + _  = undefined
  _ * _  = undefined
  _ - _  = undefined
  abs    = undefined
  signum = undefined

instance Show Reg where
  show Zero     = "$zero"
  show (VReg i) = "$v" ++ (show i)
  show (AReg i) = "$a" ++ (show i)
  show (TReg i) = "$t" ++ (show i)
  show GP       = "$gp"
  show SP       = "$sp"
  show FP       = "$fp"
  show RA       = "$ra"

to_reg :: IR.Reg -> Reg
to_reg (IR.Reg i) = fromInteger i :: Reg

-- Instructions

data MIPS = NOOP
          | ADD Reg Reg Reg | ADDI Reg Reg Int | MULT Reg Reg | DIV Reg Reg | SUB Reg Reg Reg
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

data Operand = OpReg Reg | OpInt Int | OpStr String

instance Show Operand where
  show (OpReg r) = show r
  show (OpInt i) = show i
  show (OpStr s) = s

commas :: [Operand] -> String
commas = intercalate ", " . map show

instance Show MIPS where
  show (NOOP)         = "  noop"
  show (ADD r0 r1 r2) = "  add "    ++ (commas [OpReg r0, OpReg r1, OpReg r2])
  show (ADDI r0 r1 i) = "  addi "   ++ (commas [OpReg r0, OpReg r1, OpInt i])
  show (MULT r0 r1)   = "  mult "   ++ (commas [OpReg r0, OpReg r1])
  show (DIV r0 r1)    = "  div "    ++ (commas [OpReg r0, OpReg r1])
  show (SUB r0 r1 r2) = "  sub "    ++ (commas [OpReg r0, OpReg r1, OpReg r2])
  show (AND r0 r1 r2) = "  and "    ++ (commas [OpReg r0, OpReg r1, OpReg r2])
  show (ANDI r0 r1 i) = "  andi "   ++ (commas [OpReg r0, OpReg r1, OpInt i])
  show (OR r0 r1 r2)  = "  or "     ++ (commas [OpReg r0, OpReg r1, OpReg r2])
  show (ORI r0 r1 i)  = "  ori "    ++ (commas [OpReg r0, OpReg r1, OpInt i])
  show (BEQ r0 r1 i)  = "  beq "    ++ (commas [OpReg r0, OpReg r1, OpInt i])
  show (BGEZ r0 i)    = "  bgez "   ++ (commas [OpReg r0, OpInt i])
  show (BGEZAL r0 i)  = "  bgezal " ++ (commas [OpReg r0, OpInt i])
  show (BGTZ r0 i)    = "  bgtz "   ++ (commas [OpReg r0, OpInt i])
  show (BLTZ r0 i)    = "  bltz "   ++ (commas [OpReg r0, OpInt i])
  show (BLTZAL r0 i)  = "  bltzal " ++ (commas [OpReg r0, OpInt i])
  show (BNE r0 r1 i)  = "  bne "    ++ (commas [OpReg r0, OpReg r1, OpInt i])
  show (LW r0 r1 i)   = "  lw "     ++ (commas [OpReg r0, OpReg r1, OpInt i])
  show (LUI r0 i)     = "  lui "    ++ (commas [OpReg r0, OpInt i])
  show (MOVE r0 r1)   = "  move "   ++ (commas [OpReg r0, OpReg r1])
  show (LI r0 i)      = "  li "     ++ (commas [OpReg r0, OpInt i])
  show (LA r0 s)      = "  la "     ++ (commas [OpReg r0, OpStr s])
  show (SB r0 r1 i)   = "  sb "     ++ (show r0) ++ ", " ++ (show i) ++ "(" ++ (show r1) ++ ")"
  show (SW r0 r1 i)   = "  sw "     ++ (show r0) ++ ", " ++ (show i) ++ "(" ++ (show r1) ++ ")"
  show (SLT r0 r1 r2) = "  slt "    ++ (commas [OpReg r0, OpReg r1, OpReg r2])
  show (SLTI r0 r1 i) = "  slti "   ++ (commas [OpReg r0, OpReg r1, OpInt i])
  show (J i)          = "  j "      ++ (show i)
  show (JAL i)        = "  jal "    ++ (show i)
  show (JR r0)        = "  jr "     ++ (show r0)
  show (LB r0 r1 i)   = "  lb "     ++ (show r0)
  show (MFHI r0)      = "  mfhi "   ++ (show r0)
  show (MFLO r0)      = "  mflo "   ++ (show r0)
  show (SYSCALL)      = "  syscall"
  show (LABEL s)      = s ++ ":"

next_reg :: IR.Reg -> IR.Reg
next_reg (IR.Reg r) | r == 0    = IR.Reg 2
                    | r == 1    = error "Unusable reg"
                    | r >= 25   = error "No more general purpose registers"
                    | otherwise = IR.Reg $ succ r

instance ASM MIPS where
  compile vars ir@(inst, _, _) = concat $ map (comp_inst vars) $ relocate_regs NOOP IR.reg0 inst
  relocate_regs _ _ [] = []
  relocate_regs a reg (inst : insts) = let nreg = next_reg reg in
                                         (:) (IR.relocate_instruction (Just reg) Nothing inst) (relocate_regs a nreg insts)

comp_inst :: IR.Vars -> IR.Instruction -> [MIPS]
comp_inst vars (IR.Unary reg (IR.AReg areg))                 = [MOVE (to_reg reg) (to_reg areg)]
comp_inst vars (IR.Unary reg (IR.ANumber an))                = [LI (to_reg reg) an]
comp_inst vars (IR.Unary reg (IR.ALabel lbl))                = [LA (to_reg reg) $ "__st" ++ (show lbl)]
-- Arithmetic
comp_inst vars (IR.Binary reg regl IR.Plus (IR.AReg regr))   = [ADD  (to_reg reg) (to_reg regl) (to_reg regr)]
comp_inst vars (IR.Binary reg regl IR.Minus (IR.AReg regr))  = [SUB  (to_reg reg) (to_reg regl) (to_reg regr)]
comp_inst vars (IR.Binary reg regl IR.Mult (IR.AReg regr))   = [MULT (to_reg regl) (to_reg regr), MFLO (to_reg reg)]
comp_inst vars (IR.Binary reg regl IR.Div (IR.AReg regr))    = [DIV  (to_reg regl) (to_reg regr), MFLO (to_reg reg)]
comp_inst vars (IR.Binary reg regl IR.Rem (IR.AReg regr))    = [DIV  (to_reg regl) (to_reg regr), MFHI (to_reg reg)]
comp_inst vars (IR.Binary reg regl IR.Plus (IR.ANumber n))   = [ADDI (to_reg reg) (to_reg regl) n]
comp_inst vars (IR.MkLabel l)                                = [LABEL (show l)]
-- Memory
comp_inst vars (IR.Store reg (IR.AAddr addr))                = [SW (to_reg reg) Zero (base_address + addr)]
-- Misc
comp_inst vars (IR.Halt)                                     = [LI (VReg 0) 10, SYSCALL]
--
comp_inst vars (IR.Binary reg regl op a)                     = error $ "Unimplemented IR instruction: binary with r = " ++ (show a)
comp_inst _ i                                                = error $ "Unimplemented IR instruction: " ++ (show i)
