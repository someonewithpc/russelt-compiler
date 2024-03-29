
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
                | i >= 1 && i <= 9   = SReg (i - 1)
                | i >= 10 && i <= 14 = AReg (i - 10)
                | i >= 15 && i <= 25 = TReg (i - 15)
                | i >= 26 && i <= 28 = VReg (i - 26)
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
  show (SReg i) = "$s" ++ (show i)
  show GP       = "$gp"
  show SP       = "$sp"
  show FP       = "$fp"
  show RA       = "$ra"

to_reg :: IR.Reg -> Reg
to_reg (IR.Reg i) = fromInteger (i + 1) :: Reg

-- Instructions

data MIPS =
          -- System
            NOOP
          | SYSCALL
          -- Arithmetic/Logic
          | ADD Reg Reg Reg | ADDI Reg Reg Int | MULT Reg Reg | DIV Reg Reg | SUB Reg Reg Reg
          | AND Reg Reg Reg | ANDI Reg Reg Int | OR Reg Reg Reg | ORI Reg Reg Int
          -- Branch and Jump
          | LABEL String
          | BEQ Reg Reg Int | BNE Reg Reg Int | BLT Reg Reg Int | BGT Reg Reg Int
          | BLE Reg Reg Int | BGE Reg Reg Int | BGEZ Reg Int | BGEZAL Reg Int
          | BGTZ Reg Int | BLTZ Reg Int | BLTZAL Reg Int
          | J Int | JALi Int | JALs String | JR Reg
          -- Load/Store/Move
          | LB Reg Reg Int | LW Reg Reg Int | LUI Reg Int
          | MOVE Reg Reg | LI Reg Int | LA Reg String
          | SB Reg Reg Int | SW Reg Reg Int
          | MFHI Reg | MFLO Reg
          -- Comparison
          | SLT Reg Reg Reg | SLTI Reg Reg Int | SEQ Reg Reg Reg | SNE Reg Reg Reg

data Operand = OpReg Reg | OpInt Int | OpStr String | OpLabel Int

instance Show Operand where
  show (OpReg r) = show r
  show (OpInt i) = show i
  show (OpStr s) = s
  show (OpLabel i) = show (IR.LabelI $ fromIntegral i)

commas :: [Operand] -> String
commas = intercalate ", " . map show

instance Show MIPS where
  -- System
  show (NOOP)         = "  noop"
  show (SYSCALL)      = "  syscall"
  -- Arithmetic/Logic
  show (ADD r0 r1 r2) = "  add "    ++ (commas [OpReg r0, OpReg r1, OpReg r2])
  show (ADDI r0 r1 i) = "  addi "   ++ (commas [OpReg r0, OpReg r1, OpInt i])
  show (MULT r0 r1)   = "  mult "   ++ (commas [OpReg r0, OpReg r1])
  show (DIV r0 r1)    = "  div "    ++ (commas [OpReg r0, OpReg r1])
  show (SUB r0 r1 r2) = "  sub "    ++ (commas [OpReg r0, OpReg r1, OpReg r2])
  show (AND r0 r1 r2) = "  and "    ++ (commas [OpReg r0, OpReg r1, OpReg r2])
  show (ANDI r0 r1 i) = "  andi "   ++ (commas [OpReg r0, OpReg r1, OpInt i])
  show (OR r0 r1 r2)  = "  or "     ++ (commas [OpReg r0, OpReg r1, OpReg r2])
  show (ORI r0 r1 i)  = "  ori "    ++ (commas [OpReg r0, OpReg r1, OpInt i])
  -- Branch and Jump
  show (LABEL s)      = s ++ ":"
  show (BEQ r0 r1 i)  = "  beq "    ++ (commas [OpReg r0, OpReg r1, OpLabel i])
  show (BNE r0 r1 i)  = "  bne "    ++ (commas [OpReg r0, OpReg r1, OpLabel i])
  show (BLT r0 r1 i)  = "  blt "    ++ (commas [OpReg r0, OpReg r1, OpLabel i])
  show (BGT r0 r1 i)  = "  bgt "    ++ (commas [OpReg r0, OpReg r1, OpLabel i])
  show (BLE r0 r1 i)  = "  ble "    ++ (commas [OpReg r0, OpReg r1, OpLabel i])
  show (BGE r0 r1 i)  = "  bge "    ++ (commas [OpReg r0, OpReg r1, OpLabel i])
  show (BGEZ r0 i)    = "  bgez "   ++ (commas [OpReg r0, OpLabel i])
  show (BGEZAL r0 i)  = "  bgezal " ++ (commas [OpReg r0, OpLabel i])
  show (BGTZ r0 i)    = "  bgtz "   ++ (commas [OpReg r0, OpLabel i])
  show (BLTZ r0 i)    = "  bltz "   ++ (commas [OpReg r0, OpLabel i])
  show (BLTZAL r0 i)  = "  bltzal " ++ (commas [OpReg r0, OpLabel i])
  show (J i)          = "  j "      ++ (show (IR.LabelI $ fromIntegral i))
  show (JALi i)       = "  jal "    ++ (show (IR.LabelI $ fromIntegral i))
  show (JALs s)       = "  jal "    ++ (show s)
  show (JR r0)        = "  jr "     ++ (show r0)
  -- Load/Store/Move
  show (LB r0 r1 i)   = "  lb "     ++ (show r0)
  show (LW r0 r1 i)   = "  lw "     ++ (commas [OpReg r0, OpReg r1, OpInt i])
  show (LUI r0 i)     = "  lui "    ++ (commas [OpReg r0, OpInt i])
  show (MOVE r0 r1)   = "  move "   ++ (commas [OpReg r0, OpReg r1])
  show (LI r0 i)      = "  li "     ++ (commas [OpReg r0, OpInt i])
  show (LA r0 s)      = "  la "     ++ (commas [OpReg r0, OpStr s])
  show (SB r0 r1 i)   = "  sb "     ++ (show r0) ++ ", " ++ (show i) ++ "(" ++ (show r1) ++ ")"
  show (SW r0 r1 i)   = "  sw "     ++ (show r0) ++ ", " ++ (show i) ++ "(" ++ (show r1) ++ ")"
  show (MFHI r0)      = "  mfhi "   ++ (show r0)
  show (MFLO r0)      = "  mflo "   ++ (show r0)
  -- Comparison
  show (SLT r0 r1 r2) = "  slt "    ++ (commas [OpReg r0, OpReg r1, OpReg r2])
  show (SLTI r0 r1 i) = "  slti "   ++ (commas [OpReg r0, OpReg r1, OpInt i])
  show (SEQ r0 r1 r2) = "  seq "    ++ (commas [OpReg r0, OpReg r1, OpReg r2])
  show (SNE r0 r1 r2) = "  sne "    ++ (commas [OpReg r0, OpReg r1, OpReg r2])

next_reg :: IR.Reg -> IR.Reg
next_reg (IR.Reg r) | r == 0    = IR.Reg 2
                    | r == 1    = IR.Reg 3
                    | r >= 25   = error "No more general purpose registers"
                    | otherwise = IR.Reg $ r + 2

instance ASM MIPS where
  compile vars ir@(inst, _, _) = concat $ map (comp_inst vars) $ relocate_regs NOOP IR.reg0 inst
  relocate_regs _ _ a = a -- map (IR.relocate_instruction (Just IR.reg1) Nothing) a
  -- relocate_regs _ _ [] = []
  -- relocate_regs a reg (inst@(IR.If (IR.AReg regr) _ _ _ _) : insts) = (:) (IR.relocate_instruction (Just reg) Nothing inst) (relocate_regs a IR.reg0 insts)
  -- relocate_regs a reg (inst : insts) = let nreg = next_reg reg in
  --                                        (:) (IR.relocate_instruction (Just reg) Nothing inst) (relocate_regs a nreg insts)

comp_inst :: IR.Vars -> IR.Instruction -> [MIPS]
-- System
comp_inst vars (IR.Halt)                                     = [LI (VReg 0) 10, SYSCALL]
comp_inst vars (IR.PrintLn reg)                              = [MOVE (AReg 0) (to_reg reg), LI (VReg 0) 4, SYSCALL]
-- Arithmetic/Logic
comp_inst vars (IR.Binary reg regl IR.Plus (IR.AReg regr))   = [ADD  (to_reg reg) (to_reg regl) (to_reg regr)]
comp_inst vars (IR.Binary reg regl IR.Minus (IR.AReg regr))  = [SUB  (to_reg reg) (to_reg regl) (to_reg regr)]
comp_inst vars (IR.Binary reg regl IR.Mult (IR.AReg regr))   = [MULT (to_reg regl) (to_reg regr), MFLO (to_reg reg)]
comp_inst vars (IR.Binary reg regl IR.Div (IR.AReg regr))    = [DIV  (to_reg regl) (to_reg regr), MFLO (to_reg reg)]
comp_inst vars (IR.Binary reg regl IR.Rem (IR.AReg regr))    = [DIV  (to_reg regl) (to_reg regr), MFHI (to_reg reg)]
comp_inst vars (IR.Binary reg regl IR.Plus (IR.ANumber n))   = [ADDI (to_reg reg) (to_reg regl) n]

comp_inst vars (IR.Binary reg regl IR.And (IR.AReg regr))   = [AND  (to_reg reg) (to_reg regl) (to_reg regr)]
comp_inst vars (IR.Binary reg regl IR.Or (IR.AReg regr))  = [OR  (to_reg reg) (to_reg regl) (to_reg regr)]
-- Load/Store/Move
comp_inst vars (IR.MkLabel l)                                = [LABEL (show l)]
comp_inst vars (IR.Unary reg (IR.AReg areg))                 = [MOVE (to_reg reg) (to_reg areg)]
comp_inst vars (IR.Unary reg (IR.ANumber an))                = [LI (to_reg reg) an]
comp_inst vars (IR.Unary reg (IR.ALabel lbl))                = [LA (to_reg reg) $ "__st" ++ (show lbl)]
comp_inst vars (IR.Store reg (IR.AAddr addr))                = [SW (to_reg reg) Zero (base_address + addr)]
comp_inst vars (IR.Load reg (IR.ANumber an))                 = [LI (to_reg reg) an]
comp_inst vars (IR.Load reg (IR.AAddr addr))                 = [LA (to_reg reg) (show addr)]
-- Comparison
comp_inst vars (IR.Binary reg regl IR.Equal (IR.AReg regr))  = [SEQ (to_reg reg) (to_reg regl) (to_reg regr)]
comp_inst vars (IR.Binary reg regl IR.Diff (IR.AReg regr))   = [SNE (to_reg reg) (to_reg regl) (to_reg regr)]
-- Branch/Jump
comp_inst vars (IR.Goto (IR.LabelI label))                   = [JALi (fromIntegral label)]
comp_inst vars (IR.Goto (IR.LabelS label))                   = [JALs label]
comp_inst vars (IR.If (IR.AReg n1) IR.Equal (IR.ANumber 0) (IR.LabelI label) Nothing) = [BEQ (to_reg n1) (to_reg IR.reg0) (fromIntegral label)]
comp_inst vars (IR.If (IR.AReg n1) IR.Diff (IR.ANumber 0) (IR.LabelI label) Nothing)  = [BNE (to_reg n1) (to_reg IR.reg0) (fromIntegral label)]
comp_inst vars (IR.If (IR.AReg n1) IR.Equal (IR.AReg n2) (IR.LabelI label) Nothing)   = [BEQ (to_reg n1) (to_reg n2) (fromIntegral label)]
comp_inst vars (IR.If (IR.AReg n1) IR.Diff (IR.AReg n2) (IR.LabelI label) Nothing)    = [BNE (to_reg n1) (to_reg n2) (fromIntegral label)]
comp_inst vars (IR.If (IR.AReg n1) IR.Lt (IR.AReg n2) (IR.LabelI label) Nothing)      = [BLT (to_reg n1) (to_reg n2) (fromIntegral label)]
comp_inst vars (IR.If (IR.AReg n1) IR.Gt (IR.AReg n2) (IR.LabelI label) Nothing)      = [BGT (to_reg n1) (to_reg n2) (fromIntegral label)]
comp_inst vars (IR.If (IR.AReg n1) IR.Le (IR.AReg n2) (IR.LabelI label) Nothing)      = [BLE (to_reg n1) (to_reg n2) (fromIntegral label)]
comp_inst vars (IR.If (IR.AReg n1) IR.Ge (IR.AReg n2) (IR.LabelI label) Nothing)      = [BGE (to_reg n1) (to_reg n2) (fromIntegral label)]
comp_inst vars (IR.If (IR.AReg n1) IR.Equal (IR.ANumber 0) (IR.LabelI label) (Just (IR.LabelI label_else))) = [BEQ (to_reg n1) (to_reg IR.reg0) (fromIntegral label),
                                                                                                               JALi (fromIntegral label_else)]
comp_inst vars (IR.If (IR.AReg n1) IR.Diff (IR.ANumber 0) (IR.LabelI label) (Just (IR.LabelI label_else)))  = [BNE (to_reg n1) (to_reg IR.reg0) (fromIntegral label),
                                                                                                               JALi (fromIntegral label_else)]
comp_inst vars (IR.If (IR.AReg n1) IR.Equal (IR.AReg n2) (IR.LabelI label) (Just (IR.LabelI label_else)))   = [BEQ (to_reg n1) (to_reg n2) (fromIntegral label),
                                                                                                               JALi (fromIntegral label_else)]
comp_inst vars (IR.If (IR.AReg n1) IR.Diff (IR.AReg n2) (IR.LabelI label) (Just (IR.LabelI label_else)))    = [BNE (to_reg n1) (to_reg n2) (fromIntegral label),
                                                                                                               JALi (fromIntegral label_else)]
comp_inst vars (IR.If (IR.AReg n1) IR.Lt (IR.AReg n2) (IR.LabelI label) (Just (IR.LabelI label_else)))      = [BLT (to_reg n1) (to_reg n2) (fromIntegral label),
                                                                                                               JALi (fromIntegral label_else)]
comp_inst vars (IR.If (IR.AReg n1) IR.Gt (IR.AReg n2) (IR.LabelI label) (Just (IR.LabelI label_else)))      = [BGT (to_reg n1) (to_reg n2) (fromIntegral label),
                                                                                                               JALi (fromIntegral label_else)]
comp_inst vars (IR.If (IR.AReg n1) IR.Le (IR.AReg n2) (IR.LabelI label) (Just (IR.LabelI label_else)))      = [BLE (to_reg n1) (to_reg n2) (fromIntegral label),
                                                                                                               JALi (fromIntegral label_else)]
comp_inst vars (IR.If (IR.AReg n1) IR.Ge (IR.AReg n2) (IR.LabelI label) (Just (IR.LabelI label_else)))      = [BGE (to_reg n1) (to_reg n2) (fromIntegral label),
                                                                                                               JALi (fromIntegral label_else)]
--
comp_inst vars (IR.Binary reg regl op a)                     = error $ "Unimplemented IR instruction: binary with r = " ++ (show a)
comp_inst _ i                                                = error $ "Unimplemented IR instruction: " ++ (show i)
