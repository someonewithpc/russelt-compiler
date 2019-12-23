
module Compiler where

import Parser
import qualified IR

class ASM a where
  compile :: IR.Vars -> IR.Blk -> [a]
  relocate_regs :: a -> IR.Reg -> [IR.Instruction] -> [IR.Instruction]
